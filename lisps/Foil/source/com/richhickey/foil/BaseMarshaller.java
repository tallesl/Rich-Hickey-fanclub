/*
 * Created on Dec 10, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
package com.richhickey.foil;

import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.lang.reflect.*;
/**
 * @author Rich
 *
 */
public class BaseMarshaller implements IBaseMarshaller
    {
    IReferenceManager referenceManager;
    Map marshallerMap;
    Map marshallerCache;
    
    public BaseMarshaller(IReferenceManager referenceManager)
        {
        this.referenceManager = referenceManager;
        this.marshallerMap = new LinkedHashMap();
        this.marshallerCache = new HashMap();
        }

    public void registerMarshaller(Class target,IMarshaller marshaller)
        {
        marshallerMap.put(target,marshaller);
        }
    
    public IMarshaller findMarshallerFor(Class c)
        {
        //already seen this type?
        if(marshallerCache.containsKey(c))
            return (IMarshaller)marshallerCache.get(c);
        //see if exact match has been registered
        IMarshaller m = (IMarshaller)marshallerMap.get(c);
        if(m == null)
            {
            Class bestbase = null;
            //see if a base has been registered
            Iterator i = marshallerMap.entrySet().iterator();
            while(i.hasNext())
                {
                Map.Entry e = (Map.Entry)i.next();
                Class trybase = (Class)e.getKey();
                if(trybase.isAssignableFrom(c)
                        && isMoreSpecific(trybase,bestbase))
                    {
                    bestbase = trybase;
                    m = (IMarshaller)e.getValue();
                    }
                }
            }
        //remember the result
        marshallerCache.put(c,m);
        return m;
        }
    
    boolean isMoreSpecific(Class c1,Class c2)
        {
        if(c2 == null)
            return true;
		return(c2.isAssignableFrom(c1) && !c1.isAssignableFrom(c2));
        }
    
    boolean isPrimitive(Class c)
        {
        return c == Integer.class
        	|| c == Long.class
        	|| c == Double.class
        	|| c == Float.class
        	|| c == Boolean.class
        	|| c == Character.class
        	|| c == Short.class
        	|| c == Byte.class;
        }
    
    /* (non-Javadoc)
     * @see com.richhickey.foil.IBaseMarshaller#marshallAtom(java.lang.Object, java.io.Writer, int, int)
     */
    public void marshallAtom(Object o, Writer w, int flags, int depth) throws IOException
        {
        w.write(' ');
        if(o == null)
            {
            w.write("nil");
            return;
            }
        
        Class c = o.getClass();
        if(isPrimitive(c))
            {
            if(o instanceof Boolean)
                {
                boolean b = ((Boolean)o).booleanValue();
                if(b)
                    w.write('t');
                else
                    w.write("nil");
                }
            else if(o instanceof Character)
                {
                w.write("#\\");
                w.write(((Character)o).charValue());
                }
            else
                w.write(o.toString());
            }
        else if(o instanceof String)
            {
            w.write('"');
            //TODO make this more efficient
    		w.write(((String)o).replaceAll("\\\\","\\\\\\\\").replaceAll("\"","\\\\\""));
            w.write('"');
            }
        else if(o instanceof Class && ((Class)o).isPrimitive())
            {
            if(o == int.class)
                w.write(":int");
            else if(o == long.class)
                w.write(":long");
            else if(o == double.class)
                w.write(":double");
            else if(o == float.class)
                w.write(":float");
            else if(o == boolean.class)
                w.write(":boolean");
            else if(o == char.class)
                w.write(":char");
            else if(o == void.class)
                w.write(":void");
            else if(o == short.class)
                w.write(":short");
            else if(o == byte.class)
                w.write(":byte");
            }
        else //write a reference type
            if((flags & MARSHALL_ID) != 0) //as a reference
            {
            w.write("#{:ref ");
            ObjectID oid = referenceManager.getIdForObject(o);
            w.write(Integer.toString(oid.id));
            w.write(' ');
            w.write(Integer.toString(oid.rev));
            
            if((flags & MARSHALL_HASH) != 0)
                {
                w.write(" :hash ");
                int hash = o.hashCode();
                w.write(Integer.toString(hash));
                }

            if((flags & MARSHALL_TYPE) != 0)
                {
                w.write(" :type ");
                marshallAtom(c,w,MARSHALL_ID,1);
                }

            if(depth > 0)
                {
                IMarshaller m = findMarshallerFor(c);
                if(m != null)
                    {
	                w.write(" :val");
	                m.marshall(o,w,this,flags,depth - 1);
                    }
                }
            
            w.write('}');
            }
        else if(depth > 0)	//effectively, MARSHALL_NO_REFS, write just the value of a reference type, since id was not requested
            {
            IMarshaller m = findMarshallerFor(c);
            if(m != null)
                {
                m.marshall(o,w,this,flags,depth - 1);
                }
            else
                {
                w.write("nil");
                }
            }
        else
        	{
        	w.write("nil");
        	}

        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IBaseMarshaller#marshallAsList(java.lang.Object, java.io.Writer, int, int)
     */
    public void marshallAsList(Object o, Writer w, int flags, int depth) throws IOException
    	{
        w.write(' ');
        doMarshallAsList(o,w,flags,depth);
    	}
    public void doMarshallAsList(Object o, Writer w, int flags, int depth) throws IOException
        {
        w.write('(');
        Iterator i = null;
        if(o instanceof Collection)
            i = ((Collection)o).iterator();
        else if(o instanceof Iterator)
            i = (Iterator)o;
        else if(o instanceof Object[])
            i = Arrays.asList((Object[])o).iterator();
        
        if(i != null)
            {
            while(i.hasNext())
                {
                marshallAtom(i.next(),w,flags,depth);
                }
            }
        else if(o instanceof Enumeration)
            {
            for(Enumeration e = (Enumeration)o;e.hasMoreElements();)
                {
                marshallAtom(e.nextElement(),w,flags,depth);
                }
            }
        else if(o.getClass().isArray())
            {
            int len = Array.getLength(o);
            for(int idx = 0;idx < len;idx++)
                {
                marshallAtom(Array.get(o,idx),w,flags,depth);
                }
            }
        w.write(')');
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IBaseMarshaller#marshallAsVector(java.lang.Object, java.io.Writer, int, int)
     */
    public void marshallAsVector(Object o, Writer w, int flags, int depth) throws IOException
        {
        w.write(" #");
        doMarshallAsList(o,w,flags,depth);
        }

	public boolean canMarshallAsList(Object o)
	    {
	    return 
	    	o instanceof Collection
	    	|| o instanceof Iterator
	    	|| o instanceof Enumeration
	    	|| o.getClass().isArray();
	    }

    }

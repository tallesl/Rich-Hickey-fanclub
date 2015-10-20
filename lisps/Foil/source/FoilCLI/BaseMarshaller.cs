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

using	System;
using	System.IO;
using	System.Text;
using	System.Collections;
using	System.Reflection;
/**
 * @author Eric Thorsen
 *
 */
namespace com.richhickey.foil 
{
	public class BaseMarshaller : IBaseMarshaller
	{
		IReferenceManager	referenceManager;
		Hashtable			marshallerMap;
		Hashtable			marshallerCache;
    
		public BaseMarshaller(IReferenceManager referenceManager)
		{
			this.referenceManager	= referenceManager;
			this.marshallerMap		= new Hashtable();
			this.marshallerCache	= new Hashtable();
		}

		public void registerMarshaller(Type	target,IMarshaller marshaller)
		{
			marshallerMap.Add(target,marshaller);
		}
    
		public	IMarshaller findMarshallerFor(System.Type	c)
		{
			//already seen this type?
			if(marshallerCache.ContainsKey(c))
				return (IMarshaller)marshallerCache[c];
			//see if exact match has been registered
			IMarshaller m = (IMarshaller)marshallerMap[c];
			if(m == null)
			{
				Type	bestbase	=	null;
				//see if a base has been registered
				foreach(DictionaryEntry e in marshallerMap)
				{
					Type	trybase = (Type)e.Key;
					if(trybase.IsAssignableFrom(c)
						&& isMoreSpecific(trybase,bestbase))
					{
						bestbase = trybase;
						m = (IMarshaller)e.Value;
					}
				}
			}
			//remember the result
			marshallerCache.Add(c,m);
			return m;
		}
    
		bool isMoreSpecific(Type	c1,Type	c2)
		{
			if(c2 == null)
				return true;
			return(c2.IsAssignableFrom(c1) && !c1.IsAssignableFrom(c2));
		}
    
		/* (non-Javadoc)
		 * @see com.richhickey.foil.IBaseMarshaller#marshallAtom(java.lang.Object, java.io.Writer, int, int)
		 */
		public void marshallAtom(Object o, TextWriter w, int flags, int depth) 
		{
			w.Write(' ');
			if(o == null)
			{
				w.Write("nil");
				return;
			}
        
			Type	c = o.GetType();
			if(c.IsPrimitive)
			{
				if(o is Boolean)
				{
					Boolean b = (Boolean)o;
					if(b)
						w.Write('t');
					else
						w.Write("nil");
				}
				else if(o is System.Char)
				{
					w.Write("#\\");
					w.Write("{0}",Convert.ToInt32((Char)o));
				}
				else
					w.Write(o.ToString());
			}
			else if(o is String)
			{
				w.Write('"');
				w.Write(((String)o).Replace("\\\\","\\\\\\\\").Replace("\"","\\\\\""));
				w.Write('"');
			}
			else //write a reference
			{
				if((flags & IBaseMarshallerFlags.MARSHALL_ID) != 0)
				{
					w.Write("#{:ref ");

					ObjectID oid = referenceManager.getIdForObject(o);
					//++oid.rev;
					w.Write("{0} {1}",oid.id,oid.rev);
            
					if((flags & IBaseMarshallerFlags.MARSHALL_HASH) != 0)
					{
						w.Write(" :hash {0}",o.GetHashCode());
					}

					if((flags & IBaseMarshallerFlags.MARSHALL_TYPE) != 0)
					{
						w.Write(" :type ");
						marshallAtom(c,w,IBaseMarshallerFlags.MARSHALL_ID,1);
					}

					if(depth > 0)
					{
						IMarshaller m = findMarshallerFor(c);
						if(m != null)
						{
							w.Write(" :val");
							m.marshall(o,w,this,flags,depth - 1);
						}
					}
            
					w.Write('}');
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
						w.Write("nil");
					}
				}
				else
				{
					w.Write("nil");
				}
			}
		}

		/* (non-Javadoc)
		 * @see com.richhickey.foil.IBaseMarshaller#marshallAsList(java.lang.Object, java.io.Writer, int, int)
		 */
		public void doMarshallAsList(Object o, TextWriter w, int flags, int depth) 
		{
			w.Write('(');
			IEnumerator	i = null;
			if(o is IEnumerator)
				i = (IEnumerator)o;
			else if(o is IEnumerable)
				i = ((IEnumerable)o).GetEnumerator();
        
			if(i != null)
			{
				while(i.MoveNext())
					marshallAtom(i.Current,w,flags,depth);
			}
			w.Write(')');
		}

		public void marshallAsList(Object o, TextWriter w, int flags, int depth) 
		{
			w.Write(' ');
			doMarshallAsList(o,w,flags,depth);
		}

		/* (non-Javadoc)
		 * @see com.richhickey.foil.IBaseMarshaller#marshallAsVector(java.lang.Object, java.io.Writer, int, int)
		 */
		public void marshallAsVector(Object o, TextWriter w, int flags, int depth) 
		{
			w.Write('#');
			doMarshallAsList(o,w,flags,depth);
		}

		public Boolean canMarshallAsList(Object o)
		{
			return o is IEnumerable;
		}
	}
}
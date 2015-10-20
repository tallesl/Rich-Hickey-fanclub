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
import java.beans.*;
import java.lang.reflect.*;
/**
 * @author Rich
 *
 */
public class UniversalMarshaller implements IMarshaller
    {

    /* (non-Javadoc)
     * @see com.richhickey.foil.IMarshaller#marshall(java.lang.Object, java.io.Writer, com.richhickey.foil.IBaseMarshaller, int, int)
     */
    public void marshall(Object o, Writer w, IBaseMarshaller baseMarshaller,
            int flags, int depth)  throws IOException
        {
        if(o.getClass().isArray())
            {
            baseMarshaller.marshallAsVector(o,w,flags,depth);
            }
        else if(baseMarshaller.canMarshallAsList(o))
            {
            baseMarshaller.marshallAsList(o,w,flags,depth);
            }
        else if(o instanceof Class)
            baseMarshaller.marshallAtom(((Class)o).getName(),w,flags,depth);
        else	
            //use beaninfo to dump properties as assoc list
            {
            try{
                w.write(" (");
                PropertyDescriptor[] props = Introspector.getBeanInfo(o.getClass()).getPropertyDescriptors();
                for(int i=0;i<props.length;i++)
                    {
                    PropertyDescriptor prop = props[i];
                    //skip getClass generated property
                    if(prop.getName().equals("class"))
                        continue;
                    Method m = props[i].getReadMethod();
                    //must be no-arg property getter
                    if(m != null && m.getParameterTypes().length == 0)
                        {
                        w.write("(:"); //sent as keyword
                        w.write(prop.getName());
                        w.write(" . ");
                        baseMarshaller.marshallAtom(m.invoke(o,null),w,flags,depth);
                        w.write(')');
                        }
                    }
                w.write(')');
            	}
            catch(Exception ex)
            	{
                throw new IOException(ex.toString());
            	}
            }
        }

    }

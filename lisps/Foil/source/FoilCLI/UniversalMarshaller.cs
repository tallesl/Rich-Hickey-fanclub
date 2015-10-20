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
using System;
using System.IO;
using System.Reflection;

/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil	
{
	public class UniversalMarshaller :IMarshaller
	{
		public void marshall(Object o, TextWriter w, IBaseMarshaller baseMarshaller,int flags, int depth)  
		{
			if(o.GetType().IsArray)
				baseMarshaller.marshallAsVector(o,w,flags,depth);
			else if(baseMarshaller.canMarshallAsList(o))
				baseMarshaller.marshallAsVector(o,w,flags,depth);
			else if(o is Type)
				baseMarshaller.marshallAtom(o.ToString(),w,flags,depth);
			else
			{
				try
				{
					w.Write(" (");
					PropertyInfo[] props = o.GetType().GetProperties();
					for(int i=0;i<props.Length;i++)
					{
						PropertyInfo prop = props[i];
						//skip getClass generated property
						if(prop.Name.Equals("object"))
							continue;
						MethodInfo m = props[i].GetGetMethod();
						//must be no-arg property getter
						if(m != null && m.GetParameters().Length == 0)
						{
							w.Write("(:"); //sent as keyword
							w.Write(prop.Name);
							w.Write(" . ");
							baseMarshaller.marshallAtom(m.Invoke(o,null),w,flags,depth);
							w.Write(')');
						}
					}
					w.Write(')');
				}
				catch(Exception ex)
				{
					throw new IOException(ex.ToString());
				}
			}
		}
	}
}

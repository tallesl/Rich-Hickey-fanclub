/*
 * Created on Dec 7, 2004
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
using System.Collections;

/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil 
{
	public class ReferenceManager : IReferenceManager
	{
		Hashtable idToObj; //int->Object
		Hashtable objToId; //Object->ObjectId
		int nextId = 1;
    
		public ReferenceManager()
		{
			idToObj = new Hashtable();
			objToId = new Hashtable();
		}

		public ObjectID getIdForObject(Object o)
		{
			lock(this)
			{
				ObjectID oid = findIdForObject(o);
				oid.rev++;
				return oid;
			}
		}

		private ObjectID findIdForObject(Object o)
		{
			ObjectID oid = (ObjectID)objToId[o];
			if(oid == null)
			{
				oid = new ObjectID(nextId++);
				idToObj.Add(oid.id,o);
				objToId.Add(o,oid);
			}
			return oid;
		}

		public Object getObjectForId(Object id) 
		{
			lock(this)
			{
				Object o = idToObj[id];
				if(o == null)
					throw new Exception("Invalid reference id");
				return o;
			}
		}
	
		public void free(Object id,int rev) 
		{
			lock(this)
			{
				Object o		= getObjectForId(id);
				ObjectID oid	= getIdForObject(o);
				if(oid.rev == rev)
				{
					objToId.Remove(o);
					idToObj.Remove(id);
				}
			}
		}
	}
}

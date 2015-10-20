/*
 * Created on Dec 8, 2004
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
import java.util.*;
/**
 * @author Rich
 *
 */
public class ReferenceManager implements IReferenceManager
    {
    Hashtable idToObj; //int->Object
    IdentityHashMap objToId; //Object->ObjectId
    int nextId = 1;
    
    public ReferenceManager()
        {
        idToObj = new Hashtable();
        objToId = new IdentityHashMap();
        }
    /* (non-Javadoc)
     * @see com.richhickey.foil.IReferenceManager#getIdForObject(java.lang.Object)
     */
    public ObjectID getIdForObject(Object o)
    	{
    	synchronized(this)
			{
	    	ObjectID oid = findIdForObject(o);
	    	oid.rev++;
	    	return oid;
			}
    	}

    private ObjectID findIdForObject(Object o)
        {
        ObjectID oid = (ObjectID)objToId.get(o);
        if(oid == null)
            {
            oid = new ObjectID(nextId++);
            idToObj.put(new Integer(oid.id),o);
            objToId.put(o,oid);
            }
        return oid;
        }

	public Object getObjectForId(Object id) throws Exception
	    {
    	synchronized(this)
			{
		    Object o = idToObj.get(id);
		    if(o == null)
		        throw new Exception("Invalid reference id");
		    return o;
		    }
	    }
    /* (non-Javadoc)
     * @see com.richhickey.foil.IReferenceManager#free(int)
     */
    public void free(Object id,int rev) throws Exception
        {
    	synchronized(this)
			{
	        Object o = getObjectForId(id);
	        ObjectID oid = findIdForObject(o);
	        if(oid.rev == rev)
	        	{
	        	objToId.remove(o);
	        	idToObj.remove(id);
	        	}
	        }
        }
    }

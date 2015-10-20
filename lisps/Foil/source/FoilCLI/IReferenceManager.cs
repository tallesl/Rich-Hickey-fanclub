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

/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReferenceManager.
	/// </summary>
	public interface IReferenceManager
	{
		ObjectID getIdForObject(Object o);
		Object getObjectForId(Object id);
		void free(Object id,int rev);
	}
}

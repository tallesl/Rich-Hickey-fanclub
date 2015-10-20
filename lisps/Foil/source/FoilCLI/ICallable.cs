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
	public	class	ICallableFlags
	{
		public const int METHOD = 0;
		public const int FIELD = 1;
		//public const int FIELD_SET = 2;
		public const int PROPERTY_GET = 3;
		public const int PROPERTY_SET = 4;
		//public const int STATIC_METHOD = 5;
		//public const int STATIC_FIELD_GET = 6;
		//public const int STATIC_FIELD_SET = 7;
		//public const int STATIC_PROPERTY_GET = 8;
		//public const int STATIC_PROPERTY_SET = 9;
	}
	/// <summary>
	/// Summary description for ICallable.
	/// </summary>
	public interface ICallable 
	{
		Object invoke(Object o,ArrayList args);
	}
}

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
using	System;
using	System.IO;
using	System.Collections;
using	System.Reflection;

/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil	
{
	public interface IReflector
	{
		ICallable	getCallable(int memberType, Type	c, String memberName);
		Object		createNew(Type c, ArrayList args);
		void		members(Type c,TextWriter w);
		ArrayList	bases(Type c);
		Object		createVector(Type c,int length,ArrayList inits);
		Object		vectorGet(Object v,int index) ;
		void		vectorSet(Object v,int index,Object val);
		Object		indexerGet(Object v,ArrayList indexes) ;
		void		indexerSet(Object v,ArrayList indexesAndVal);
		Object		vectorLength(Object v);
		void		setProps(Object o, ArrayList nameValuePairs);
		ArrayList	getClassNames(Object assembly,ArrayList assemblyNames);
		Object		makeProxy(IRuntimeServer runtime,int marshallFlags,int marshallDepth,ArrayList interfaces);
 	}
}
/*
 * Created on Jan 11, 2005
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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;


/**
 * @author Rich
 */

public class ProxyHandler implements InvocationHandler {

	IRuntimeServer runtime;
	int marshallFlags;
	int marshallDepth;
	
	public ProxyHandler(IRuntimeServer runtime,int marshallFlags,int marshallDepth)
		{
		this.runtime = runtime;
		this.marshallFlags = marshallFlags;
		this.marshallDepth = marshallDepth;
		}
	/* (non-Javadoc)
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {
		return runtime.proxyCall(marshallFlags,marshallDepth,method,proxy,args);
	}

}

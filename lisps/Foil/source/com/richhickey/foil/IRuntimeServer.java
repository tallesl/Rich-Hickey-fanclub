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
package com.richhickey.foil;
import java.io.*;
import java.lang.reflect.*;
/**
 * @author Rich
 *
 */
public interface IRuntimeServer {
    public Object processMessages(Reader ins,Writer outs) throws IOException;
    public Object proxyCall(int marshallFlags,int marshallDepth,Method method, Object proxy,Object[] args) throws Exception;
}

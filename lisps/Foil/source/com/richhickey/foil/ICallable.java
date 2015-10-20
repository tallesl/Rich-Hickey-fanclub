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
import java.lang.reflect.*;
import java.util.List;
/**
 * @author Rich
 *
 */
public interface ICallable {
    
    public final static int METHOD = 0;
    public final static int FIELD = 1;
//    public final static int FIELD_SET = 2;
    public final static int PROPERTY_GET = 3;
    public final static int PROPERTY_SET = 4;
//    public final static int STATIC_METHOD = 5;
//    public final static int STATIC_FIELD_GET = 6;
//    public final static int STATIC_FIELD_SET = 7;
//    public final static int STATIC_PROPERTY_GET = 8;
//    public final static int STATIC_PROPERTY_SET = 9;

    
	Object invoke(Object target,List args) throws InvocationTargetException;

}

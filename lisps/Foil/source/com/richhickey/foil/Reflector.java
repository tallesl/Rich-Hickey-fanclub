/*
 * Created on Dec 11, 2004
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
import java.lang.reflect.*;
import java.util.*;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.beans.*;
/**
 * @author Rich
 *
 */
public class Reflector implements IReflector
    {
    IBaseMarshaller baseMarshaller;
    public Reflector(IBaseMarshaller baseMarshaller)
        {
        this.baseMarshaller = baseMarshaller;
        }
    /**
     * @author Rich
     *
     */
    public class CallableField implements ICallable
        {
        Field field;
        public CallableField(Field field)
            {
            this.field = field;
            }
        
        /* (non-Javadoc)
         * @see com.richhickey.foil.ICallable#invoke(java.lang.Object, java.util.List)
         */
        public Object invoke(Object target, List args)
                throws InvocationTargetException
            {
            try{
                if(args.size() == 0) //get
                    {
                    return field.get(target);
                    }
                else if(args.size() == 1) //set
                    {
                    field.set(target,boxArg(field.getType(),args.get(0)));
                    return null;
                    }
                else 
                    throw new Exception("invalid number of args passed to field");
            	}
            catch(Exception ex)
            	{	
            	throw new InvocationTargetException(ex);
            	}
            }

        }
    
    public class CallableMethod implements ICallable
        {
        List methods;
        public CallableMethod(List methods)
            {
        	this.methods = methods;
            }
        
        public Object invoke(Object target, List args) throws InvocationTargetException
            {
//            Object target = args.get(0);
//            args = args.subList(1,args.size());
            for(Iterator i = methods.iterator();i.hasNext();)
                {
                Method m = (Method)i.next();
                
                Class[] params = m.getParameterTypes();
                if(isCongruent(params,args))
                    {
                    Object[] boxedArgs = boxArgs(params,args);
                    try{
                        return m.invoke(target,boxedArgs);
                    	}
                    catch(Exception ex)
                    	{	
                    	throw new InvocationTargetException(ex);
                    	}
                    }
                }
            throw new InvocationTargetException(new Exception("no matching function found"));
            }
        }

    static Object[] boxArgs(Class[] params,List args)
        {
        if(params.length == 0)
            return null;
        Object[] ret = new Object[params.length];
        for(int i=0;i<params.length;i++)
            {
            Object arg = args.get(i);
            Class paramType = params[i]; 
            ret[i] = boxArg(paramType,arg);
            }
        return ret;
        }
    
    static Object boxArg(Class paramType,Object arg)
        {
        if(paramType == boolean.class && arg == null)
                return Boolean.FALSE;
        else
            return arg;
        }
    
    static boolean isCongruent(Class[] params,List args)
        {
        boolean ret = false;
        if(args == null)
            return params.length == 0;
        if(params.length == args.size())
            {
            ret = true;
            for(int i=0;ret && i<params.length;i++)
                {
                Object arg = args.get(i);
                Class argType = (arg == null)?null:arg.getClass();
                Class paramType = params[i]; 
                if(paramType == boolean.class)
                    {
                    ret = arg == null || argType == Boolean.class;
                    }
                else if(paramType.isPrimitive())
                    {
                    if(arg == null)
                        ret = false;
                    else if(paramType == int.class)
                        ret = argType == Integer.class;
                    else if(paramType == double.class)
                        ret = argType == Double.class;
                    else if(paramType == long.class)
                        ret = argType == Long.class;
                    else if(paramType == char.class)
                        ret = argType == Character.class;
                    else if(paramType == short.class)
                        ret = argType == Short.class;
                    else if(paramType == byte.class)
                        ret = argType == Byte.class;
                    }
                else
                    {
                    ret = arg == null 
                    	|| argType == paramType 
                    	|| paramType.isAssignableFrom(argType);
                    }
                }
            }
        return ret;
        }
    
    public ICallable getCallable(int memberType, Class c, String memberName)
            throws Exception
        {
        switch(memberType)
        	{
        	case ICallable.METHOD:
        	    return getMethod(c,memberName);
        	case ICallable.FIELD:
        	    return getField(c,memberName);
        	case ICallable.PROPERTY_GET:
        	    return getPropertyGetter(c,memberName);
        	case ICallable.PROPERTY_SET:
        	    return getPropertySetter(c,memberName);
        	default:
        	    throw new Exception("unsupported member type");
        	}
        }
    
    
    ICallable getMethod(Class c,String method)
    	throws Exception
        {
        Method[] allmethods = c.getMethods();
        ArrayList methods = new ArrayList();
        for(int i=0;i<allmethods.length;i++)
            {
            if(method.equals(allmethods[i].getName()))
                methods.add(allmethods[i]);
            }
        if(methods.size() == 0)
            throw new Exception("no methods found");
        return new CallableMethod(methods);
        }

    ICallable getField(Class c,String field)
	throws Exception
	    {
	    Field[] allfields = c.getDeclaredFields();
	    for(int i=0;i<allfields.length;i++)
	        {
	        if(field.equals(allfields[i].getName()))
			    return new CallableField(allfields[i]);
	        }
        throw new Exception("no field found");
	    }

    ICallable getPropertyGetter(Class c,String property)
	throws Exception
        {
        PropertyDescriptor[] props = Introspector.getBeanInfo(c).getPropertyDescriptors();
        ArrayList methods = new ArrayList();
        for(int i=0;i<props.length;i++)
            {
            if(property.equals(props[i].getName()))
                methods.add(props[i].getReadMethod());
            }
        if(methods.size() == 0)
            throw new Exception("no properties found");
        return new CallableMethod(methods);
        }

    ICallable getPropertySetter(Class c,String property)
	throws Exception
        {
        PropertyDescriptor[] props = Introspector.getBeanInfo(c).getPropertyDescriptors();
        ArrayList methods = new ArrayList();
        for(int i=0;i<props.length;i++)
            {
            if(property.equals(props[i].getName()))
                methods.add(props[i].getWriteMethod());
            }
        if(methods.size() == 0)
            throw new Exception("no properties found");
        return new CallableMethod(methods);
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#createNew(java.lang.Class, java.util.List)
     */
    public Object createNew(Class c, List args) throws Exception
        {
        Constructor[] ctors = c.getConstructors();
        for(int i=0;i<ctors.length;i++)
            {
            Constructor ctor = ctors[i];
            Class[] params = ctor.getParameterTypes();
            if(isCongruent(params,args))
                {
                Object[] boxedArgs = boxArgs(params,args);
                try{
                    return ctor.newInstance(boxedArgs);
                	}
                catch(Exception ex)
                	{	
                	throw new InvocationTargetException(ex);
                	}
                }
            }
        throw new InvocationTargetException(new Exception("no matching ctor found"));
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#members(java.lang.Class, java.io.Writer)
     */
    public void members(Class c, Writer w) throws Exception
        {
        w.write(" (");
        
        Constructor[] ctors = c.getConstructors(); 
        if(ctors.length > 0)
            {
            w.write("(:ctors ");
            for(int i=0;i<ctors.length;i++)
                {
                Constructor ctor = ctors[i];
                baseMarshaller.marshallAtom(ctor.toString(),w,0,0);
                //Class[] params = ctor.getParameterTypes();
                //reflectParams(params,w);
                }
            w.write(')');
            }
        
        Method[] methods = c.getMethods();
        if(methods.length > 0)
            {
            w.write("(:methods ");
            for(int i=0;i<methods.length;i++)
                {
                Method method = methods[i];
                w.write('(');
                
//                w.write("(:mref ");
//                baseMarshaller.marshallAtom(method,w,IBaseMarshaller.MARSHALL_ID,0);
//                w.write(')');
                
                w.write("(:name ");
                baseMarshaller.marshallAtom(method.getName(),w,IBaseMarshaller.MARSHALL_ID,0);
                w.write(')');

                //if(Modifier.isStatic(method.getModifiers()))
                    {
                    w.write("(:static ");
                    baseMarshaller.marshallAtom(Modifier.isStatic(method.getModifiers())?Boolean.TRUE:Boolean.FALSE,
                        					w,IBaseMarshaller.MARSHALL_ID,0);
                    w.write(')');
                    }
//                reflectMethodSignature(method,w);
                    w.write("(:doc ");
                    baseMarshaller.marshallAtom(method.toString(),w,IBaseMarshaller.MARSHALL_ID,0);
                    w.write(')');
               
                w.write(')');
                }
            w.write(')');
            }

        Field[] fields = c.getFields();
        if(fields.length > 0)
            {
            w.write("(:fields ");
            for(int i=0;i<fields.length;i++)
                {
                Field field = fields[i];
                w.write('(');
                
                w.write("(:name ");
                baseMarshaller.marshallAtom(field.getName(),w,IBaseMarshaller.MARSHALL_ID,0);
                w.write(')');

                //if(Modifier.isStatic(field.getModifiers()))
                    {
	                w.write("(:static ");
	                baseMarshaller.marshallAtom(Modifier.isStatic(field.getModifiers())?Boolean.TRUE:Boolean.FALSE,
	                        					w,IBaseMarshaller.MARSHALL_ID,0);
	                w.write(')');
                    }

                w.write("(:doc ");
                baseMarshaller.marshallAtom(field.toString(),w,IBaseMarshaller.MARSHALL_ID,1);
                w.write(')');

                w.write(')');
                }
            w.write(')');
            }
        
        PropertyDescriptor[] props = Introspector.getBeanInfo(c).getPropertyDescriptors();
        if(props.length > 0)
            {
            w.write("(:properties ");
            for(int i=0;i<props.length;i++)
                {
                PropertyDescriptor prop = props[i];
                w.write('(');

                w.write("(:name ");
                baseMarshaller.marshallAtom(prop.getName(),w,IBaseMarshaller.MARSHALL_ID,0);
                w.write(')');

                //only create this section if static
                //never true for Java
                w.write("(:static ");
                baseMarshaller.marshallAtom(Boolean.FALSE,
                        					w,IBaseMarshaller.MARSHALL_ID,0);
                w.write(')');

                Method readm = prop.getReadMethod();
                if(readm != null)
                    {
                    w.write("(:get-doc ");
                    baseMarshaller.marshallAtom(readm.toString(),w,IBaseMarshaller.MARSHALL_ID,0);
                    w.write(')');
                    }
                Method setm = prop.getWriteMethod();
                if(setm != null)
                    {
                    w.write("(:set-doc ");
                    baseMarshaller.marshallAtom(setm.toString(),w,IBaseMarshaller.MARSHALL_ID,0);
                    w.write(')');
                    }
                w.write(')');
                }

            w.write(')');
            }

        w.write(')');
        }
    
    void reflectMethodSignature(Method method, Writer w)
    	throws IOException
        {
        Class[] params = method.getParameterTypes();
        reflectParams(params,w);
        w.write("(:ret");
        baseMarshaller.marshallAtom(method.getReturnType(),w,IBaseMarshaller.MARSHALL_ID,1);
        w.write(')');
        }
    

	void reflectParams(Class[] params, Writer w)
	throws IOException
		{
	    w.write("(:args ");
	    for(int p=0;p<params.length;p++)
	        {
	        w.write('(');
	
	        //we don't have param names in Java (maybe in 5?), but will in .Net
	//        w.write("(:name");
	//        ...
	//        w.write(')');
	        
	        
	        w.write("(:type");
	        baseMarshaller.marshallAtom(params[p],w,IBaseMarshaller.MARSHALL_ID,1);
	        w.write(')');
	        
	        w.write(')');
	        }
	    w.write(')');
		}

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#createVector(java.lang.Class, int, java.util.List)
     */
    public Object createVector(Class c, int length, List inits) throws Exception
        {
        Object ret = Array.newInstance(c,length);
        boolean isNumeric = isNumericType(c);
        for(int i=0;i<inits.size();i++)
            {
            if(isNumeric)
                Array.set(ret,i,numericConvert(c,inits.get(i)));
            else
                Array.set(ret,i,inits.get(i));
            }
        return ret;
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorGet(java.lang.Object, int)
     */
    public Object vectorGet(Object v, int index) throws Exception
        {
        return Array.get(v,index);
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorSet(java.lang.Object, int, java.lang.Object)
     */
    public void vectorSet(Object v, int index, Object val) throws Exception
        {
        Array.set(v,index,val);
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorLength(java.lang.Object)
     */
    public Object vectorLength(Object v) throws Exception
        {
        return new Integer(Array.getLength(v));
        }

    static boolean isNumericType(Class c)
        {
        return
        c == int.class
||        c == double.class
||        c == long.class
||        c == float.class
||        c == short.class
||        c == byte.class;
        }
	static Object numericConvert(Class targetType,Object num)
	throws Exception
	    {
        Number n = (Number)num;
        if(targetType == int.class)
            {
            return new Integer(n.intValue());
            }
        else if(targetType == double.class)
            {
            return new Double(n.doubleValue());
            }
        else if(targetType == long.class)
            {
            return new Long(n.longValue());
            }
        else if(targetType == float.class)
            {
            return new Float(n.floatValue());
            }
        else if(targetType == short.class)
            {
            return new Short(n.shortValue());
            }
        else if(targetType == byte.class)
            {
            return new Byte(n.byteValue());
            }
        throw new Exception("unsupported numeric box type");
	    
	    }

	public void setProps(Object o, List nameValuePairs)
	throws Exception
	    {
	    //presumes name is :-prefixed
        PropertyDescriptor[] props = Introspector.getBeanInfo(o.getClass()).getPropertyDescriptors();
	    for(int i=0;i<nameValuePairs.size();i+=2)
	        {
	        String name = (String)nameValuePairs.get(i);
	        setProp(o,props,name.substring(1),nameValuePairs.get(i+1));
	        }
	    }
	
	void setProp(Object o, PropertyDescriptor[] props, String name, Object value)
	throws Exception
	    {
        for(int i=0;i<props.length;i++)
            {
            PropertyDescriptor prop = props[i];
//          only support keyword-style init of non-indexed props
            if(!(prop instanceof IndexedPropertyDescriptor) 
                    && prop.getName().equalsIgnoreCase(name))
                {
                Method method = prop.getWriteMethod();
                if(method != null)
                    {
	                Object[] args = new Object[1];
	                args[0] = boxArg(method.getParameterTypes()[0],value);
	                method.invoke(o,args);
	                return;
                    }
                }
            }
        throw new Exception("can't find property");
	    }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#bases(java.lang.Class)
     */
    public List bases(Class c) throws Exception
        {
        ArrayList supers = new ArrayList();
        if(c.isInterface())
            supers.add(Object.class);
        else if(c.getSuperclass() != null)
            supers.add(c.getSuperclass());
        Class[] interfaces = c.getInterfaces();
        for(int i=0;i<interfaces.length;i++)
            {
            Class inter = interfaces[i];
            boolean placed = false;
            for(int p=0;!placed && p<supers.size();p++)
                {
                Class s = (Class)supers.get(p);
                if(s.isAssignableFrom(inter))
                    {
                    supers.add(p,inter);
                    placed =true;
                    }
                }
            if(!placed)
                supers.add(inter);
                
            }
        for(int i=0;i<supers.size();i++)
            supers.set(i,((Class)supers.get(i)).getName());
        return supers;
        }

	/* (non-Javadoc)
	 * @see com.richhickey.foil.IReflector#makeProxy(com.richhickey.foil.IRuntimeServer, int, int, java.util.List)
	 */
	public Object makeProxy(IRuntimeServer runtime, int marshallFlags, int marshallDepth, List interfaceList) throws Exception {
		Class[] interfaces = new Class[interfaceList.size()];
		for(int i=0;i<interfaces.length;i++)
			interfaces[i] = RuntimeServer.typeArg(interfaceList.get(i));
		return Proxy.newProxyInstance(ClassLoader.getSystemClassLoader(),interfaces,
				new ProxyHandler(runtime,marshallFlags,marshallDepth));
	}

	/* (non-Javadoc)
	 * @see com.richhickey.foil.IReflector#getClassNames(java.lang.String, java.util.List)
	 */
	public List getClassNames(String jarfile, List packages) throws Exception{
		JarFile jar = new JarFile(jarfile);
		Enumeration entries = jar.entries();
		ArrayList names = new ArrayList();
		
		while(entries.hasMoreElements())
		{
			ZipEntry entry = (ZipEntry)entries.nextElement();
			if(!entry.isDirectory())
			{
				String ename = entry.getName();
				if(
					ename.endsWith(".class")
					&&
					(!	(ename.indexOf('$') >= 0
							&&
						Character.isDigit(ename.charAt((ename.indexOf('$') + 1)))))
					&&
					matchesSomePackage(ename,packages))
				{
					names.add(ename.substring(0,ename.length()-6).replace('/','.'));
				}
			}
		}
		return names;
	}
	boolean matchesSomePackage(String classname, List packages)
	{
		for(int i=0;i<packages.size();i++)
		{
			String p = (String)packages.get(i);
			if(classname.startsWith(p)
				&&
				(!p.endsWith("/") //recursive, we'll take any below
				||
				classname.indexOf("/",p.length()) == -1 //nonSubdirectory
				))
				return true;
		}
		return false;
	}
	
}
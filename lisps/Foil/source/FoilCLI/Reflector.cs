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
using System;
using System.Reflection;
using System.IO;
using System.Collections;

/**
 * @author Eric Thorsen
 *
 */

namespace  com.richhickey.foil 
{
	public class Reflector :IReflector
	{
		IBaseMarshaller baseMarshaller;
		public Reflector(IBaseMarshaller baseMarshaller)
		{
			this.baseMarshaller = baseMarshaller;
		}

    public class CallableField :ICallable
        {
        FieldInfo field;
        public CallableField(FieldInfo field)
            {
            this.field = field;
            }
        
        /* (non-Javadoc)
         * @see com.richhickey.foil.ICallable#invoke(java.lang.Object, java.util.List)
         */
        public Object invoke(Object target, ArrayList args)
            {
            try{
                if(args.Count == 0) //get
                    {
                    return field.GetValue(target);
                    }
                else if(args.Count == 1) //set
                    {
                    field.SetValue(target,boxArg(field.FieldType,args[0]));
                    return null;
                    }
                else 
                    throw new Exception("invalid number of args passed to field");
            	}
            catch(Exception ex)
            	{	
            	throw new TargetInvocationException(ex);
            	}
            }
        }

		public class CallableMethod :ICallable
		{
			ArrayList methods;

			public CallableMethod(ArrayList methods)
			{
				this.methods = methods;
			}
        
			public Object invoke(Object target,ArrayList args) 
			{
				//Object target = args[0];
				//args = args.GetRange(1,args.Count-1);
				foreach(MethodInfo m in methods)
				{
					ParameterInfo[] parameters = m.GetParameters();
					if(isCongruent(parameters,args))
					{
						Object[] boxedArgs = boxArgs(parameters,args);
						try
						{
							return m.Invoke(target,boxedArgs);
						}
						catch(Exception ex)
						{	
							throw new TargetInvocationException(ex);
						}
					}
				}
				throw new TargetInvocationException(new Exception("no matching function found"));
			}
		}

	static	public	Type[]	getParameterTypes(ParameterInfo[] parameters)
		{
			if(parameters==null||parameters.Length==0)
				return	null;
			Type[]	types	=	new Type[parameters.Length];
			for(Int32 i=0;i<parameters.Length;++i)
				types[i]	=	parameters[i].ParameterType;
			return	types;
		}

	static Object[] boxArgs(ParameterInfo[] parameters,ArrayList args)
		{
			if(parameters.Length == 0)
				return null;
			Object[] ret = new Object[parameters.Length];
			for(int i=0;i<parameters.Length;i++)
			{
				Object arg		= args[i];
				Type paramType	= parameters[i].ParameterType; 
				ret[i]			= boxArg(paramType,arg);
			}
			return ret;
		}
    
	static Object boxArg(Type paramType,Object arg)
		{
			if(paramType == Type.GetType("System.Boolean") && arg == null)
				return false;
			else
				return arg;
		}
    
	static Boolean isCongruent(ParameterInfo[] parameters,ArrayList args)
		{
			Boolean ret = false;
			if(parameters.Length == (args==null?0:args.Count))
			{
				ret = true;
				for(int i=0;ret && i<parameters.Length;i++)
				{
					Object arg		= args[i];
					Type argType	= (arg == null)?null:arg.GetType();
					Type paramType	= parameters[i].ParameterType; 
					if(paramType == Type.GetType("System.Boolean"))
					{
						ret = arg == null || argType == Type.GetType("System.Boolean");
					}
					else if(paramType.IsPrimitive)
						ret	=	paramType	==	argType;
					else
					{
						ret = arg == null 
							|| argType == paramType 
							|| paramType.IsAssignableFrom(argType);
					}
				}
			}
			return ret;
		}
    
	public ICallable getCallable(int memberType, Type c, String memberName)
		{
			switch(memberType)
			{
				case ICallableFlags.METHOD:
					return getMethod(c,memberName);
				case ICallableFlags.FIELD:
					return getField(c,memberName);
				case ICallableFlags.PROPERTY_GET:
					return getPropertyGetter(c,memberName);
				case ICallableFlags.PROPERTY_SET:
					return getPropertySetter(c,memberName);
				default:
					throw new Exception("unsupported member type");
			}
		}

      
	ICallable getMethod(Type c,String method)
		{
			MethodInfo[] allmethods = c.GetMethods();
			ArrayList methods 		= new ArrayList();
			for(int i=0;i<allmethods.Length;i++)
			{
				if(method	==	allmethods[i].Name)
					methods.Add(allmethods[i]);
			}
			if(methods.Count == 0)
				throw new Exception("no methods found");
			return new CallableMethod(methods);
		}

	ICallable getField(Type c,String field)
			{
				FieldInfo[] allfields = c.GetFields();
				for(int i=0;i<allfields.Length;i++)
					{
					if(field.Equals(allfields[i].Name))
						return new CallableField(allfields[i]);
					}
			throw new Exception("no field found");
			}
 
	ICallable getPropertyGetter(Type c,String property)
        {
        PropertyInfo[] props = c.GetProperties();
        ArrayList methods = new ArrayList();
        for(int i=0;i<props.Length;i++)
            {
            if(property.Equals(props[i].Name))
                methods.Add(props[i].GetGetMethod());
            }
        if(methods.Count == 0)
            throw new Exception("no properties found");
        return new CallableMethod(methods);
        }

    ICallable getPropertySetter(Type c,String property)
        {
		PropertyInfo[] props = c.GetProperties();
        ArrayList methods = new ArrayList();
        for(int i=0;i<props.Length;i++)
            {
            if(property.Equals(props[i].Name))
                methods.Add(props[i].GetSetMethod());
            }
        if(methods.Count == 0)
            throw new Exception("no properties found");
        return new CallableMethod(methods);
        }

    public Object createNew(Type c, ArrayList args) 
        {
        ConstructorInfo[] ctors = c.GetConstructors();
        for(int i=0;i<ctors.Length;i++)
            {
            ConstructorInfo ctor	= ctors[i];
            ParameterInfo[] params_ = ctor.GetParameters();
            if(isCongruent(params_,args))
                {
                Object[] boxedArgs = boxArgs(params_,args);
                try{
                    return ctor.Invoke(boxedArgs);
                	}
                catch(Exception ex)
                	{	
                	throw new TargetInvocationException(ex);
                	}
                }
            }
        throw new TargetInvocationException(new Exception("no matching ctor found"));
        }

   
	public void members(Type c, TextWriter w)
	 {
        w.Write(" (");
        
        ConstructorInfo[] ctors = c.GetConstructors(BindingFlags.Instance|BindingFlags.Public);
        if(ctors.Length > 0)
            {
            w.Write("(:ctors ");
            for(int i=0;i<ctors.Length;++i)
                {
                ConstructorInfo ctor = ctors[i];
                baseMarshaller.marshallAtom(ctor.ToString(),w,0,0);
                //Type[] params_ = ctor.GetParameters();
                //reflectParams(params_,w);
                }
            w.Write(')');
            }
        
        MethodInfo[] methods = c.GetMethods(BindingFlags.Instance|BindingFlags.Public|BindingFlags.Static);
        if(methods.Length > 0)
            {
            w.Write("(:methods ");
            for(int i=0;i<methods.Length;i++)
                {
                MethodInfo method = methods[i];
                w.Write('(');
					 
                //w.Write("(:mref ");
                //baseMarshaller.marshallAtom(method,w,IBaseMarshallerFlags.MARSHALL_ID,0);
                //w.Write(')');
                
                w.Write("(:name ");
                baseMarshaller.marshallAtom(method.Name,w,IBaseMarshallerFlags.MARSHALL_ID,0);
                w.Write(')');

                //if(method.IsStatic)
                    {
                    w.Write("(:static ");
                    baseMarshaller.marshallAtom(method.IsStatic
												,w
												,IBaseMarshallerFlags.MARSHALL_ID
												,0);
                    w.Write(')');
                    }
                //reflectMethodSignature(method,w);
                    w.Write("(:doc ");
                    baseMarshaller.marshallAtom(method.ToString()
												,w
												,IBaseMarshallerFlags.MARSHALL_ID
												,0);
                    w.Write(')');
                w.Write(')');
                }
            w.Write(')');
            }

        FieldInfo[] fields = c.GetFields(BindingFlags.Instance|BindingFlags.Public|BindingFlags.Static);
        if(fields.Length > 0)
            {
            w.Write("(:fields ");
            for(int i=0;i<fields.Length;i++)
                {
                FieldInfo field = fields[i];
                w.Write("(");
                
                w.Write("(:name ");
                baseMarshaller.marshallAtom(field.Name,w,IBaseMarshallerFlags.MARSHALL_ID,0);
                w.Write(')');

                //if(field.IsStatic)
                   {
	                w.Write("(:static ");
	                baseMarshaller.marshallAtom(	field.IsStatic
													,w
													,IBaseMarshallerFlags.MARSHALL_ID
													,0);
	                w.Write(')');
                   }

                w.Write("(:doc ");
                baseMarshaller.marshallAtom(field.ToString(),w,IBaseMarshallerFlags.MARSHALL_ID,1);
                w.Write(')');
					 
                w.Write(')');
                }
            w.Write(')');
            }
        
        PropertyInfo[] props = c.GetProperties(BindingFlags.Instance|BindingFlags.Public|BindingFlags.Static);
        if(props.Length > 0)
            {
            w.Write("(:properties ");
            for(int i=0;i<props.Length;i++)
                {
                PropertyInfo prop = props[i];
                
                w.Write("(");
					 w.Write("(:name ");
                baseMarshaller.marshallAtom(prop.Name,w,IBaseMarshallerFlags.MARSHALL_ID,0);
                w.Write(')');

                //only create this section if static
                //never true for Java
                w.Write("(:static ");
                baseMarshaller.marshallAtom(false,w,IBaseMarshallerFlags.MARSHALL_ID,0);
                w.Write(')');

                MethodInfo readm = prop.GetGetMethod();
                if(readm != null)
                    {
                    w.Write("(:get-doc ");
                    baseMarshaller.marshallAtom(readm.ToString(),w,IBaseMarshallerFlags.MARSHALL_ID,0);
                    w.Write(')');
                    }
                MethodInfo setm = prop.GetSetMethod();
                if(setm != null)
                    {
                    w.Write("(:set-doc ");
                    baseMarshaller.marshallAtom(setm.ToString(),w,IBaseMarshallerFlags.MARSHALL_ID,0);
                    w.Write(')');
                    }
                w.Write(')');
                }

            w.Write(')');
            }

        w.Write(')');
        }

    void reflectMethodSignature(MethodInfo method, TextWriter w)
        {
        ParameterInfo[] params_ = method.GetParameters();
        reflectParams(params_,w);
        w.Write("(:ret");
        baseMarshaller.marshallAtom(method.ReturnType,w,IBaseMarshallerFlags.MARSHALL_ID,1);
        w.Write(')');
        }
    
	void reflectParams(ParameterInfo[] params_, TextWriter w)
		{
	    w.Write("(:args ");
	    for(int p=0;p<params_.Length;p++)
	        {
	        w.Write('(');
	
	        //we don't have param names in Java (maybe in 5?), but will in .Net
//	        w.Write("(:name");
//	        w....
//	        w.Write(')');
	        
	        w.Write("(:type");
	        baseMarshaller.marshallAtom(params_[p].ParameterType,w,IBaseMarshallerFlags.MARSHALL_ID,1);
	        w.Write(')');
	        
	        w.Write(')');
	        }
	    w.Write(')');
		}

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#createVector(java.lang.Type, int, java.util.List)
     */
    public Object createVector(Type c, int length, ArrayList inits) 
        {
        Array ret = Array.CreateInstance(c,length);
        Boolean isNumeric = isNumericType(c);
        for(int i=0;i<inits.Count;i++)
            {
            if(isNumeric)
                ret.SetValue(numericConvert(c,inits[i]),i);
            else
                ret.SetValue(inits[i],i);
            }
        return ret;
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorGet(java.lang.Object, int)
     */
    public Object vectorGet(Object v, int index) 
        {
        return ((Array)v).GetValue(index);
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorSet(java.lang.Object, int, java.lang.Object)
     */
    public void vectorSet(Object v, int index, Object val) 
        {
        ((Array)v).SetValue(val,index);
        }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReflector#vectorLength(java.lang.Object)
     */
    public Object vectorLength(Object v) 
        {
        return ((Array)v).Length;
        }

    internal	static Boolean isNumericType(Type c)
        {
        return c.IsPrimitive;
        }

	internal	static Object numericConvert(Type targetType,Object num)
	    {
		return Convert.ChangeType(num,targetType);
	    }

	public void setProps(Object o, ArrayList nameValuePairs)
	    {
	    //presumes name is :-prefixed
        PropertyInfo[] props = o.GetType().GetProperties();
	    for(int i=0;i<nameValuePairs.Count;i+=2)
	        {
	        String name = (String)nameValuePairs[i];
	        setProp(o,props,name.Substring(1),nameValuePairs[i+1]);
	        }
	    }
	
	void setProp(Object o, PropertyInfo[] props, String name, Object value_)
	    {
        for(int i=0;i<props.Length;i++)
            {
            PropertyInfo prop = props[i];
//          only support keyword-style init of non-indexed props
            if(!(prop.PropertyType.IsArray) 
                    && String.Compare(prop.Name,name,true)==0)
                {
                MethodInfo method = prop.GetSetMethod();
                if(method != null)
                    {
	                Object[] args = new Object[1];
	                args[0] = boxArg(method.GetParameters()[0].ParameterType,value_);
	                method.Invoke(o,args);
	                return;
                    }
                }
            }
        throw new Exception("can't find property");
	    }

	public	Object		indexerGet(Object o,ArrayList indexes) 
		{
			PropertyInfo prop = o.GetType().GetProperty("Item",BindingFlags.Instance|BindingFlags.Public);
			if(prop!=null)
			{
				MethodInfo method = prop.GetGetMethod();
				if(method != null)
				{
					Object[] args = boxArgs(method.GetParameters(),indexes);
					return	method.Invoke(o,args);
				}
				else
					throw new Exception(String.Format("No get method for indexer property \"Item\" for type {0}.  Could be write only?",o.GetType()));
			}
			throw new Exception(String.Format("Can't find indexer property \"Item\" for type {0}.",o.GetType()));
		}

	/// <summary>
		/// Eric Thorsen - Assumes the last element in 'indexes' is the value to be assigned.
		/// </summary>
		/// <param name="o"></param>
		/// <param name="indexes"></param>
	public	void		indexerSet(Object o,ArrayList indexes)
		{
			PropertyInfo prop = o.GetType().GetProperty("Item",BindingFlags.Instance|BindingFlags.Public);
			if(prop!=null)
			{
				MethodInfo method = prop.GetSetMethod();
				if(method != null)
				{
					Object[] args = boxArgs(method.GetParameters(),indexes);
					method.Invoke(o,args);
				}
				else
					throw new Exception(String.Format("No set method for indexer property \"Item\" for type {0}.  Could be read only?",o.GetType()));
			}
			else
				throw new Exception(String.Format("Can't find indexer property \"Item\" for type {0}.",o.GetType()));
		}
		 
    public ArrayList bases(Type c) 
	 {
        ArrayList supers = new ArrayList();
        if(c.IsInterface)
            supers.Add(Type.GetType("System.Object"));
        else if(c.BaseType != null) 
            supers.Add(c.BaseType);
        Type[] interfaces = c.GetInterfaces();
        for(int i=0;i<interfaces.Length;i++)
            {
            Type inter = interfaces[i];
            bool placed = false;
            for(int p=0;!placed && p<supers.Count;p++)
                {
                Type s = (Type)supers[p];
                if(s.IsAssignableFrom(inter))
                    {
                    supers[p]	=	inter;
                    placed		=	true;
                    }
                }
            if(!placed)
                supers.Add(inter);
            }
        for(int p=0;p<supers.Count;p++)
            supers[p]	=	((Type)supers[p]).ToString();
		return	supers;
	 }

	public Object makeProxy(IRuntimeServer runtime, int marshallFlags, int marshallDepth, ArrayList interfaceList) 
		{
		Type[] interfaces = new Type[interfaceList.Count];
		for(int i=0;i<interfaces.Length;i++)
			interfaces[i] = RuntimeServer.typeArg(interfaceList[i]);
		ProxyHandler handler	=	new ProxyHandler(runtime,marshallFlags,marshallDepth);
        return Proxy.BuildProxy(new Proxy.InvocationDelegate(handler.invoke),interfaces);
		}

		public	ArrayList	getClassNames(Object assembly,ArrayList assemblyNames)
		{
			Assembly	asm	=	Assembly.LoadFrom(assembly.ToString());
			Type[]	types	=	asm.GetTypes();
			//Covert the '/' to '.'
			SortedList	packages	=	new SortedList();
			foreach(String	pname	in assemblyNames)
			{
				String s = pname.Replace('/','.');
				packages.Add(s,s);
			}
			ArrayList	typeNames	=	new ArrayList();
			foreach(Type t in types)
			{
				if(anyMatch(packages,t.ToString()))
					typeNames.Add(t.ToString());
			}
			return	typeNames;
		}

		static	bool	anyMatch(SortedList packages,String fullTypeName)
		{
			foreach(String package in packages.Keys)
				if(includeType(package,fullTypeName))
					return	true;
			return	false;
		}

		static	bool	includeType(String package,String fullTypeName)
		{
			return	fullTypeName.StartsWith(package)
				&& (package[package.Length-1] != '.' ||
					fullTypeName.IndexOf('.',package.Length)==-1)
				&&	(fullTypeName.IndexOfAny(new char[] {'$','+'})==-1
					&& fullTypeName.IndexOf("__")==-1
					&& fullTypeName.IndexOf("PrivateImplementationDetails")==-1);
		}
	}
}


												  

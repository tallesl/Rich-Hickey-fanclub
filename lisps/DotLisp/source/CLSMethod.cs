//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal class CLSMethod : CLSMember
	{
	private Type type;
	private MemberInfo[] methods;
	private Boolean isStatic;

	internal CLSMethod(String name, Type type, MemberInfo[] methods,Boolean isStatic)
		{
		this.isStatic = isStatic;
		this.type = type;
		this.methods = methods;
		this.name = name;
		}



	public override Object Invoke(params Object[] args)
		{
		Object target = null;
		Object[] argarray = args;
		if(!isStatic)
			{				 // instance field gets target from first arg
			target=args[0];
			argarray = Util.vector_rest(args); 
			}
		MethodInfo mi = null;
		if(methods.Length == 1)	//it's not overloaded
			{
			mi = (MethodInfo)methods[0];
			return mi.Invoke(target,argarray);
			}
		else
			{
			//this should always work, but seems to have problems, i.e. String.Concat
			if(Util.containsNull(argarray))
				{
				return type.InvokeMember(this.name,
												 BindingFlags.Public|BindingFlags.InvokeMethod//|BindingFlags.FlattenHierarchy
												 |(isStatic?BindingFlags.Static:BindingFlags.Instance)
												 ,null,target,argarray);
				}
			///*
			Type[] argtypes = Type.GetTypeArray(argarray);
			//todo cache result?
			//n.b. we are not specifying static/instance here - hmmm...
			mi = type.GetMethod(name,argtypes);
			if(mi == null)
				{
				throw new Exception("Can't find matching method: " + name + " for: " + type.Name +
										  " taking those arguments");
				}
			//*/
			return mi.Invoke(target,argarray);
			}
		}

	}
}

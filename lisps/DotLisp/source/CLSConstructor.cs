//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal class CLSConstructor	//: IFunction
	{
	private Type type;
	internal String name;

	internal CLSConstructor(Type type)
		{
		this.type = type;
		this.name = type.Name;
		}

	internal static Object Invoke(Type type, params Object[] args)
		{
		//if(Util.containsNull(args))
		{
			return type.InvokeMember(type.Name,
											 BindingFlags.CreateInstance//|BindingFlags.FlattenHierarchy
											 //|(isStatic?BindingFlags.Static:BindingFlags.Instance)
											 ,null,null,args);
		}
		}

	}
}

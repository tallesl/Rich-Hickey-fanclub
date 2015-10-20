//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal abstract class CLSMember : IFunction
	{
	public abstract Object Invoke(params Object[] args);
	internal virtual Object getValue()
		{
		return this;
		}

	internal virtual void setValue(Object val)
		{
		throw new Exception("Can't set value of member symbol");
		}

	internal String name;

	public override String ToString()
		{
		return "CLSMember:" + name;
		}

	internal static Object GetDefaultIndexedProperty(Object target,Object[] args)
		{
		Type targetType = target.GetType();
		return targetType.InvokeMember("",BindingFlags.Default |BindingFlags.GetProperty , null,
												 target,args);
		}

	internal static Object SetDefaultIndexedProperty(Object target,Object[] args)
		{
		Type targetType = target.GetType();
		return targetType.InvokeMember("",BindingFlags.Default |BindingFlags.SetProperty , null,
												 target,args);
		}

	internal static CLSMember FindMember(String name, Type type, Boolean isStatic)
		{
		if(type == null)
			{
			return new CLSLateBoundMember(name);
			}
		//lookup name in type, create approriate derivee
		MemberInfo[] members = type.GetMember(name,
														  BindingFlags.Public|
														  (isStatic?BindingFlags.Static:BindingFlags.Instance)
														 ); //all public members with matching isstatic
		if(members.Length == 0)
			{
			throw new Exception("Can't find " + 
									  (isStatic?"static":"instance") + 
									  " member: " + name + " in Type: " + type.Name);
			}

		//CLS says all same-named members must be same type (field or param or method)
		//so just check first one

		if(members[0] is FieldInfo)
			{
			FieldInfo fi = (FieldInfo) members[0];
			return new CLSField(name,type,fi,fi.IsStatic);
			}

		else if(members[0] is PropertyInfo)
			{
			PropertyInfo pi = (PropertyInfo) members[0];
			//why doesn't PropertyInfo have IsStatic?
			MethodInfo mi = pi.GetGetMethod();
			return new CLSProperty(name,type,members,mi.IsStatic);
			}

		else if(members[0] is MethodInfo)
			{
			MethodInfo mi = (MethodInfo)members[0];
			return new CLSMethod(name,type,members,mi.IsStatic);
			}
		else
			{
			throw new Exception("Unsupported type of member: " + name +" in Type: " 
									  + type.Name + " MemberType: " + members[0].MemberType);
			}
		}
	}

}

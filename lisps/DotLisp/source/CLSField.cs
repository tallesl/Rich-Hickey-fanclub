//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal class CLSField : CLSMember
	{
	private Type type;
	private FieldInfo fi;
	private Boolean isStatic;

	internal CLSField(String name, Type type, FieldInfo fi,Boolean isStatic)
		{
		this.isStatic = isStatic;
		this.type = type;
		this.fi = fi;
		this.name = name;
		}

	internal override Object getValue()
		{
		if(isStatic)
			{
			return fi.GetValue(null);
			}
		return this;
		}

	internal override void setValue(Object val)
		{
		if(isStatic)
			fi.SetValue(null,val);
		}

	public override Object Invoke(Object[] args)
		{
		Object target = null;
		Boolean isSet = isStatic?(args.Length==1):(args.Length==2);
		if(!isStatic)
			{				 // instance field gets target from first arg
			target=args[0];
			}
		if(isSet)		 // an extra arg indicates a "set"
			{
			Object val = isStatic?args[0]:args[1];
			fi.SetValue(target,val);
			return val;
			}
		else
			return fi.GetValue(target);
		}
	}
}


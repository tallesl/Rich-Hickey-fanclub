//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal class CLSProperty : CLSMember
	{
	private Type type;
	private PropertyInfo pi; 
	private Boolean isStatic;

	internal CLSProperty(String name, Type type, MemberInfo[] properties,Boolean isStatic)
		{
		this.isStatic = isStatic;
		this.type = type;
		this.pi = (PropertyInfo)properties[0];
		this.name = name;
		for(int i=0;i<properties.Length;i++)
			{
			PropertyInfo pi = (PropertyInfo)properties[i];
			ParameterInfo[] info = pi.GetIndexParameters();
			if(info.Length > 0) //it's an indexed property - we don't support field-style access
				{
				throw new Exception("Field-style access for indexed property  of: " + type.Name + 
										  " not supported -" +
										  " use get_" + name + " and set_" + name + " functions instead");
				}
			}
		}

	internal override Object getValue()
		{
		if(isStatic)
			{
			return pi.GetValue(null,null);
			}
		return this;
		}

	internal override void setValue(Object val)
		{
		if(isStatic)
			pi.SetValue(0,val,null);
		}

	public override Object Invoke(params Object[] args)
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
			pi.SetValue(target,val,null);
			return val;
			}
		else
			return pi.GetValue(target,null);

		}
	}
}

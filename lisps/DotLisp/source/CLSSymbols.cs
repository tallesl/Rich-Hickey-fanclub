//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;

namespace DotLisp{
internal class CLSInstanceSymbol:Symbol
	{

	internal CLSInstanceSymbol(String name,String memberName,Type type)
	:base(name)
		{
		globalValue = CLSMember.FindMember(
													 memberName,
													 type,false);
		}

	override public Object getGlobalValue()
		{
		return globalValue;
		}

	override public Object setGlobalValue(Object newval)
		{
		throw new Exception("Cannot set value of instance member symbol: " + name);
		}
	}

internal class CLSStaticSymbol:Symbol
	{
	private CLSMember member;

	internal CLSStaticSymbol(String name,String memberName,Type type):base(name)
		{
		globalValue = member = CLSMember.FindMember(
																 memberName,
																 type,true);
		}

	override public Object getGlobalValue()
		{
		return member.getValue();
		}

	override public Object setGlobalValue(Object newval)
		{
		member.setValue(newval);
		return newval;
		}
	}

internal class CLSTypeSymbol:Symbol
	{
	internal CLSTypeSymbol(String name,Type type):base(name)
		{
		globalValue = type;
		}

	override public Object getGlobalValue()
		{
		return globalValue;
		}

	public override Object setGlobalValue(Object newval)
		{
		throw new Exception("Cannot set value of Type symbol: " + name);
		}
	}

}
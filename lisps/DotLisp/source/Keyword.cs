//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;

namespace DotLisp{
internal class Keyword : Symbol	 //derived just to give it distinguished type
	{
	internal Keyword(String name):
	base(name)
		{
		globalValue = this;
		}

	public override Object setGlobalValue(Object newval)
		{
		throw new Exception("Cannot set value of keyword: " + name + 
								  " - keywords evaluate to themselves");
		}
	}

internal class Constant : Symbol	  //derived just to give it distinguished type
	{
	internal Constant(String name,Object val):
	base(name)
		{
		globalValue = val;
		}

	public override Object setGlobalValue(Object newval)
		{
		throw new Exception("Cannot set value of constant: " + name);
		}
	}

}

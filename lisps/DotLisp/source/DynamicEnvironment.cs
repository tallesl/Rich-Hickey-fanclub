//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;

namespace DotLisp
{
internal class DynamicEnv
	{
	//this will have to be initialized with a copy of the creating thread's version
	//on thread start - no interface to thread start yet

	//hmmm... interpreter isolation will be broken if multiple interpreters on same thread
	//i.e. they will share the dynamic namespace
	[ThreadStatic] static DynamicEnv denv = null;

	internal static DynamicEnv current()
		{
		return denv;
		}

	internal static void restore(DynamicEnv olddenv)
		{
		denv = olddenv;
		}

	internal static void extend(Symbol sym, Object val)
		{
		if(!sym.isDynamic)
			throw new Exception("Dynamic vars must have prefix *");
		denv = new DynamicEnv(sym,val,denv);
		}

	internal static DynamicEnv find(Symbol s)
		{
		for(DynamicEnv d = denv;d != null;d = d.next)
			{
			if(d.sym == s)
				return d;
			}
		return null;
		}

	private DynamicEnv(Symbol sym,Object val, DynamicEnv next)
		{
		this.sym = sym;
		this.val = val;
		this.next = next;
		}

	internal Symbol sym;
	internal Object val;
	internal DynamicEnv next;

	}
}

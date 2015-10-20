//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;

namespace DotLisp{
internal class Env
	{

	Env parent;
	Parameter[] vars;
	Object[] vals;

	internal Env(Parameter[] vars, Object[] vals, Env parent)
		{
		this.vars = vars; this.vals = vals; this.parent = parent;
		}

	internal Object lookup(Symbol var) 
		{
		Int32 level = 0;
		for(Env e = this;e.parent != null;e = e.parent,++level)
			{
			for(Int32 i = 0;i<e.vars.Length;i++)
				{
				if(e.vars[i].symbol == var)
					return new LocalVariable(level, i, var);
				}
			}
		return var;
		}

	internal Object getValue(LocalVariable var) 
		{
		return rib(var.level).vals[var.index];
		}

	internal Object setValue(LocalVariable var, Object newVal) 
		{
		return rib(var.level).vals[var.index] = newVal;
		}  

	internal Env rib(int level) 
		{
		Env ret = this;
		while(level > 0)
			{
			ret = ret.parent;
			--level;
			}
		return ret;
		}

	}

}

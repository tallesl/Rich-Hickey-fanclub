//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
namespace DotLisp{

public class LocalVariable
	{
	internal Int32 level;
	internal Int32 index;

	// The name of the variable for debugging
	internal Symbol sym;

	internal LocalVariable(Int32 level, Int32 index, Symbol name) 
		{
		this.level = level; 
		this.index = index; 
		this.sym = name;
		}

	public override String ToString() 
		{
		return sym.ToString() + "^" + level + "->" + index; 
		}
	}

}

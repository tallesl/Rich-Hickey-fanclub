//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;

namespace DotLisp{

internal class Macro: Closure	//derived just to distinguish type
	{
	internal Macro(Cons args,Env env,Interpreter interpreter,Loc loc)
	:base(args,env,interpreter,loc)
		{
		}
	}
}

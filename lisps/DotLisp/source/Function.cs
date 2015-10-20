//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp
{
public delegate Object Function(params Object[] args);

public interface IFunction
{
	Object Invoke(params Object[] args);
}

}

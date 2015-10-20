//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Text;

namespace DotLisp {

internal class Parameter
	{
	internal enum Spec
		{
		REQ,OPT,KEY,REST
		}

	internal Symbol symbol;
	internal Spec spec = Spec.REQ;
	internal IExpression initCode = null;
	internal Symbol key = null;
	internal Object typeSpec = typeof(Object);

	internal Parameter(Symbol symbol, Spec spec, IExpression initCode)
		{
		if(symbol.isDynamic)
			throw new Exception("Dynamic variables cannot be parameters to functions or let");
		if(symbol is Keyword)
			throw new Exception("Keywords cannot be parameters to functions or let");
		this.symbol = symbol;
		this.spec = spec;
		this.initCode = initCode;
		}
	public override String ToString()
		{
		return "(" + symbol + " " + spec + " " + initCode + ")";
		}
	}

internal class ArgSpecs
	{
	internal ArgSpecs copy(Env env)
		{
		ArgSpecs a = (ArgSpecs)MemberwiseClone();
		a.env = env;
		return a;
		}
	internal Env env;

	internal ArgSpecs(Env env)
		{
		this.env = env;
		}
	internal Int32 nParams()
		{
		return parameters.Length;
		}
	public override String ToString()
		{
		StringBuilder str = new StringBuilder("(");
		foreach(Parameter p in parameters)
			{
			str.Append(p.ToString());
			str.Append(" ");
			}
		str.Append(")");
		return str.ToString();
		}
	internal Parameter[] parameters;
	internal int numReq = 0;
	internal int numOpt = 0;
	internal int numKey = 0;
	internal int numRest = 0; //0 or 1

	}
}
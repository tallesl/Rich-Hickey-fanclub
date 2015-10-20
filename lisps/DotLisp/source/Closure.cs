//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp{

internal class Closure: IFunction, IExpression
	{

	internal IExpression body;
	internal Env env;
	internal ArgSpecs argSpecs;
	internal Interpreter interpreter;
	internal Loc loc;

	internal Closure(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.interpreter = interpreter;
		this.loc = loc;
		ArgSpecs specs = analyzeArgSpec((Cons)args.first,env,loc);
		//create an env expanded by params in which to analyze body
		Env env2 = new Env(specs.parameters, null, env);

		this.argSpecs = specs;
		this.body = interpreter.analyze(new Cons(interpreter.BLOCK,args.rest), env2,loc);
		this.env = env;
		}

	public Object eval(Env env)
		{
		return copy(env);
		}

	internal int nParms()
		{
		return argSpecs.nParams();
		}

	public Object Invoke(params Object[] args)
		{
		try
			{
			return body.eval(
								 new Env(argSpecs.parameters, buildParamArray(args,0,env), env));
			}
		catch(Exception ex)
			{
			if(!(ex is BacktraceException))
				{
				throw BacktraceException.push(ex,new BacktraceFrame(loc,this,args),interpreter);
				}
			throw ex;
			}
		}


	//parse out params from spec (which may contain &optional, &key, &rest, initforms etc

	internal ArgSpecs analyzeArgSpec(Cons arglist, Env env,Loc loc)
		{
		//count the params
		int nParams = 0;
		Cons a = arglist;
		while(a!=null)
			{
			Object p = a.first;
			if(p!=interpreter.AMPOPT && p!=interpreter.AMPKEY && p!=interpreter.AMPREST)
				++nParams;
			a = a.rest;
			}

		ArgSpecs ret = new ArgSpecs(env);
		ret.parameters = new Parameter[nParams];
		Parameter.Spec state = Parameter.Spec.REQ;

		int param = 0;
		a = arglist;
		while(a!=null)
			{
			Object p = a.first;
			switch(state)
				{
				case Parameter.Spec.REQ:
					if(p==interpreter.AMPOPT)
						state = Parameter.Spec.OPT;
					else if(p==interpreter.AMPKEY)
						state = Parameter.Spec.KEY;
					else if(p==interpreter.AMPREST)
						state = Parameter.Spec.REST;
					else
						{
						if(p is Symbol)
							{
							ret.parameters[param++] = 
							new Parameter((Symbol)p,Parameter.Spec.REQ,null);
							++ret.numReq;
							}
						else if(p is Cons)
							{
							ret.parameters[param] = 
							new Parameter((Symbol)((Cons)p).first,Parameter.Spec.REQ,null);
							ret.parameters[param].typeSpec = interpreter.eval(Cons.Second((Cons)p),env);
							++param;
							++ret.numReq;
							}
						}
					break;
				case Parameter.Spec.OPT:
					if(p==interpreter.AMPOPT)
						throw new Exception("&optional can appear only once in arg list");
					else if(p==interpreter.AMPKEY)
						state = Parameter.Spec.KEY;
					else if(p==interpreter.AMPREST)
						state = Parameter.Spec.REST;
					else
						{
						if(p is Symbol)
							{
							ret.parameters[param++] = 
							new Parameter((Symbol)p,Parameter.Spec.OPT,null);
							++ret.numOpt;
							}
						else if(p is Cons)
							{
							ret.parameters[param++] = 
							new Parameter((Symbol)((Cons)p).first,Parameter.Spec.OPT,
											  interpreter.analyze(Cons.Second((Cons)p),env,loc));
							++ret.numOpt;
							}
						else
							throw	new Exception("&optional parameters must be symbols or (symbol init-form)");
						}
					break;
				case Parameter.Spec.KEY:
					if(p==interpreter.AMPOPT)
						throw new Exception("&optional must appear before &key in arg list");
					else if(p==interpreter.AMPKEY)
						throw new Exception("&key can appear only once in arg list");
					else if(p==interpreter.AMPREST)
						state = Parameter.Spec.REST;
					else
						{
						if(p is Symbol)
							{
							ret.parameters[param] = 
							new Parameter((Symbol)p,Parameter.Spec.KEY,null);
							ret.parameters[param].key = interpreter.intern(":" + ((Symbol)p).name);
							++param;
							++ret.numKey;
							}
						else if(p is Cons)
							{
							ret.parameters[param] = 
							new Parameter((Symbol)((Cons)p).first,Parameter.Spec.KEY,
											  interpreter.analyze(Cons.Second((Cons)p),env,loc));
							ret.parameters[param].key = 
							interpreter.intern(":" + ((Symbol)((Cons)p).first).name);
							++param;
							++ret.numKey;
							}
						else
							throw	new Exception("&key parameters must be symbols or (symbol init-form)");
						}
					break;
				case Parameter.Spec.REST:
					if(p==interpreter.AMPOPT)
						throw new Exception("&optional must appear before &rest in arg list");
					else if(p==interpreter.AMPKEY)
						throw new Exception("&key must appear before &rest in arg list");
					else if(p==interpreter.AMPREST)
						throw new Exception("&rest can appear only once in arg list");
					else
						{
						if(!(p is Symbol))
							throw new Exception("&rest parameter must be a symbol");
						else
							{
							if(ret.numRest > 0) //already got a rest param
								throw new Exception("Only one &rest arg can be specified");
							ret.parameters[param++] = 
							new Parameter((Symbol)p,Parameter.Spec.REST,null);
							++ret.numRest;
							}
						}
					break;
				}

			a = a.rest;
			}

		return ret;
		}

	internal Closure copy(Env env)
		{
		Closure c = null;
		try
			{
			c = (Closure)this.MemberwiseClone();
			}
		catch(Exception e)
			{
			throw new Exception("internal error: no clone " + e.Message);
			}
		c.env = env;
		c.argSpecs = argSpecs.copy(env);
		return c;
		}

	public override String ToString()
		{
		return "{" + this.GetType().Name + " "// + /*this.name + toStringArgs() + */
		+ argSpecs.ToString() 
		+ "}";
		}

	//returns an array of evaluated args corresponding to params of argspec,
	//including substitution of default values where none provided, construction of
	//rest list etc
	//suitable for extending the environment prior to evaluating body of closure
	internal Object[] buildParamArray(Object[] code, Int32 offset,
												 Env env//, Boolean evalArgs
												)
		{
		//do nothing if fixed params and matching number
		if(argSpecs.nParams() == argSpecs.numReq  && argSpecs.numReq == code.Length)
			return code;

		Object[] argArray = new Object[nParms()];
		int nargs = code.Length-offset;
		if(nargs < argSpecs.numReq)
			throw new Exception("Too few arguments to procedure, expected at least " + argSpecs.numReq
									  +", but found "+ nargs  +" arguments");

		int i;
		// Fill in the required parameters
		for(i = 0; i < argSpecs.numReq; i++)
			{
			argArray[i] = //evalArgs?Interpreter.execute(code[i+offset], env):
							  code[i+offset];
			}

		//now grab args to satisfy optionals
		if(argSpecs.numOpt > 0)
			{
			for(i = argSpecs.numReq; i < argSpecs.numReq + argSpecs.numOpt; i++)
				{
				if(i<nargs)
					{
					argArray[i] = //evalArgs?Interpreter.execute(code[i+offset], env):
									  code[i+offset];
					//if missing passed to optional, get default
					if(argArray[i] == Missing.Value)
						{
						argArray[i] = getDefaultParamValue(argSpecs.parameters[i]);
						}
					}
				else //ran out of args, default the rest
					{
					argArray[i] = getDefaultParamValue(argSpecs.parameters[i]);
					}
				}
			}

		//build a rest list
		Cons rest = null;
		for(int x = code.Length-1;x-offset >= i; --x)
			{
			Object val = //evalArgs?Interpreter.execute(code[x], env):
							 code[x];
			rest = new Cons(val, rest);
			}

		//search for key args in rest
		if(argSpecs.numKey > 0)
			{
			for(i = argSpecs.numReq + argSpecs.numOpt; 
				i < argSpecs.numReq + argSpecs.numOpt + argSpecs.numKey; i++)
				{
				argArray[i] = findKeyParamValue(argSpecs.parameters[i],rest);
				}
			}

		// Add the rest parameter (if there is one)
		if(argSpecs.numRest == 1)
			{
			argArray[i] = rest;
			}

		return argArray;
		}

	internal Object getDefaultParamValue(Parameter p)
		{
		if(p.initCode != null)
			return p.initCode.eval(argSpecs.env);
		else
			return Missing.Value; //hmmm... could return null
		}

	internal Object findKeyParamValue(Parameter p,Cons args)
		{
		for(;args!=null;args = args.rest)
			{
			if(args.first == p.key)
				{
				if(args.rest != null)
					{
					Object ret = Cons.Second(args);
					if(ret == Missing.Value)
						{
						ret = getDefaultParamValue(p);
						}
					return ret;
					}
				else
					throw	new Exception("Key args must be provided in pairs of [:key value]");
				}
			}
		return getDefaultParamValue(p);
		}

	}

}

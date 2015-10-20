//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Diagnostics;

namespace DotLisp{

interface IExpression{
	Object eval(Env env);
}

interface IVar{
	void setval(Object val,Env env);
	Symbol getSymbol();
}

internal class QuoteExpr:IExpression
	{
	internal QuoteExpr(Object val)
		{
		this.val = val;
		}
	public Object eval(Env env)
		{
		return val;
		}

	Object val;
	}

internal class GlobalVar:IExpression, IVar
	{
	internal GlobalVar(Symbol s)
		{
		this.sym = s;
		}
	public Object eval(Env env)
		{
		return sym.getGlobalValue();
		}

	public void setval(Object val,Env env)
		{
		sym.setGlobalValue(val);
		}

	public Symbol getSymbol()
		{
		return sym;
		}

	internal Symbol sym;
	}

internal class LocalVar:IExpression, IVar
	{
	internal LocalVar(LocalVariable var)
		{
		this.var = var;
		}

	public Object eval(Env env)
		{
		return env.getValue(var);
		}

	public void setval(Object val,Env env)
		{
		env.setValue(var,val);
		}

	public Symbol getSymbol()
		{
		return var.sym;
		}

	internal LocalVariable var;
	}

internal class DynamicVar:IExpression, IVar
	{
	internal DynamicVar(Symbol s)
		{
		this.sym = s;
		}
	public Object eval(Env env)
		{
		DynamicEnv d = DynamicEnv.find(sym);
		if(d != null)
			return d.val;
		return sym.getGlobalValue();
		}

	public void setval(Object val,Env env)
		{
		DynamicEnv d = DynamicEnv.find(sym);
		if(d != null)
			d.val = val;
		sym.setGlobalValue(val);
		}

	public Symbol getSymbol()
		{
		return sym;
		}

	internal Symbol sym;
	}

internal struct BindPair
	{
	internal DynamicVar dvar;
	internal IExpression expr;
	}

internal class DynamicLet:IExpression
	{
	internal DynamicLet(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		Cons bindlist = (Cons)args.first;
		Int32 blen = Cons.Length(bindlist);
		if((blen%2) != 0)	//odd
			{
			throw new Exception("Odd number of args in dynamic-let binding list");
			}
		binds = new BindPair[blen/2];
		for(int i = 0;i<binds.Length;i++)
			{
			binds[i].dvar = (DynamicVar)interpreter.analyze(bindlist.first, env,loc);
			bindlist = bindlist.rest;
			binds[i].expr = interpreter.analyze(bindlist.first, env,loc);
			bindlist = bindlist.rest;
			}
		this.body = interpreter.analyze(new Cons(interpreter.BLOCK,args.rest), env,loc);
		}

	public Object eval(Env env)
		{
		DynamicEnv olddenv = DynamicEnv.current();
		try
			{
			for(int i = 0;i<binds.Length;i++)
				{
				DynamicEnv.extend(binds[i].dvar.sym,
										binds[i].expr.eval(env));
				}
			return body.eval(env);
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"set",null),interpreter);
			}
		finally
			{
			DynamicEnv.restore(olddenv);
			}
		}

	internal Interpreter interpreter;

	BindPair[] binds;
	IExpression body;
	Loc loc;
	}

internal class WhileExpression: IExpression
	{
	internal WhileExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		this.test = interpreter.analyze(args.first,env,loc);
		this.body = interpreter.analyze(new Cons(interpreter.BLOCK,args.rest),env,loc);
		}

	public Object eval(Env env)
		{
		try
			{
			while(Util.isTrue(test.eval(env)))
				{
				body.eval(env);
				}

			return null;
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"set",null),interpreter);
			}
		}
	internal Interpreter interpreter;

	internal IExpression test;
	internal IExpression body;
	Loc loc;
	}

internal class BlockExpression: IExpression
	{
	internal BlockExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		exprs = new IExpression[Cons.Length(args)];
		for(Int32 i=0;i<exprs.Length;i++,args = args.rest)
			{
			exprs[i] = interpreter.analyze(args.first,env,loc);
			}
		}

	public Object eval(Env env)
		{
		try
			{
			for(Int32 i=0;i<exprs.Length-1;i++)
				exprs[i].eval(env);

			return exprs[exprs.Length-1].eval(env);
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"set",null),interpreter);
			}
		}

	internal Interpreter interpreter;
	internal IExpression[] exprs;
	Loc loc;
	}

internal class OrExpression: IExpression
	{
	internal OrExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		exprs = new IExpression[Cons.Length(args)];
		for(Int32 i=0;i<exprs.Length;i++,args = args.rest)
			{
			exprs[i] = interpreter.analyze(args.first,env,loc);
			}
		}

	public Object eval(Env env)
		{
		try
			{
			for(Int32 i = 0; i < exprs.Length-1; i++)
				{
				Object result = exprs[i].eval(env);
				if(Util.isTrue(result))	return result;
				}

			return exprs[exprs.Length-1].eval(env);
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"set",null),interpreter);
			}
		}

	internal Interpreter interpreter;
	internal IExpression[] exprs;
	Loc loc;
	}  

internal class IfExpression: IExpression
	{
	internal IfExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		Int32 len = Cons.Length(args);
		if(len < 2 || len > 3)
			throw new Exception("Wrong number of args for if");
		test = interpreter.analyze(args.first,env,loc);
		brtrue = interpreter.analyze(Cons.Second(args),env,loc);
		if(len == 3)
			brfalse = interpreter.analyze(Cons.Third(args),env,loc);
		else
			brfalse = new QuoteExpr(null);
		}

	public Object eval(Env env)
		{
		try
			{
			if(Util.isTrue(test.eval(env)))
				return brtrue.eval(env);
			return brfalse.eval(env);
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"if",null),interpreter);
			}
		}

	internal Interpreter interpreter;
	IExpression test;
	IExpression brtrue;
	IExpression brfalse;
	Loc loc;
	}

internal class SetExpression: IExpression
	{
	internal SetExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		Int32 len = Cons.Length(args);
		if(len != 2)
			throw new Exception("Wrong number of args for set");
		var = (IVar)interpreter.analyze(args.first,env,loc);
		val = interpreter.analyze(Cons.Second(args),env,loc);
		}

	public Object eval(Env env)
		{
		try
			{
			Object retval = val.eval(env);
			var.setval(retval,env);
			return retval;
			}
		catch(BacktraceException bex)
			{
			throw bex;
			}
		catch(Exception ex)
			{
			throw BacktraceException.push(ex,new BacktraceFrame(loc,"set",null),interpreter);
			}
		}

	IVar var;
	IExpression val;
	internal Interpreter interpreter;
	Loc loc;
	}

internal class ApplyExpression: IExpression
	{
	internal ApplyExpression(Cons args,Env env,Interpreter interpreter,Loc loc)
		{
		this.loc = loc;
		this.interpreter = interpreter;
		fexpr = interpreter.analyze(args.first,env,loc);
		if(fexpr is IVar)
			{
			fsym = ((IVar)fexpr).getSymbol();
			}
		Int32 len = Cons.Length(args.rest);
		argexprs = new IExpression[len];
		args = args.rest;
		for(Int32 i=0;i<argexprs.Length;i++,args = args.rest)
			{
			argexprs[i] = interpreter.analyze(args.first,env,loc);
			}
		}

	public Object eval(Env env)
		{
		Object f = fexpr.eval(env);
		Object[] args = new Object[argexprs.Length];
		for(Int32 i=0;i<args.Length;i++)
			args[i] = argexprs[i].eval(env);

		Boolean doTrace = fsym != null &&
								interpreter.traceList.Contains(fsym);
		try
			{
			if(doTrace)
				{
				interpreter.trace(fsym.name,args);
				Trace.Indent();
				Object ret = Util.InvokeObject(f,args);
				Trace.Unindent();
				return ret;
				}
			else
				return Util.InvokeObject(f,args);
			}
		catch(Exception ex)
			{
			if(fsym != null && !fsym.name.Equals("throw"))
				{
				throw BacktraceException.push(ex,new BacktraceFrame(loc,fsym,args)
														,interpreter);
				}
			else
				{
				throw ex;
				}
			}
		}

	Symbol fsym = null;
	IExpression fexpr;
	IExpression[] argexprs;
	Interpreter interpreter;
	Loc loc;
	}
}
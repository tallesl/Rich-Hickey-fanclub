//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.IO;
using System.Reflection;
using System.Collections.Specialized;
//using System.Diagnostics;
using System.Collections;

namespace DotLisp
{
public class Interpreter
	{
	public Interpreter()
		{
		globalenv = new Env(null,null,null);
		reader = new Reader(this);
		symbolTable = new SymbolTable();

		Assembly[] asm = AppDomain.CurrentDomain.GetAssemblies();
		foreach(Assembly a in asm)
			{
			addTypesFrom(a);
			}

		internBuiltins();
		Primitives.internAll(this);
		LASTVAL.setGlobalValue(null);
		NEXTLASTVAL.setGlobalValue(null);
		THIRDLASTVAL.setGlobalValue(null);

		//these primitives are here, rather than in Primitives, because their implementation
		//requires calls to gfs bound to symbols		
		Intern("intern",new Function(internf));
		Intern("eval",new Function(evalf));
		Intern("load",new Function(loadf));
		Intern("map->list",new Function(map_to_list));
		Intern("apply",new Function(apply));
		Intern("<",new Function(lt));
		Intern("<=",new Function(lteq));
		Intern(">",new Function(gt));
		Intern(">=",new Function(gteq));
		Intern("==",new Function(eqeq));
		Intern("!=",new Function(noteq));

		strgf = (GenericFunction)intern("str").getGlobalValue();
		get_enum_gf = (GenericFunction)intern("get-enum").getGlobalValue();
		compare_gf = (BinOp)intern("compare").getGlobalValue();

		Intern("interpreter",this);
		}

	public void Intern(String sym,Object f)
		{
		intern(sym).setGlobalValue(f);
		}

	public void InternType(Type t)
		{
		symbolTable.internType(t);
		}

	public void InternTypesFrom(Assembly a)
		{
		//Assembly a = Assembly.LoadWithPartialName(assemblyName);
		addTypesFrom(a);
		}

	void addTypesFrom(Assembly a)
		{
		Type[] types = a.GetTypes();
		foreach(Type t in types)
			{
			symbolTable.internType(t);
			}
		}

	public String Str(Object x)
		{
		return Util.InvokeObject(strgf,x).ToString();
		}

	public Object Read(String streamname, TextReader t)
		{
		return reader.Read(new LocTextReader(streamname,t));
		}

	internal Object Read(LocTextReader t)
		{
		return reader.Read(t);
		}

	public Boolean Eof(Object o)
		{
		return Reader.eof(o);
		}

	public Object LoadFile(String path)
		{
		TextReader tr = new StreamReader(path);
		try
			{
			Load(new LocTextReader(Path.GetFullPath(path),tr));
			}
		finally
			{
			tr.Close();
			}
		return "loaded: " + path;
		}

	internal Object Load(LocTextReader t)
		{
		Object expr = null;
		do    
			{
			Int32 line = t.line;
			expr = Read(t);
			if(!Eof(expr))
				{
				try
					{
					eval(expr,globalenv);
					}
				catch(Exception ex)
					{
					throw BacktraceException.push(ex,
															new BacktraceFrame(new Loc(t.file,line),"when evaluating ",expr),this);
					}
				}
			}while(!Eof(expr));
		return true;
		}

	public Object Eval(Object x)
		{
		Object ret = null;
		try
			{
			ret = eval(x,globalenv);
			THIRDLASTVAL.setGlobalValue(NEXTLASTVAL.getGlobalValue());
			NEXTLASTVAL.setGlobalValue(LASTVAL.getGlobalValue());
			LASTVAL.setGlobalValue(ret);
			}
		catch(Exception e)
			{
			EX.setGlobalValue(e);
			throw;
			}
		return ret;
		}

	internal Object eval(Object x, Env env)
		{
		IExpression analyzedCode = analyze(x,env);
		return analyzedCode.eval(env); 
		//Object analyzedCode = analyze(x,env);
		//return execute(analyzedCode, env); 
		}

	public void Trace(Symbol s)
		{
		traceList[s] = null;
		}

	public void UnTrace(Symbol s)
		{
		traceList.Remove(s);
		}

	public void UnTraceAll()
		{
		traceList.Clear();
		}

	public ICollection TraceList{get {return traceList.Keys;}}

	internal void trace(String fname,Object[] args)
		{
		if(!inTrace)
			{
			inTrace = true;
			System.Diagnostics.Trace.WriteLine(fname + strgf.Invoke((Object)args));    
			inTrace = false;
			}
		}

	internal IExpression analyze(Object expr,Env env)
		{
		return analyze(expr,env,null);
		}

	internal IExpression analyze(Object expr, Env env, Loc enclosingLoc)
		{
		Symbol symbol = expr as Symbol;
		if(symbol != null)
			{
			if(symbol.isDynamic)
				return new DynamicVar(symbol);
			Object var = env.lookup(symbol);
			if(var is LocalVariable)
				return new LocalVar((LocalVariable)var);
			else
				return new GlobalVar((Symbol)var);
			}
		if(!(expr is Cons))	 //must be literal
			return new QuoteExpr(expr);

		Cons exprs = (Cons)expr;

		Loc loc = (Loc)reader.locTable[expr];
		if(loc != null)
			reader.locTable.Remove(expr);
		else
			loc = enclosingLoc;

		Object f = exprs.first;
		Cons args = exprs.rest;

		//see if it's a macro
		Symbol s = f as Symbol;
		if(s != null && s.isDefined() && s.getGlobalValue() is Macro)
			{
			Macro m = (Macro)s.getGlobalValue();
			Object[] argarray = Cons.ToVector(args);
			Object mexprs = null;
			try
				{
				mexprs = m.Invoke(argarray);
				}
			catch(Exception ex)
				{
				BacktraceFrame frame = new BacktraceFrame(loc,s,args);
				throw BacktraceException.push(ex,frame,this);
				}
			try
				{
				return analyze(mexprs,env,loc);
				}
			catch(Exception ex)
				{
				BacktraceFrame frame = new BacktraceFrame(loc,"when expanding ",exprs);
				throw BacktraceException.push(ex,frame,this);
				}
			}
		Int32 numargs = Cons.Length(args);

		if(f == DLET)
			return new DynamicLet(args,env,this,loc);
		else if(f == FN)
			return new Closure(args,env,this,loc);
		else if(f == MACRO)
			return new Macro(args,env,this,loc);
		else if(f == WHILE)
			return new WhileExpression(args,env,this,loc);
		else if(f == BLOCK)
			{
			if(numargs == 0)
				return new QuoteExpr(null);
			//remove block from block of one
			else if(numargs == 1)
				return analyze(args.first,env,loc);
			else
				return new BlockExpression(args,env,this,loc);
			}
		else if(f == OR)
			{
			if(numargs == 0)
				return new QuoteExpr(null);
			else
				return new OrExpression(args,env,this,loc);  
			}
		else if(f == IF)
			return new IfExpression(args,env,this,loc);
		else if(f == QUOTE)
			return new QuoteExpr(args.first);
		else if(f == SET)
			return new SetExpression(args,env,this,loc);
		else	//presume funcall
			return new ApplyExpression(exprs,env,this,loc);
		}


	internal Object evalf(params Object[] args)
		{
		Object x = Primitives.arg(0,args);

		return eval(x,globalenv);
		}

	internal Object loadf(params Object[] args)
		{
		String path = (String)Primitives.arg(0,args);
		return LoadFile(path);
		}

	internal Object map_to_list(params Object[] args)
		{
		Object f = Primitives.arg(0,args);
		IEnumerator[] enums = new IEnumerator[args.Length - 1];
		for(int i=0;i<enums.Length;i++)
			{
			enums[i] = (IEnumerator)get_enum_gf.Invoke(Primitives.arg(i+1,args));
			}
		//n.b. setting up arg array which will be reused
		//mean functions cannot assume ownership of args w/o copying them
		Object[] fargs = new Object[enums.Length];
		Cons ret = null;
		Cons tail = null;
		while(true)
			{
			for(int i=0;i<enums.Length;i++)
				{
				if(enums[i].MoveNext())
					fargs[i] = enums[i].Current;
				else //bail on shortest
					return ret;
				}

			Object x = Util.InvokeObject(f,fargs);
			Cons node = new Cons(x,null);
			if(ret == null)
				ret = tail = node;
			else
				tail = tail.rest = node;
			}
		}

	//last arg must be seq
	internal Object apply(params Object[] args)
		{
		Object f = Primitives.arg(0,args);
		IEnumerator tail = (IEnumerator)get_enum_gf.Invoke(args[args.Length-1]);
		ArrayList fargs = new ArrayList();
		for(int i = 1;i<args.Length-1;i++)
			fargs.Add(args[i]);
		while(tail.MoveNext())
			{
			fargs.Add(tail.Current);
			}
		return Util.InvokeObject(f,fargs.ToArray());
		}  

	internal Object lt(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) < 0;
		}

	internal Object lteq(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) <= 0;
		}

	internal Object gt(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) > 0;
		}

	internal Object gteq(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) >= 0;
		}

	internal Object eqeq(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) == 0;
		}

	internal Object noteq(params Object[] args)
		{
		return(Int32)Util.InvokeObject(compare_gf,args) != 0;
		}

	internal Object internf(params Object[] args)
		{
		String sym = (String)Primitives.arg(0,args);

		return symbolTable.intern(sym);
		}

	internal Symbol intern(String sym)
		{
		return symbolTable.intern(sym);
		}

	internal Symbol internConstant(String sym,Object val)
		{
		return symbolTable.internConstant(sym,val);
		}

	void internBuiltins()
		{
		BLOCK = intern("block");
		SET              = intern("__set");
		FIRST           = intern("first");
		REST            = intern("rest");
		DEF            = intern("def");
		IF              = intern("if");
		FN          = intern("fn");
		MACRO            = intern("macro");
		OR               = intern("or");
		WHILE           = intern("while");
		BREAK       = intern(":break");
		AMPOPT         = intern("&opt");
		AMPKEY          = intern("&key");
		AMPREST         = intern("&rest");
		QUOTE            = intern("quote");
		DLET            = intern("dynamic-let");
		STR            = intern("str");
		VECTOR         = intern("vector");
		COMPARE        = intern("compare");
		BACKQUOTE      = intern("backquote");
		UNQUOTE          = intern("unquote");
		UNQUOTE_SPLICING = intern("unquote-splicing");
		NIL            = internConstant("nil",null);
		TRUE        = internConstant("true",true);
		FALSE       = internConstant("false",false);
		EX          = intern("!");
		LASTVAL        = intern("$");
		NEXTLASTVAL    = intern("$$");
		THIRDLASTVAL   = intern("$$$");
		EOF            = intern(":eof");
		}

	internal Symbol 
	BLOCK ,
	SET              ,
	FIRST           ,
	REST            ,
	DEF            ,
	IF              ,
	FN          ,
	MACRO            ,
	OR               ,
	WHILE          , 
	BREAK       , 
	AMPOPT         , 
	AMPKEY          ,
	AMPREST         ,
	QUOTE            ,
	DLET            ,
	STR            ,
	VECTOR         ,
	COMPARE        ,
	BACKQUOTE      ,
	UNQUOTE         ,
	UNQUOTE_SPLICING ,
	NIL            ,
	TRUE        ,
	FALSE       ,
	EX          ,
	LASTVAL        ,
	NEXTLASTVAL    ,
	THIRDLASTVAL   ,
	EOF            ;

	Env globalenv;
	Reader reader;
	GenericFunction get_enum_gf;
	BinOp compare_gf;
	GenericFunction strgf;
	SymbolTable symbolTable;

	internal HybridDictionary traceList = new HybridDictionary();
	[ThreadStatic] static Boolean inTrace = false;
	}


}

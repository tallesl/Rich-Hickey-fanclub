//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt

using System;
using System.Collections;
using System.Reflection;
using System.IO;

namespace DotLisp
{
public class Primitives
	{
	internal static void internAll(Interpreter interpreter)
		{
		interpreter.Intern("not",new Function(not));
		interpreter.Intern("to-bool",new Function(to_bool));
		interpreter.Intern("nil?",new Function(nilp));
		interpreter.Intern("symbol?",new Function(symbolp));
		interpreter.Intern("eqv?",new Function(eqv));
		interpreter.Intern("eql?",new Function(eql));
		interpreter.Intern("cons",new Function(cons));
		interpreter.Intern("cons?",new Function(consp));
		interpreter.Intern("atom?",new Function(atomp));
		interpreter.Intern("first",new Function(first));
		interpreter.Intern("rest",new Function(rest));
		interpreter.Intern("set-first",new Function(setfirst));
		interpreter.Intern("set-rest",new Function(setrest));
		interpreter.Intern("second",new Function(second));
		interpreter.Intern("third",new Function(third));
		interpreter.Intern("fourth",new Function(fourth));
		interpreter.Intern("reverse",new Function(reverse));
		interpreter.Intern("list?",new Function(listp));
		interpreter.Intern("len",new Function(listlength));
		interpreter.Intern("nth",new Function(nth));
		interpreter.Intern("nth-rest",new Function(nthrest));
		interpreter.Intern("append",new Function(append));
		interpreter.Intern("concat!",new Function(concat_d));
		interpreter.Intern("type-of",new Function(type_of));
		interpreter.Intern("is?",new Function(isp));
		interpreter.Intern("even?",new Function(evenp));
		interpreter.Intern("gensym",new Function(gensym));
		interpreter.Intern("macroexpand-1",new Function(macroexpand_1));
		interpreter.Intern("vector",new Function(vector));
		interpreter.Intern("vector-of",new Function(vector_of));
		interpreter.Intern("throw",new Function(throwfun));
		interpreter.Intern("try-catch-finally",new Function(try_catch_finally));

		interpreter.Intern("<i",new Function(lti));
		interpreter.Intern("addi",new Function(addints));

		BinOp addop = new BinOp();
		addop.AddMethod(typeof(Int32),typeof(Int32),new Function(addints));
		addop.AddMethod(typeof(Int32),typeof(Object),new Function(addobjs));
		addop.AddMethod(typeof(Object),typeof(Object),new Function(addobjs));
		interpreter.Intern("add",addop);

		BinOp subtractop = new BinOp();
		subtractop.AddMethod(typeof(Int32),typeof(Int32),new Function(subtractints));
		subtractop.AddMethod(typeof(Int32),typeof(Object),new Function(subtractobjs));
		subtractop.AddMethod(typeof(Object),typeof(Object),new Function(subtractobjs));
		interpreter.Intern("subtract",subtractop);

		BinOp multiplyop = new BinOp();
		multiplyop.AddMethod(typeof(Int32),typeof(Int32),new Function(multiplyints));
		multiplyop.AddMethod(typeof(Int32),typeof(Object),new Function(multiplyobjs));
		multiplyop.AddMethod(typeof(Object),typeof(Object),new Function(multiplyobjs));
		interpreter.Intern("multiply",multiplyop);

		BinOp divideop = new BinOp();
		divideop.AddMethod(typeof(Int32),typeof(Int32),new Function(divideints));
		divideop.AddMethod(typeof(Int32),typeof(Object),new Function(divideobjs));
		divideop.AddMethod(typeof(Object),typeof(Object),new Function(divideobjs));
		interpreter.Intern("divide",divideop);

		BinOp compareop = new BinOp();
		compareop.AddMethod(typeof(Int32),typeof(Int32),new Function(subtractints));
		compareop.AddMethod(typeof(Int32),typeof(Object),new Function(compareobjs));
		compareop.AddMethod(typeof(Object),typeof(Object),new Function(compareobjs));
		interpreter.Intern("compare",compareop);

		BinOp bitorop = new BinOp();
		bitorop.AddMethod(typeof(Enum),typeof(Enum),new Function(bitorenum));
		bitorop.AddMethod(typeof(Object),typeof(Object),new Function(bitor));
		interpreter.Intern("bit-or",bitorop);

		BinOp bitandop = new BinOp();
		bitandop.AddMethod(typeof(Enum),typeof(Enum),new Function(bitandenum));
		bitandop.AddMethod(typeof(Object),typeof(Object),new Function(bitand));
		interpreter.Intern("bit-and",bitandop);

		BinOp bitxorop = new BinOp();
		bitxorop.AddMethod(typeof(Enum),typeof(Enum),new Function(bitxorenum));
		bitxorop.AddMethod(typeof(Object),typeof(Object),new Function(bitxor));
		interpreter.Intern("bit-xor",bitxorop);

		GenericFunction bitnotgf = new GenericFunction();     
		bitnotgf.AddMethod(typeof(Enum),new Function(bitnotenum));
		bitnotgf.AddMethod(typeof(Object),new Function(bitnot));
		interpreter.Intern("bit-not",bitnotgf);

		GenericFunction get_enum_gf = new GenericFunction();
		get_enum_gf.AddMethod(typeof(IEnumerator),new Function(get_enum_IEnumerator));
		get_enum_gf.AddMethod(typeof(IEnumerable),new Function(get_enum_IEnumerable));
		get_enum_gf.AddMethod(null,new Function(get_enum_nil));
		interpreter.Intern("get-enum",get_enum_gf);

		GenericFunction strgf = new GenericFunction();     
		strgf.AddMethod(null,new Function(strnil));
		strgf.AddMethod(typeof(Object),new Function(strobj));
		interpreter.Intern("str",strgf);

		//interpreter.Intern("pr",new GenericFunction());
		}


	internal static Object arg(Int32 idx,Object[] args)
		{
		if(idx >= args.Length)
			{
			throw new Exception("Insufficient arguments");
			}
		return args[idx];
		}


//basics///////////////////////////

	public static Object not(params Object[] args)
		{
		return !Util.isTrue(arg(0,args));
		}

	public static Object to_bool(params Object[] args)
		{
		return Util.isTrue(arg(0,args));
		}

	public static Object nilp(params Object[] args)
		{
		Object x = arg(0,args);
		return x == null;
		}

	public static Object symbolp(params Object[] args)
		{
		Object x = arg(0,args);
		return x is Symbol;
		}

	public static Object eqv(params Object[] args)
		{
		Object x = arg(0,args);
		Object y = arg(1,args);
		return Object.Equals(x,y);
		//return x == y || (x != null && x.Equals(y));
		}

	public static Object eql(params Object[] args)
		{
		Object x = arg(0,args);
		Object y = arg(1,args);
		if(x != null && x.GetType().IsValueType)
			{
			return x.Equals(y);
			}
		return x == y;
		}



//lists/////////////////////////////
	public static Object cons(params Object[] args)
		{
		Object x = arg(0,args);
		Cons y = (Cons)arg(1,args);
		return new Cons(x,y);
		}     

	public static Object consp(params Object[] args)
		{
		Object x = arg(0,args);
		return x is Cons;
		}

	public static Object atomp(params Object[] args)
		{
		Object x = arg(0,args);
		return !(x is Cons);
		}

	public static Object first(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		if(x == null)
			return null;
		return x.first;
		}

	public static Object rest(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);

		if(x == null)
			return null;
		return x.rest;
		}


	public static Object setfirst(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		Object y = arg(1,args);
		return x.first = y;
		}

	public static Object setrest(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		Cons y = (Cons)arg(1,args);
		return x.rest = y;
		}

	public static Object second(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		if(x == null)
			return null;
		return Cons.Second(x);;
		}

	public static Object third(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		if(x == null)
			return null;
		return Cons.Third(x);
		}

	public static Object fourth(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		if(x == null)
			return null;
		return Cons.Fourth(x);
		}

	public static Object reverse(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		if(x == null)
			return null;
		return Cons.Reverse(x);
		}

	public static Object listp(params Object[] args)
		{
		Object x = arg(0,args);
		return x == null || x is Cons;
		}

	public static Object listlength(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		return Cons.Length(x);
		}

	public static Object nth(params Object[] args)
		{
		Int32 i = Convert.ToInt32(arg(0,args));
		Cons list = (Cons)arg(1,args);
		return Cons.Nth(i,list);
		}

	public static Object nthrest(params Object[] args)
		{
		Int32 i = Convert.ToInt32(arg(0,args));
		Cons list = (Cons)arg(1,args);
		return Cons.NthTail(i,list);
		}

	public static Object append(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		Cons y = (Cons)arg(1,args);
		return Cons.Append(x,y);
		}

	public static Object concat_d(params Object[] args)
		{
		Cons tail = (Cons)args[args.Length-1];
		for(int x = args.Length - 2;x >= 0;--x)
			{
			Cons prev = (Cons)args[x];
			if(prev != null)
				{
				Cons.Last(1,prev).rest = tail;
				tail = prev;
				}
			}
		return tail;
		}  

	public static Object type_of(params Object[] args)
		{
		Object x = arg(0,args);

		if(x == null)
			return null;
		return x.GetType();
		}  

	public static Object isp(params Object[] args)
		{
		Object x = arg(0,args);
		Type y = (Type)arg(1,args);
		return y.IsInstanceOfType(x);
		}  

	public static Object gensym(params Object[] args)
		{
		return Symbol.gensym();    
		}     

	public static Object macroexpand_1(params Object[] args)
		{
		Cons x = (Cons)arg(0,args);
		Symbol s = x.first as Symbol;
		if(s != null && s.isDefined() && s.getGlobalValue() is Macro)
			{
			Macro m = (Macro)s.getGlobalValue();
			Object[] argarray = Cons.ToVector(x.rest);
			return m.Invoke(argarray);
			}
		else
			return x;      
		}  

	public static Object vector(params Object[] args)
		{
		if(args.Length == 0)
			return new Object[0];
		Boolean same = true;

		if(Util.containsNull(args))
			same = false;
		//if homogenous, make typed vector
		if(same)
			{
			Type type = arg(0,args).GetType();
			foreach(Object o in args)
				{
				if(!type.IsInstanceOfType(o))
					{
					same = false;
					break;
					}
				}
			if(same)
				{
				Array result = Array.CreateInstance(type,args.Length);
				Array.Copy(args,result,args.Length);
				return result;
				}
			}
		return args.Clone();
		}  

	public static Object vector_of(params Object[] args)
		{
		Type type = (Type)arg(0,args);
		Array result = Array.CreateInstance(type,args.Length-1);
		for(int i = 1;i<args.Length;i++)
			{
			result.SetValue(args[i],i-1);
			}
		return result;
		}  

	public static Object throwfun(params Object[] args)
		{
		Exception e = (Exception)arg(0,args);
		throw e;
		}  

	public static Object strnil(params Object[] args)
		{
		return "nil";
		}

	public static Object strobj(params Object[] args)
		{
		return arg(0,args).ToString();
		}

	public static Object addobjs(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					+ Convert.ToInt32(arg1);
				case TypeCode.Double:
					return Convert.ToDouble(arg0) 
					+ Convert.ToDouble(arg1);
				case TypeCode.Decimal:
					return Convert.ToDecimal(arg0) 
					+ Convert.ToDecimal(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					+ Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToDouble(arg0) 
													 + Convert.ToDouble(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object subtractobjs(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					- Convert.ToInt32(arg1);
				case TypeCode.Double:
					return Convert.ToDouble(arg0) 
					- Convert.ToDouble(arg1);
				case TypeCode.Decimal:
					return Convert.ToDecimal(arg0) 
					- Convert.ToDecimal(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					- Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToDouble(arg0) 
													 - Convert.ToDouble(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object multiplyobjs(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					* Convert.ToInt32(arg1);
				case TypeCode.Double:
					return Convert.ToDouble(arg0) 
					* Convert.ToDouble(arg1);
				case TypeCode.Decimal:
					return Convert.ToDecimal(arg0) 
					* Convert.ToDecimal(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					* Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToDouble(arg0) 
													 * Convert.ToDouble(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object divideobjs(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					/ Convert.ToInt32(arg1);
				case TypeCode.Double:
					return Convert.ToDouble(arg0) 
					/ Convert.ToDouble(arg1);
				case TypeCode.Decimal:
					return Convert.ToDecimal(arg0) 
					/ Convert.ToDecimal(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					/ Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToDouble(arg0) 
													 / Convert.ToDouble(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object compareobjs(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			return((IComparable)Convert.ChangeType(arg0,biggerType)).CompareTo
			(Convert.ChangeType(arg1,biggerType));
		}
		}

	public static Object subtractints(params Object[] args)
		{
		//Int32 arg0 = (Int32)arg(0,args);
		//Int32 arg1 = (Int32)arg(1,args);
		checked{
			return(Int32)args[0]-(Int32)args[1];
		}
		}

	public static Object addints(params Object[] args)
		{
		//Int32 i0 = (Int32)arg(0,args);
		//Int32 i1 = (Int32)arg(1,args);
		checked{
			return(Int32)args[0]+(Int32)args[1];;
		}
		}

	public static Object multiplyints(params Object[] args)
		{
		checked{
			return(Int32)args[0]*(Int32)args[1];
		}
		}

	public static Object divideints(params Object[] args)
		{
		checked{
			return(Int32)args[0]/(Int32)args[1];
		}
		}

	public static Object lti(params Object[] args)
		{
		return((Int32)args[0]-(Int32)args[1]) < 0;
		}


	public static Object evenp(params Object[] args)
		{
		Int32 x = Convert.ToInt32(arg(0,args));
		return(x%2) == 0;
		}

	public static Object try_catch_finally(params Object[] args)
		{
		IFunction body = arg(0,args) as IFunction;
		IFunction catchHandler = arg(1,args) as IFunction;
		IFunction finallyCleanup = arg(2,args) as IFunction;
		try
			{
			return body.Invoke();
			}
		catch(Exception e)
			{
			if(catchHandler != null)
				return catchHandler.Invoke(e);
			else
				throw;
			}
		finally
			{
			if(finallyCleanup != null)
				{
				finallyCleanup.Invoke();
				}
			}
		}

	public static Object get_enum_IEnumerable(params Object[] args)
		{
		IEnumerable e = (IEnumerable)arg(0,args);
		return e.GetEnumerator();
		}

	public static Object get_enum_IEnumerator(params Object[] args)
		{
		return(IEnumerator)arg(0,args);
		}

	public static Object get_enum_nil(params Object[] args)
		{
		return new ConsEnumerator(null);
		}

	public static Object bitor(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					| Convert.ToInt32(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					| Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToUInt32(arg0) 
													 | Convert.ToUInt32(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object bitorenum(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt32(arg0) 
												| Convert.ToInt32(arg1));
				case TypeCode.Int64:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt64(arg0) 
												| Convert.ToInt64(arg1));     
				default:
					return Enum.ToObject(arg0.GetType(),
												Convert.ToUInt32(arg0) 
												| Convert.ToUInt32(arg1));
				}
		}
		}

	public static Object bitand(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					& Convert.ToInt32(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					& Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToUInt32(arg0) 
													 & Convert.ToUInt32(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object bitandenum(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt32(arg0) 
												& Convert.ToInt32(arg1));
				case TypeCode.Int64:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt64(arg0) 
												& Convert.ToInt64(arg1));     
				default:
					return Enum.ToObject(arg0.GetType(),
												Convert.ToUInt32(arg0) 
												& Convert.ToUInt32(arg1));
				}
		}
		}

	public static Object bitxor(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Convert.ToInt32(arg0) 
					^ Convert.ToInt32(arg1);
				case TypeCode.Int64:
					return Convert.ToInt64(arg0) 
					^ Convert.ToInt64(arg1);      
				default:
					return Convert.ChangeType(
													 Convert.ToUInt32(arg0) 
													 ^ Convert.ToUInt32(arg1)
													 ,biggerType);
				}
		}
		}

	public static Object bitxorenum(params Object[] args)
		{
		Object arg0 = arg(0,args);
		Object arg1 = arg(1,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		TypeCode tc1 = Convert.GetTypeCode(arg1);
		TypeCode biggerType = tc0>tc1?tc0:tc1;
		checked{
			switch(biggerType)
				{
				case TypeCode.Int32:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt32(arg0) 
												^ Convert.ToInt32(arg1));
				case TypeCode.Int64:
					return Enum.ToObject(arg0.GetType(),Convert.ToInt64(arg0) 
												^ Convert.ToInt64(arg1));     
				default:
					return Enum.ToObject(arg0.GetType(),
												Convert.ToUInt32(arg0) 
												^ Convert.ToUInt32(arg1));
				}
		}
		}

	public static Object bitnotenum(params Object[] args)
		{
		Object arg0 = arg(0,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		checked{
			switch(tc0)
				{
				case TypeCode.Int32:
					return Enum.ToObject(arg0.GetType(),~Convert.ToInt32(arg0)) ;
				case TypeCode.Int64:
					return Enum.ToObject(arg0.GetType(),~Convert.ToInt64(arg0)) ;
				default:
					return Enum.ToObject(arg0.GetType(),
												~Convert.ToUInt32(arg0)); 
				}
		}
		}

	public static Object bitnot(params Object[] args)
		{
		Object arg0 = arg(0,args);
		TypeCode tc0 = Convert.GetTypeCode(arg0);
		checked{
			switch(tc0)
				{
				case TypeCode.Int32:
					return ~Convert.ToInt32(arg0) ;
				case TypeCode.Int64:
					return ~Convert.ToInt64(arg0) ;
				default:
					return Convert.ChangeType(
													 ~Convert.ToUInt32(arg0) 
													 ,tc0);
				}
		}
		}

	}
}

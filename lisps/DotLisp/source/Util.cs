//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;

namespace DotLisp
{
public class Util
	{
	public static Object[] EMPTY_VECTOR = new Object[0];

	public static Object[] vector_push(Object x, Object[] a)
		{
		Object[] ret = new Object[a.Length+1];
		Array.Copy(a,0,ret,1,a.Length);
		ret[0] = x;
		return ret;
		}

	public static Object[] vector_rest(Object[] a)
		{
		Object[] ret = new Object[a.Length-1];
		Array.Copy(a,1,ret,0,a.Length-1);
		return ret;
		}

	public static Boolean containsNull(Object[] v)
		{
		foreach(Object o in v)
			{
			if(o == null)
				return true;
			}
		return false;
		}

	public static Boolean isTrue(Object o)
		{
		return o != null && ((o is Boolean)?(Boolean)o:true);
		}

	internal static void error(String message)
		{
		throw new Exception(message);
		}

	public static Object InvokeObject(Object f,params Object[] args)
		{
		Function func = f as Function;
		if(func != null)
			return func(args);

		IFunction ifunc = f as IFunction;
		if(ifunc != null)
			return ifunc.Invoke(args);


		else if(f is Type)
			{
			Type t = (Type)f;
			//try to support ctor-style init of Int32 etc. which don't have ctors
			//unfortunately, IntPtr IsPrimitive but has a ctor!
			if(t.IsPrimitive)
				return Convert.ChangeType(args[0],t);
			//CLSConstructor ctor = new CLSConstructor(t);
			return CLSConstructor.Invoke(t,args);
			}
		else if(args.Length == 1)
			{
			//treat as indexed get
			Array a = args[0] as Array;
			if(a != null)
				{
				if(f is Int32)
					return a.GetValue((Int32)f);
				else
					return a.GetValue((Int32[])f);
				}
			else	//not an array, try default member
				{
				if(f is Array)
					return CLSMember.GetDefaultIndexedProperty(args[0],(Object[])f);
				else
					return CLSMember.GetDefaultIndexedProperty(args[0],new Object[]{f});
				}
			}
		else if(args.Length == 2)
			{
			//treat as indexed set
			Object val = args[1];
			Object target = args[0];
			Array a = target as Array;
			if(a != null)
				{
				if(f is Int32)
					a.SetValue(val,(Int32)f);
				else
					a.SetValue(val,(Int32[])f);
				}
			else	//not an array, try default member
				{
				if(f is Array)
					{
					Array af = f as Array;

					Object[] subsandval = new Object[af.Length+1];
					Array.Copy(af,subsandval,af.Length);
					subsandval[af.Length] = args[1];
					CLSMember.SetDefaultIndexedProperty(target,subsandval);
					}
				else
					{
					CLSMember.SetDefaultIndexedProperty(target,new Object[]{f,val});
					}
				}
			return args[1];
			}
		//what should policy be on invoke nil?
		if(f == null)
			return null;
		throw new Exception("Don't know how to invoke: " + f);
		//+ " with args: " + Primitives.strgf.Invoke((Object)args));
		}
	}
}

//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.Text;

namespace DotLisp{

public class Cons : IEnumerable
	{
	public Cons(Object first,Cons rest)
		{
		this.first = first;
		this.rest = rest;
		}

	public Cons(Object first):this(first,null)
		{
		}

	public IEnumerator GetEnumerator()
		{
		return new ConsEnumerator(this);
		}

	public static Object First(Cons list)
		{
		return list.first;
		}

	public static Object Rest(Cons list)
		{
		return list.rest;
		}

	public static Object Second(Cons list)
		{
		return list.rest.first;
		}

	public static Object Third(Cons list)
		{
		return Second(list.rest);
		}

	public static Object Fourth(Cons list)
		{
		return Third(list.rest);
		}

	public static Cons Reverse(Cons list)
		{
		Cons result = null;
		while(list != null)
			{
			result = new Cons(list.first, result);
			list = list.rest;
			}
		return result;
		}

	public override int GetHashCode()
		{
		int head = (first == null) ? 17 : first.GetHashCode();
		int tail = (rest == null) ? 17 : rest.GetHashCode();
		return head + tail*37;
		}

	public override Boolean Equals(Object that)
		{
		if(this == that) return true;
		else return((that is Cons)
						&& equalsFirst((Cons)that)
						&& equalsRest((Cons)that));
		}

	private Boolean equalsFirst(Cons that)
		{
		return(first == null) ? that.first == null :
		first.Equals(that.first);
		}

	private Boolean equalsRest(Cons that)
		{
		return(rest == null) ? that.rest == null :
		rest.Equals(that.rest);
		}

	public static int Length(Cons list)
		{
		int len = 0;
		while(list != null)
			{
			len++;
			list = list.rest;
			}
		return len;
		}

	public static Cons Last(int num,Cons list)
		{
		int len = Length(list);
		if(num > len)
			return null;
		return NthTail(len-num,list);
		}

	public static Object Nth(int n,Cons list)
		{
		return NthTail(n,list).first;
		}

	public static Cons NthTail(int n,Cons list)
		{
		while(n > 0)
			{
			n--; 
			list = list.rest;
			}
		return list;
		}             

	public static Object[] ToVector(Cons list)
		{
		int len = Length(list);
		if(len == 0)
			return Util.EMPTY_VECTOR;
		else
			{
			Object[] result = new Object[len];
			for(int i = 0; list!=null; i++, list = list.rest)
				{
				result[i] = list.first;
				}
			return result;
			}
		}

	static public Object MakeList(params Object[] args)
		{
		Cons ret = null;
		for(int i = args.Length-1;i>=0;--i)
			{
			ret = new Cons(args[i],ret);
			}
		return ret;
		}

	public static Cons Append(Cons x, Cons y)
		{
		return(x != null) ? new Cons(x.first, Append(x.rest, y)) : y;
		}

	public override String ToString()
		{
		StringBuilder buf = new StringBuilder();
		buf.Append('(');
		buf.Append(first.ToString());
		Cons tail = rest;
		while(tail != null)
			{
			buf.Append(' ');
			buf.Append(tail.first.ToString());
			tail = tail.rest;
			}
		buf.Append(')');
		return buf.ToString();
		}

	internal Object first;
	internal Cons rest;
	}

public class ConsEnumerator: IEnumerator
	{
	public ConsEnumerator(Cons start)
		{
		this.start = start;
		this.current = null;
		this.next = start;
		}

	//IEnumerator implementation
	public Object Current
	{
		get
		{
			return current.first;
		}
	}

	public Boolean MoveNext()
		{
		current = next;
		if(current != null)
			next = current.rest;
		return current != null;
		}

	public void Reset()
		{
		this.current = null;
		this.next = start;
		}

	Cons start;
	Cons current;
	Cons next;
	}

}

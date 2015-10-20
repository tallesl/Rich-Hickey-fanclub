//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;

namespace DotLisp
{
public class FnEnumerator: IEnumerator
	{
	public FnEnumerator(IFunction getfn,IFunction movefn)
		{
		this.getfn = getfn;
		this.movefn = movefn;
		}
	//IEnumerator implementation
	public Object Current
	{
		get
		{
			return getfn.Invoke();
		}
	}

	public Boolean MoveNext()
		{
		return(Boolean)movefn.Invoke();
		}

	public void Reset()
		{
		throw new Exception("Reset() not supported");
		}

	IFunction getfn;
	IFunction movefn;
	}
}

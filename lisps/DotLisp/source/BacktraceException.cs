//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.Text;

namespace DotLisp
{
internal class BacktraceFrame
	{
	internal BacktraceFrame(Loc loc,Object f,Object args)
		{
		if(loc != null)
			{
			this.file = loc.file;
			this.line = loc.line;
			}
		else
			{
			this.file = "no file";
			this.line = 0;
			}
		this.f = f;
		this.args = args;
		}

	internal String file;
	internal Int32 line;
	internal Object f;
	internal Object args;
	}

public class BacktraceException:Exception
	{
	ArrayList frames = new ArrayList();

	internal static BacktraceException push(Exception inner,
														 BacktraceFrame frame,Interpreter interpreter)
		{
		if(inner is BacktraceException)
			{
			((BacktraceException)inner).frames.Add(frame);
			return(BacktraceException)inner;
			}
		return new BacktraceException(inner,frame,interpreter);
		}

	internal BacktraceException(Exception inner,
										 BacktraceFrame frame,Interpreter interpreter)
	:base(inner.Message,inner)
		{
		frames.Add(frame);
		this.interpreter = interpreter;
		}

	public override String ToString()
		{
		StringBuilder sb = new StringBuilder(InnerException.Message);
		sb.Append('\n');
		foreach(BacktraceFrame frame in frames)
			{
			sb.Append(frame.file + "(" + frame.line + "): ");
			sb.Append(frame.f==null?"":frame.f.ToString());
			sb.Append(" " + interpreter.Str(frame.args));
			sb.Append('\n');
			}
		return sb.ToString();
		}

	Interpreter interpreter;
	}
}

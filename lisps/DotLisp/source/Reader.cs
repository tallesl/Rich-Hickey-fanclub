//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Globalization;

namespace DotLisp
{
internal class LocTextReader
	{
	internal LocTextReader(String file, TextReader t)
		{
		this.file = file;
		this.t = t;
		}

	internal Int32 Read()
		{
		Int32 val = t.Read();
		if(val == '\n')
			++ line;
		return val;
		}

	internal Int32 Peek()
		{
		return t.Peek();
		}

	internal String file;   
	internal Int32 line = 1;
	TextReader t;
	}

internal class Loc
	{
	internal Loc(String file,Int32 line)
		{
		this.file = file;
		this.line = line;
		}

	internal String file;
	internal Int32 line;
	}  

internal class CompositeSymbol
	{
	internal CompositeSymbol(Cons symAsList)
		{
		this.symbolAsList = symAsList;
		}

	internal Cons symbolAsList;
	}

internal class ReaderMacro
	{
	internal ReaderMacro(Function func,Boolean isTerminating)
		{
		this.func = func;
		this.isTerminating = isTerminating;
		}

	internal Function func;
	internal Boolean isTerminating = true;
	}

internal class EndDelimiter
	{
	internal Char delim;
	}

internal class Eof
	{
	}

internal class Reader
	{
	static Eof EOF_MARKER = new Eof();
	public static Boolean eof(Object o)
		{
		return o == EOF_MARKER;
		}

	Hashtable macroTable = new Hashtable();

	internal Hashtable locTable = new Hashtable();	//Object->Loc

	internal Reader(Interpreter interpreter)
		{
		this.interpreter = interpreter;
		macroTable.Add('(',new ReaderMacro(new Function(ReadList),true));
		macroTable.Add(')',new ReaderMacro(new Function(ReadEndDelimiter),true));
		macroTable.Add('[',new ReaderMacro(new Function(ReadVector),true));
		macroTable.Add(']',new ReaderMacro(new Function(ReadEndDelimiter),true));
		macroTable.Add('"',new ReaderMacro(new Function(ReadString),true));
		macroTable.Add('\'',new ReaderMacro(new Function(ReadQuote),true));
		macroTable.Add('`',new ReaderMacro(new Function(ReadBackQuote),true));
		macroTable.Add('~',new ReaderMacro(new Function(ReadUnquote),true));
		//no func since read directly implements
		macroTable.Add(';',new ReaderMacro(null,true));
		macroTable.Add('#',new ReaderMacro(null,true));
		}

	internal object Read(LocTextReader t)
		{
		Object result = doRead(t,false);
		if(result is EndDelimiter)
			throw new Exception("Read error - read unmatched: " + ((EndDelimiter)result).delim);
		return result;
		}

	internal Object doRead(LocTextReader t,Boolean firstInForm)
		{
		Int32 ch = t.Read();
		while(Char.IsWhiteSpace((Char)ch))
			{
			ch = t.Read();
			}
		if(ch == -1)
			{
			//return null;
			//return Symbol.EOF;
			return EOF_MARKER;
			}
		if(ch == '#')
			{
			eatMultiComment(t);
			return doRead(t,firstInForm);
			}
		if(ch == ';')
			{
			eatComment(t);
			return doRead(t,firstInForm);
			}
		ReaderMacro rm = (ReaderMacro)macroTable[(Char)ch];
		if(rm != null)
			{
			return rm.func(t,(Char)ch);
			}
		else
			{
			return readSymbolOrNumber(t,ch,firstInForm);
			}
		}

	internal void eatComment(LocTextReader t)
		{
		Int32 ch = t.Peek();
		while(ch != -1 && ch != '\n' && ch != '\r')
			{
			t.Read();
			ch = t.Peek();
			}
		if(ch != -1 && ch != '\n' && ch != '\r')
			t.Read();
		}

	internal void eatMultiComment(LocTextReader t)
		{
		Int32 ch = t.Peek();
		while(ch != -1 && ch != '#')
			{
			t.Read();
			ch = t.Peek();
			}
		if(ch == '#')
			t.Read();
		}

	internal Object readSymbolOrNumber(LocTextReader t,Int32 ch,Boolean firstInForm)
		{
		StringBuilder b = new StringBuilder();
		b.Append((Char)ch);
		Boolean complete = false;
		while(!complete)
			{
			ch = t.Peek();
			if(ch == -1)
				complete = true;
			else if(Char.IsWhiteSpace((Char)ch))
				complete = true;
			else
				{
				ReaderMacro rm = (ReaderMacro)macroTable[(Char)ch];
				if(rm != null && rm.isTerminating)
					complete = true;
				else
					{
					t.Read();
					b.Append((Char)ch);
					}
				}
			}
		return parseSymbolOrNumber(b.ToString(),firstInForm);
		}

	internal Object parseSymbolOrNumber(String s,Boolean firstInForm)
		{
		//treat nil as a constant
		if(s.Equals("nil"))
			return null;
		Double d;
		if(Double.TryParse(s,NumberStyles.Integer,NumberFormatInfo.CurrentInfo,out d))
			{
			return(Int32)d;
			}
		else if(Double.TryParse(s,NumberStyles.Float,NumberFormatInfo.CurrentInfo,out d))
			{
			return d;
			}
		else
			{
			Object o = splitSymbol(s);
			if(o is Cons && firstInForm)
				{
				return new CompositeSymbol((Cons)o);
				}
			return o;
			}
		}

	internal Object splitSymbol(String s)
		{
		//turn x[y] into ([y] x) - can we with readvector in place?
		//Int32 bridx = s.LastIndexOf("[");


		Int32 dotidx = s.LastIndexOf(".");
		Int32 colonidx = s.LastIndexOf(":");
		//dot in the middle and not member(dot at start) or type(dot at end)
		if(dotidx >= 0 && dotidx > colonidx && dotidx < (s.Length-1) && s[0] != '.')
			{
			//turn x.y into (.y x)

			return Cons.MakeList(interpreter.intern(s.Substring(dotidx)),
										splitSymbol(s.Substring(0,dotidx)));
			}
		return interpreter.intern(s);
		}

	internal Cons readDelimitedList(LocTextReader t,Int32 delim)
		{
		Cons ret = null;
		Cons tail = null;

		Int32 ch = t.Peek();
		while(Char.IsWhiteSpace((Char)ch))
			{
			t.Read();
			ch = t.Peek();
			}
		while(ch != delim)
			{
			Object o = doRead(t,delim == ')' && ret == null);
			if(eof(o))
				{
				throw new Exception("Read error - eof found before matching: " 
										  + (Char)delim + "\n File: " + t.file + ", line: " + t.line);
				}
			EndDelimiter ed = o as EndDelimiter;
			if(ed != null)
				{
				if(ed.delim == delim)
					{
					return ret;
					}
				else
					throw	new Exception("Read error - read unmatched: " + ed.delim
											  + "\n File: " + t.file + ", line: " + t.line);
				}
			Cons link = new Cons(o,null);
			if(delim == ')' && ret == null && o is CompositeSymbol)
				{
				ret = ((CompositeSymbol)o).symbolAsList;
				tail = ret.rest;
				}
			else if(ret == null)
				{
				ret = tail = link; 
				}
			else
				{
				tail.rest = link;
				tail = link;
				}
			ch = t.Peek();
			while(Char.IsWhiteSpace((Char)ch))
				{
				t.Read();
				ch = t.Peek();
				}
			}

		//eat delim
		t.Read();
		return ret;
		}

	internal Object ReadQuote(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		Object ret = Cons.MakeList(interpreter.QUOTE,doRead(t,false));    
		//record the location
		locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	internal Object ReadBackQuote(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		Object ret = Cons.MakeList(interpreter.BACKQUOTE,doRead(t,false));      
		//record the location
		locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	internal Object ReadUnquote(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		Int32 ch = t.Peek();
		Object ret = null;
		if(ch == '@')
			{
			t.Read();
			ret = Cons.MakeList(interpreter.UNQUOTE_SPLICING,doRead(t,false));      
			}
		else
			ret = Cons.MakeList(interpreter.UNQUOTE,doRead(t,false));      
		//record the location
		locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	internal Object ReadList(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		Object ret = readDelimitedList(t,')');
		//record the location
		if(ret != null)
			locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	internal Object ReadVector(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		Cons largs = readDelimitedList(t,']');
		Object ret = new Cons(interpreter.VECTOR,largs);
		//record the location
		locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	internal Object ReadEndDelimiter(params Object[] args)
		{
		LocTextReader t = (LocTextReader)args[0];
		//so we can complain
		EndDelimiter ed = new EndDelimiter();
		//Char c = (Char)args[1];//t.Read();
		ed.delim = (Char)args[1];//t.Read();
		return ed;
		//throw new Exception("Read error - read unmatched: " + c);
		}

	internal Object ReadString(params Object[] args)
		{
		StringBuilder b = new StringBuilder();
		LocTextReader t = (LocTextReader)args[0];
		Int32 line = t.line;
		//eat the double-quote
		//t.Read();
		//Int32 ch = t.Peek();
		Int32 ch = t.Read();
		while(ch != '"')
			{
			if(ch == -1)
				{
				throw new Exception("Read error - eof found before matching: \""
										  + "\n File: " + t.file + ", line: " + t.line);
				}
			//eat it
			//t.Read();
			if(ch == '\\')	//escape
				{
				ch = t.Read();
				if(ch == -1)
					{
					throw new Exception("Read error - eof found before matching: \""
											  + "\n File: " + t.file + ", line: " + t.line);
					}
				switch(ch)
					{
					case 't':
						ch = '\t';
						break;
					case 'r':
						ch = '\r';
						break;
					case 'n':
						ch = '\n';
						break;
					case '\\':
						break;
					case '"':
						break;
					default:
						throw new Exception("Unsupported escape character: \\" + (Char)ch
												  + "\n File: " + t.file + ", line: " + t.line);
					}
				}
			b.Append((Char)ch);
			//ch = t.Peek();
			ch = t.Read();
			}
		//eat the trailing quote
		//t.Read();
		Object ret = b.ToString();
		//record the location
		locTable[ret] = new Loc(t.file,line);
		return ret;
		}

	Interpreter interpreter;
	}
}

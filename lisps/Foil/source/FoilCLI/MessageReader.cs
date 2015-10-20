/*
 * Created on Dec 7, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
using System;
using System.Collections;
using System.IO;
using System.Text;

/**
 * @author Eric Thorsen
 *
 */
namespace com.richhickey.foil
{
	public class MessageReader :IReader
	{
		IReferenceManager	referenceManager;
		IReflector			reflector;

		public	MessageReader(IReferenceManager referenceManager,IReflector reflector)
		{
			this.referenceManager	=	referenceManager;
			this.reflector			=	reflector;
		}

		public ArrayList readMessage(TextReader strm) 
		{
			return readSexpr(strm);
		}

		public ArrayList readSexpr(TextReader strm)
		{
			return readDelimitedList(strm,'(',')');
		}

		public ArrayList readDelimitedList(TextReader strm, int startc, int endc)
		{
	    
			int c = strm.Read();
			while (Char.IsWhiteSpace((char) c))
				c = strm.Read();
        
			if (c != startc)
				throw new IOException(String.Format("expected list to begin with; {0}",(char)startc));
			ArrayList ret = new ArrayList();
			for (c=strm.Peek();c!=-1;c=strm.Peek())
			{
				if (Char.IsWhiteSpace((char) c))
				{
					strm.Read();
					continue;
				}
				if(c==-1)
					throw new IOException("unexpected EOF");
				if(c==endc)
				{
					strm.Read();
					return	ret;
				}
				switch ((char)c)
				{
					case '(':
						ret.Add(readSexpr(strm));
						break;
					case '"':
						ret.Add(readString(strm));
						break;
					case '#':
						ret.Add(readMacro(strm));
						break;
					default:
						ret.Add(readToken(strm));
						break;
				}
			}
			return ret;
		}

		Object readMacro(TextReader strm)
			{
			int c = strm.Read();
			if (c != '#')
				throw new IOException("expected macro to begin with '#'");
			c = strm.Peek();
			if(c == '{')
				{
				return processMacroList(readDelimitedList(strm,'{','}'));
				}
			else if(c == '}')	// }id
				{
				strm.Read();
				Object	id = Convert.ToInt32(readToken(strm));
				return referenceManager.getObjectForId(id);
				}
			else if(c == '\\')
				{
				strm.Read();
				c = strm.Read();
				return Convert.ToChar(c);
				}
			else
				throw new Exception("unsupported macro sequence");
			}

		Object processMacroList(ArrayList args)
		{
	    if(RuntimeServer.isMessage(":box",args))
	        {
	        return Reflector.numericConvert(RuntimeServer.typeArg(args[1]),args[2]);
	        }
	    else if(RuntimeServer.isMessage(":vector",args))
	        {
	        return reflector.createVector(	RuntimeServer.typeArg(args[1])
											,args.Count-2
											,args.GetRange(2,args.Count));
	        }
       throw new Exception("unsupported macro sequence");
		}

		static protected String readString(TextReader strm)
		{
			int c = strm.Read();
			if (c != '"')
				throw new IOException("expected string to begin with '\"'");
			StringBuilder sb = new StringBuilder();
			Boolean end = false;
			while (!end)
			{
				c = strm.Read();
				if (c == -1)
					throw new IOException("unexpected EOF");
				if (c == '"')
					end = true;
				else if (c == '\\')
				{
					c = strm.Read();
					if (c == -1)
						throw new IOException("unexpected EOF");
					if (c == '"' || c == '\\')
						sb.Append((char) c);
					else
						throw new IOException(String.Format("unsupported escape character: '{0}'"
															,(char) c));
				}
				else
					sb.Append((char) c);
			}
			return sb.ToString();
		}

		
		protected Object readToken(TextReader strm) 
		{
			StringBuilder sb	= new StringBuilder();
			int	c	=	strm.Peek();
			for (;c!=-1;c=strm.Peek())
			{
				if (c == -1)
					throw new IOException("unexpected EOF");
				if (
					c == ')' 
					|| c == '(' 
					|| Char.IsWhiteSpace((char) c)
					|| c == '}'
					)
					break;
				else
					sb.Append((char) strm.Read());
			}
			String ret = sb.ToString();
			if(isInteger(ret))
				return Convert.ToInt32(ret);
			else if(shouldBeNumber(ret))
				return Convert.ToDouble(ret);
			else if(String.Compare(ret,"nil",true)==0)
				return null;
			else if(String.Compare(ret,"t",true)==0)
				return true;
			return ret;
		}

    	static Boolean isInteger(String s)
		{
			bool ret = true;
			for(int i=0;ret && i<s.Length;++i)
			{
				if(i == 0)
					ret = Char.IsDigit(s[i]) || s[i] == '-';
				else
					ret = Char.IsDigit(s[i]);
			}
			return ret;
		}

		static Boolean shouldBeNumber(String s)
		{
			char c = s[0];
			return Char.IsDigit(c) || c == '.' || c == '-';
		}
	}
}
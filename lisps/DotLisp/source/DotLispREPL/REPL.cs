//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt

using System;
using DotLisp;

namespace DotLispREPL
{
	class REPL
	{
	[STAThread]
	static void Main(string[] args)
		{
		Interpreter interpreter = new Interpreter();
		try{
			foreach(String path in args)
				{
				Console.WriteLine(interpreter.LoadFile(path).ToString());
				}
			}
		catch(Exception e)
			{
			Console.Error.WriteLine(e.ToString());
			}
		for(;;)
			{
			try{
				Console.Write("> ");
				Object r = null;
				try{
					r = interpreter.Read("console",Console.In);
					if(interpreter.Eof(r))
						return;
					Console.In.ReadLine();	//eat the newline
					}
				catch
					{
					Console.In.ReadLine();	//eat the rest of the line
					throw;
					}
				Object x = interpreter.Eval(r);
				Console.WriteLine(interpreter.Str(x));
				}
			catch(Exception e)
				{
				Console.WriteLine("!Exception: " + e.GetBaseException().Message);
				}
			}
		}
	}
}

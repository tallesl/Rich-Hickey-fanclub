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
 * 
 * 
 */
using System;
using System.IO;
using System.Text;
using System.Collections;
using System.Reflection;
using System.Net;
using System.Net.Sockets;
using System.Data.SqlClient;
// These may need to reside in a separate init file?
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Configuration;
using System.Threading;

/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for RuntimeServer.
	/// </summary>
	public class RuntimeServer:IRuntimeServer
	{
		    
		static	System.Drawing.Region	r						=	null;
		static	System.Windows.Forms.Form	f					=	null;
		static	System.ComponentModel.AmbientValueAttribute	a	=	null;
		static	System.Configuration.AppSettingsReader	ar		=	null;

    IReader				reader;
    IBaseMarshaller		marshaller;
    IReferenceManager	referenceManager;
    IReflector			reflector;

	[ThreadStatic]
	Object		proxyWriter	=null;
	[ThreadStatic]
	Object		proxyReader	=null;

    public RuntimeServer(	IReader reader
							, IBaseMarshaller marshaller
							, IReferenceManager referenceManager
							, IReflector reflector
						)
        {
        this.reader				= reader;
        this.marshaller			= marshaller;
        this.referenceManager	= referenceManager;
        this.reflector			= reflector;
        }

    /*
     * (non-Javadoc)
     * 
     * @see com.richhickey.foil.IRuntimeServer#processMessages()
     */
public Object processMessages(TextReader ins,TextWriter outs) 
	{
//	lock(this)
	{
		proxyReader	=	ins;
		proxyWriter	=	outs;

		for(;;)
		{
			String	resultMessage	=	null;
			String	errorMesssage	=	null;
			String	sin				=	null;
			ArrayList message		=	null;
			try
			{
				sin			=	slurpForm(ins);
				message		=	reader.readMessage(new StringReader(sin));
				if(isMessage(":call",message))
					//(:call cref marshall-flags marshall-value-depth-limit args ...)
				{
					ICallable c = (ICallable)message[1];
					int marshallFlags = intArg(message[2]);
					int marshallDepth = intArg(message[3]);
					Object ret = c.invoke(message[4],message.GetRange(5,message.Count-5));
					resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
				}
				else if(isMessage(":cref",message))
					//(:cref member-type tref|"packageQualifiedTypeName" "memberName")
				{
					int memberType = intArg(message[1]);
					Type c = typeArg(message[2]);
					String memberName = stringArg(message[3]);
					ICallable ret = reflector.getCallable(memberType,c,memberName);
					resultMessage = createRetString(ret,marshaller,IBaseMarshallerFlags.MARSHALL_ID,0);
				}
				else if(isMessage(":new",message))
					//(:new tref marshall-flags marshall-value-depth-limit (args ...) property-inits ...)
				{
					Type c = typeArg(message[1]);
					int marshallFlags = intArg(message[2]);
					int marshallDepth = intArg(message[3]);
					ArrayList args = (ArrayList)message[4];
					Object ret = reflector.createNew(c,args);
					//set props
					if(message.Count>5)
					{
						reflector.setProps(ret,message.GetRange(5,message.Count-5));
					}
					resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
				}
				else if(isMessage(":tref",message))
					//(:tref "packageQualifiedTypeName")
				{
					Type c =typeArg(message[1]);
					resultMessage = createRetString(c,marshaller,IBaseMarshallerFlags.MARSHALL_ID,1);
				}
				else if(isMessage(":free",message))
					//(:free refid ...)
				{
					for(int i=1;i<message.Count;i++)
					{
						Object	id	=	message[i++];
						int rev 	= 	intArg(message[i]);
						referenceManager.free(id,rev);
					}
					resultMessage = createRetString(null,marshaller,0,0);
				}
				else if(isMessage(":ret",message))
					//only on callback, note will break out of message loop
				{
					return	message[1];
				}
				else if(isMessage(":str",message))
					//(:str refid)
				{
					resultMessage = createRetString(message[1].ToString(),marshaller,0,0);
				}
				else if(isMessage(":equals",message))
					//(:equals ref1 ref2)
				{
					Object o1 = message[1];
					Object o2 = message[2];
					Boolean ret = (o1 == null) ? (o2 == null) : o1.Equals(o2);
					resultMessage = createRetString(ret?true:false,marshaller,0,0);
				}
				else if(isMessage(":vector",message))
				{
					//(:vector tref|"packageQualifiedTypeName" length value ...)			    {
					Type c = typeArg(message[1]);
					int length = intArg(message[2]);
					Object ret = reflector.createVector(c
						,length
						,message.GetRange(3,message.Count-3)
						);
					resultMessage = createRetString(ret,marshaller,IBaseMarshallerFlags.MARSHALL_ID,0);
				}
				else if(isMessage(":vget",message))
					//(:vget aref marshall-flags marshall-value-depth-limit index)
				{
					int marshallFlags	= intArg(message[2]);
					int marshallDepth	= intArg(message[3]);
					int index			= intArg(message[4]);
					Object ret			= reflector.vectorGet(message[1],index);
					resultMessage		= createRetString(ret,marshaller,marshallFlags,marshallDepth);
				}
				else if(isMessage(":vset",message))
					//(:vset aref index value)
				{
					int index = intArg(message[2]);
					reflector.vectorSet(message[1],index,message[3]);
					resultMessage = createRetString(null,marshaller,0,0);
				}
					//Eric - For .net indexers
				else if(isMessage(":iget",message))
					//(:iget indexable-object-ref marshall-flags marshall-value-depth-limit indexes...)
				{
					int marshallFlags	= intArg(message[2]);
					int marshallDepth	= intArg(message[3]);
					Object ret			= reflector.indexerGet(message[1],message.GetRange(4,message.Count-4));
					resultMessage		= createRetString(ret,marshaller,marshallFlags,marshallDepth);
				}
				else if(isMessage(":iset",message))
					//(:iset indexable-object-ref indexes... value)
				{
					reflector.indexerSet(message[1],message.GetRange(2,message.Count-2));
					resultMessage = createRetString(null,marshaller,0,0);
				}
				else if(isMessage(":vlen",message))
					//(:vlen aref)
				{
					Object ret = reflector.vectorLength(message[1]);
					resultMessage = createRetString(ret,marshaller,0,0);
				}
				else if(isMessage(":bases",message))
					//(:bases tref|"packageQualifiedTypeName")
				{
					Type c = typeArg(message[1]);
					StringWriter sw = new StringWriter();
					sw.Write("(:ret");
					marshaller.marshallAsList(reflector.bases(c),sw,IBaseMarshallerFlags.MARSHALL_ID,1);
					sw.Write(')');
					resultMessage = sw.ToString(); 
				}
				else if(isMessage(":type-of",message))
					//(:type-of ref)
				{
					Type c = message[1].GetType();
					resultMessage = createRetString(c,marshaller,IBaseMarshallerFlags.MARSHALL_ID,1);
				}
				else if(isMessage(":is-a",message))
					//(:is-a ref tref|"packageQualifiedTypeName")
				{
					Object o = message[1];
					Type c = typeArg(message[2]);
					resultMessage = createRetString(c.IsInstanceOfType(o)?true:false,marshaller,0,0);
				}
				else if(isMessage(":hash",message))
					//(:hash refid)
				{
					resultMessage = createRetString(message[1].GetHashCode(),marshaller,0,0);
				}
				else if(isMessage(":members",message))
					//(:members :tref|"packageQualifiedTypeName")
				{
					Type c = typeArg(message[1]);
					StringWriter sw = new StringWriter();
					sw.Write("(:ret");
					reflector.members(c,sw);
					sw.Write(')');
					resultMessage = sw.ToString(); 
				}
				else if(isMessage(":marshall",message))
					//(:marshall ref marshall-flags marshall-value-depth-limit)
				{
					Object ret = message[1];
					int marshallFlags = intArg(message[2]);
					int marshallDepth = intArg(message[3]);
					resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
				}
				else if(isMessage(":proxy",message))
					//(:proxy marshall-flags marshall-value-depth-limit interface-trefs ...)
				{
					int marshallFlags = intArg(message[1]);
					int marshallDepth = intArg(message[2]);
					resultMessage = createRetString(reflector.makeProxy(this,marshallFlags,marshallDepth
																		,message.GetRange(3,message.Count-3))
																		,marshaller,IBaseMarshallerFlags.MARSHALL_ID,0);
				}
				else if(isMessage(":err",message))
					//only on callback, note will break out of message loop
				{
					//there was an error on the Lisp side during a proxy callback
					//will turn into an exception below
					errorMesssage = (String)message[1];
				}
				else if(isMessage(":cnames",message))
				{
					StringWriter sw = new StringWriter();
					sw.Write("(:ret");
					marshaller.marshallAsList(
							reflector.getClassNames(message[1],message.GetRange(2,message.Count-2))
							,sw,IBaseMarshallerFlags.MARSHALL_ID,1);
					sw.Write(')');
					resultMessage = sw.ToString(); 
				}
				else
				{
					throw new Exception("unsupported message");
				}
			}
			catch(Exception ex)
			{
				if(ex is IOException)
					throw (IOException)ex;
				else if(ex is TargetInvocationException )
				{
					while(ex.InnerException != null && ex is TargetInvocationException)
						ex = ex.InnerException;
				}
				outs.Write("(:err");
				StringBuilder sb	=	new StringBuilder();
				sb.Append(ex.Message);
				Exception	ie	=	ex.InnerException;
				while(ie!=null)
				{
					sb.AppendFormat(" {0}",ie.Message);
					ie	=	ie.InnerException;
				}
				Console.WriteLine("Thread:{0} - Exception:{1}",Thread.CurrentThread.Name,sb.ToString());
				marshaller.marshallAtom(sb.ToString(),outs,0,0);
				marshaller.marshallAtom(String.Format("{0}{1}",sb.ToString(),ex.StackTrace),outs,0,0);
				outs.Write(')');
				outs.Write('\n');
				outs.Flush();
			}
			if(resultMessage != null)
			{
				outs.Write(resultMessage);
				outs.Write('\n');
				outs.Flush();
			}
			else if (errorMesssage != null)
			{
				//there was an error on the Lisp side during a proxy callback
				//throw an exception to the calling code
				throw new IOException(errorMesssage);
			}
		}
	}	
}

	public Object proxyCall(int marshallFlags, int marshallDepth, MethodInfo method, Object proxy, Object[] args) 
	{
		TextReader reader = (TextReader)proxyReader;
		TextWriter writer = (TextWriter)proxyWriter;
		//form the call message:
		//(:proxy-call method-symbol proxy-ref args ...)
		//method-symbol has the form: |package.name|::classname.methodname
		String decl = method.DeclaringType.FullName;
		StringWriter sw = new StringWriter();
		sw.Write("(:proxy-call |");
		//ET deal with delegate calls
		//if(proxy is Delegate)
		//	sw.Write("System|::Delegate.DynamicInvoke");
		//else
		{
			int dotInx	=	decl.LastIndexOf('.');
			sw.Write(decl.Substring(0,dotInx));
			sw.Write("|::");
			sw.Write(decl.Substring(dotInx+1));
			sw.Write('.');
			sw.Write(method.Name);
		}
		marshaller.marshallAtom(proxy,sw,IBaseMarshallerFlags.MARSHALL_ID,0);
		
		for(int i=0;i<args.Length;i++)
			marshaller.marshallAtom(args[i],sw,marshallFlags,marshallDepth);
		
		sw.Write(')');
		
		writer.Write(sw.ToString());
		writer.Write('\n');
		writer.Flush();
		
		return processMessages(reader,writer);
	}

		static String slurpForm(TextReader strm) 
			{
			StringWriter sw = new StringWriter();
		
			while(strm.Read() != '(')
				;
			int parenCount = 1;
			sw.Write('(');
			Boolean inString = false;
			Boolean escape = false;
			do
			{
			int c = strm.Read();
			if(c == '(')
				{
				if(!inString)
					++parenCount;
				}
			else if(c == ')')
				{
				if(!inString)
					--parenCount;
				}
			else if(c == '"')
				{
				if(!escape)
					inString = !inString;
				}		
			if(inString && c == '\\')
				escape = true;
			else
				escape = false;
			sw.Write((Char)c);
			} while(parenCount > 0);		
			return sw.ToString();
			}

	internal	static	String stringArg(Object o)
	    {
	    return (String)o;
	    }
	
	internal	static	int intArg(Object o)
	    {
	    return Convert.ToInt32(o);
	    }
	
	internal	static	Type typeArg(Object arg) 
	    {
	    if(arg is Type)
	        return (Type)arg;
	    else if (arg is String)
	        {
	        Type t =	Type.GetType((String)arg);
			return	typeArgFromAssemblies((String)arg);
	        }
	    else
	        throw new Exception("expecting type arg, either reference or packageQualifiedName string");
	    }

		internal	static	Type	typeArgFromAssemblies(String obj)
		{
			Assembly[] asms	=	System.AppDomain.CurrentDomain.GetAssemblies();
			foreach(Assembly a in asms)
			{
				Type t	=	a.GetType(obj);
				if(t!=null)
					return	t;
			}
			return	null;
		}
	
	internal	static	Boolean isMessage(String type,ArrayList message)
	    {
	    return String.Compare(type,(String)message[0],true)==0;
	    }
	
	String createRetString(Object o,IBaseMarshaller marshaller,int flags,int depth) 
	    {
		StringWriter sw = new StringWriter();
		sw.Write("(:ret");
		marshaller.marshallAtom(o,sw,flags,depth);
		sw.Write(')');
		return sw.ToString();
	    }
	}
	/// <summary>
	/// Class to support muliple channels for the single server.
	/// </summary>
	public class RuntimeSocketServer 
	{
		RuntimeServer	rs;
		Int32			port;

		public	RuntimeSocketServer(RuntimeServer	rs,Int32 port)
		{
			this.rs		=	rs;
			this.port	=	port;
		}

		public void processMessagesOnSocket() 
		{
			try 
			{
				TcpListener	me		=	new TcpListener(IPAddress.Any,this.port);
				me.Start();
				TcpClient	tcp		=	me.AcceptTcpClient();		
				//s.setTcpNoDelay(true);
				 rs.processMessages(	new StreamReader(tcp.GetStream()),
					new StreamWriter(tcp.GetStream()));
			} 
			catch(Exception exc)
			{
				Console.WriteLine("Exception:{0}",exc.Message);
				Console.WriteLine(exc.StackTrace);
			}
		}
	}
}

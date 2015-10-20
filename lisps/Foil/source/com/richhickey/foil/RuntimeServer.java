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
package com.richhickey.foil;

import java.io.*;
import java.lang.reflect.Method;
import java.net.*;
import java.util.List;


/**
 * @author Rich
 * 
 */
public class RuntimeServer implements IRuntimeServer
    {

    
    IReader reader;

    IBaseMarshaller marshaller;

    IReferenceManager referenceManager;
    
    IReflector reflector;
    
    ThreadLocal proxyWriter;
    ThreadLocal proxyReader;

    public RuntimeServer(IReader reader, IBaseMarshaller marshaller,
            IReferenceManager referenceManager, IReflector reflector)
        {
    	this.proxyReader = new ThreadLocal();
    	this.proxyWriter = new ThreadLocal();
        this.reader = reader;
        this.marshaller = marshaller;
        this.referenceManager = referenceManager;
        this.reflector = reflector;
        }



public Object processMessages(Reader ins,Writer outs) throws IOException
	{
	//on this thread the main streams are also the proxy streams
	proxyReader.set(ins);
	proxyWriter.set(outs);
	
	for(;;)
	{
	    String resultMessage = null;
	    String errorMesssage = null;
		try{
			String form = slurpForm(ins);
			List message = reader.readMessage(new StringReader(form));
			//List message = reader.readMessage(ins);
			if(isMessage(":call",message))
			    //(:call cref marshall-flags marshall-value-depth-limit args ...)
			    {
				ICallable c = (ICallable)message.get(1);
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				Object ret = c.invoke(message.get(4),message.subList(5,message.size()));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":cref",message))
			    //(:cref member-type tref|"packageQualifiedTypeName" "memberName")
			    {
			    int memberType = intArg(message.get(1));
			    Class c = typeArg(message.get(2));
			    String memberName = stringArg(message.get(3));
			    ICallable ret = reflector.getCallable(memberType,c,memberName);
				resultMessage = createRetString(ret,marshaller,IBaseMarshaller.MARSHALL_ID,0);
			    }
			else if(isMessage(":new",message))
				//(:new tref marshall-flags marshall-value-depth-limit (args ...) property-inits ...)
			    {
			    Class c = typeArg(message.get(1));
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				List args = (List)message.get(4);
			    Object ret = reflector.createNew(c,args);
			    //set props
			    if(message.size()>5)
			        {
			        reflector.setProps(ret,message.subList(5,message.size()));
			        }
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":tref",message))
			    //(:tref "packageQualifiedTypeName")
			    {
			    Class c = Class.forName((String)message.get(1));
				resultMessage = createRetString(c,marshaller,IBaseMarshaller.MARSHALL_ID,1);
			    }
			else if(isMessage(":free",message))
			    //(:free refid refrev ... ...)
			    {
			    for(int i=1;i<message.size();i++)
			        {
			        Object id = message.get(i++);
			        int rev = intArg(message.get(i));
			        referenceManager.free(id,rev);
			        }
				resultMessage = createRetString(null,marshaller,0,0);
			    }
			else if(isMessage(":ret",message))
				//only on callback, note will break out of message loop
				{
				return message.get(1);
				}
			else if(isMessage(":str",message))
			    //(:str refid)
			    {
				resultMessage = createRetString(message.get(1).toString(),marshaller,0,0);
			    }
			else if(isMessage(":equals",message))
			    //(:equals ref1 ref2)
			    {
			    Object o1 = message.get(1);
			    Object o2 = message.get(2);
			    boolean ret = (o1 == null) ? (o2 == null) : o1.equals(o2);
				resultMessage = createRetString(ret?Boolean.TRUE:Boolean.FALSE,marshaller,0,0);
			    }
			else if(isMessage(":vector",message))
			    {
			    //(:vector tref|"packageQualifiedTypeName" length value ...)			    {
			    Class c = typeArg(message.get(1));
				int length = intArg(message.get(2));
				Object ret = reflector.createVector(c,length,
    					message.subList(3,message.size()));
				resultMessage = createRetString(ret,marshaller,IBaseMarshaller.MARSHALL_ID,0);
			    }
			else if(isMessage(":vget",message))
			    //(:vget aref marshall-flags marshall-value-depth-limit index)
			    {
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				int index = intArg(message.get(4));
				Object ret = reflector.vectorGet(message.get(1),index);
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":vset",message))
			    //(:vset aref index value)
			    {
				int index = intArg(message.get(2));
				reflector.vectorSet(message.get(1),index,message.get(3));
				resultMessage = createRetString(null,marshaller,0,0);
			    }
			else if(isMessage(":vlen",message))
			    //(:vlen aref)
			    {
				Object ret = reflector.vectorLength(message.get(1));
				resultMessage = createRetString(ret,marshaller,0,0);
			    }
			else if(isMessage(":bases",message))
			    //(:bases tref|"packageQualifiedTypeName")
			    {
			    Class c = typeArg(message.get(1));
				StringWriter sw = new StringWriter();
				sw.write("(:ret");
				marshaller.marshallAsList(reflector.bases(c),sw,IBaseMarshaller.MARSHALL_ID,1);
				sw.write(')');
				resultMessage = sw.toString(); 
			    }
			else if(isMessage(":type-of",message))
			    //(:type-of ref)
			    {
			    Class c = message.get(1).getClass();
				resultMessage = createRetString(c,marshaller,IBaseMarshaller.MARSHALL_ID,1);
			    }
			else if(isMessage(":is-a",message))
			    //(:is-a ref tref|"packageQualifiedTypeName")
			    {
			    Object o = message.get(1);
			    Class c = typeArg(message.get(2));
				resultMessage = createRetString(c.isInstance(o)?Boolean.TRUE:Boolean.FALSE,marshaller,0,0);
			    }
			else if(isMessage(":hash",message))
			    //(:hash refid)
			    {
				resultMessage = createRetString(new Integer(message.get(1).hashCode()),marshaller,0,0);
			    }
			else if(isMessage(":members",message))
			    //(:members :tref|"packageQualifiedTypeName")
			    {
			    Class c = typeArg(message.get(1));
				StringWriter sw = new StringWriter();
				sw.write("(:ret");
				reflector.members(c,sw);
				sw.write(')');
				resultMessage = sw.toString(); 
			    }
			else if(isMessage(":marshall",message))
			    //(:marshall ref marshall-flags marshall-value-depth-limit)
			    {
			    Object ret = message.get(1);
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
//			    IMarshaller m = marshaller.findMarshallerFor(ret.getClass());
//				StringWriter sw = new StringWriter();
//				sw.write("(:ret");
//				m.marshall(ret,sw,marshaller,marshallFlags,marshallDepth);
//				sw.write(')');
//				resultMessage = sw.toString(); 
			    }
			else if(isMessage(":proxy",message))
			//(:proxy marshall-flags marshall-value-depth-limit interface-trefs ...)
				{
				int marshallFlags = intArg(message.get(1));
				int marshallDepth = intArg(message.get(2));
				resultMessage = createRetString(reflector.makeProxy(this,marshallFlags,marshallDepth,
										message.subList(3,message.size())),marshaller,IBaseMarshaller.MARSHALL_ID,0);
				}
			else if(isMessage(":err",message))
				//only on callback, note will break out of message loop
				{
				//there was an error on the Lisp side during a proxy callback
				//will turn into an exception below
				errorMesssage = (String)message.get(1);
				}
			else if(isMessage(":cnames",message))
				//(:cnames jarfilename packagename ...)
				{
				StringWriter sw = new StringWriter();
				sw.write("(:ret");
				marshaller.marshallAsList(
						reflector.getClassNames((String)message.get(1),message.subList(2,message.size()))
						,sw,IBaseMarshaller.MARSHALL_ID,1);
				sw.write(')');
				resultMessage = sw.toString(); 
				}			
			else
			    {
			    throw new Exception("unsupported message");
			    }
			}
		catch(Throwable ex)
			{
		    String message;
		    String trace;
		    if(ex instanceof SocketException)
		        throw (IOException)ex;
		    
			while(ex.getCause() != null)
		        {
			    ex = ex.getCause();
		        }

		    outs.write("(:err");
			marshaller.marshallAtom(ex.toString(),outs,0,0);
			StringWriter sw = new StringWriter();
			
			//marshaller.marshallAsList(ex.getStackTrace(),outs,0,1);
			ex.printStackTrace(new PrintWriter(sw));
			marshaller.marshallAtom(sw.toString(),outs,0,0);
			outs.write(')');
			outs.write('\n');
			outs.flush();
			}

		if(resultMessage != null)
		    {
		    outs.write(resultMessage);
			outs.write('\n');
			outs.flush();
		    }
		else if (errorMesssage != null)
			{
			//there was an error on the Lisp side during a proxy callback
			//throw an exception to the calling code
			throw new IOException(errorMesssage);
			}
		}
	//return null;
	}

	String stringArg(Object o)
	    {
	    return (String)o;
	    }
	
	int intArg(Object o)
	    {
	    return ((Number)o).intValue();
	    }
	
	static Class typeArg(Object arg) throws Exception
	    {
	    if(arg instanceof Class)
	        return (Class)arg;
	    else if (arg instanceof String)
	        {
	        String tname = (String)arg;
	        if(tname.equalsIgnoreCase(":int"))
	            return int.class;
	        else if(tname.equalsIgnoreCase(":double"))
	            return double.class;
	        else if(tname.equalsIgnoreCase(":long"))
	            return long.class;
	        else if(tname.equalsIgnoreCase(":float"))
	            return float.class;
	        else if(tname.equalsIgnoreCase(":char"))
	            return char.class;
	        else if(tname.equalsIgnoreCase(":boolean"))
	            return boolean.class;
	        else if(tname.equalsIgnoreCase(":short"))
	            return short.class;
	        else if(tname.equalsIgnoreCase(":byte"))
	            return byte.class;
	        else
	            return Class.forName(tname);
	        }
	    else
	        throw new Exception("expecting type arg, either reference or packageQualifiedName string");
	    }
	
	static String slurpForm(Reader strm) throws IOException
		{
		StringWriter sw = new StringWriter();
		
		while(strm.read() != '(')
			;
		int parenCount = 1;
		sw.write('(');
		boolean inString = false;
		boolean escape = false;
		do{
			int c = strm.read();
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
			
			sw.write(c);
		}while(parenCount > 0);
		
		return sw.toString();
		}
	
	static boolean isMessage(String type,List message)
	    {
	    return type.equalsIgnoreCase((String)message.get(0));
	    }
	
	String createRetString(Object o,IBaseMarshaller marshaller,int flags,int depth) throws IOException
	    {
		StringWriter sw = new StringWriter();
		sw.write("(:ret");
		marshaller.marshallAtom(o,sw,flags,depth);
		sw.write(')');
		return sw.toString();
	    }

	public void processMessagesOnSocket(int port) throws IOException
		{
		ServerSocket ss = new ServerSocket(port);
		
		Socket s = ss.accept();
		//s.setTcpNoDelay(true);
		processMessages(new BufferedReader(new InputStreamReader(s.getInputStream())),
				new BufferedWriter(new OutputStreamWriter(s.getOutputStream())));
		
		}
	
	public static void main(String[] args)
        {
	    IReferenceManager referenceManager = new ReferenceManager();
	    BaseMarshaller baseMarshaller = new BaseMarshaller(referenceManager);
	    baseMarshaller.registerMarshaller(Object.class, new UniversalMarshaller());
	    IReflector reflector = new Reflector(baseMarshaller);
	    IReader reader = new MessageReader(referenceManager,reflector);
	    RuntimeServer server = new RuntimeServer(reader,baseMarshaller,referenceManager,reflector);
	    try{
	    	if(args.length >= 1) //port #s, run on sockets
	    		{	
	    		//fire up a background thread for all sockets except first
	    		for(int i=1;i<args.length;i++)
	    			{
	    			final RuntimeServer rs = server;
	    			final int port = Integer.parseInt(args[i]);
	    			Runnable r = new Runnable()
						{
	    				public void run()
	    					{
	    					try
								{
	    						rs.processMessagesOnSocket(port);
	    						}
	    					catch(IOException e)
								{
	    						
								}
	    					}
						};
					new Thread(r).start();
	    			}
	    		//app lives with first socket
	    		server.processMessagesOnSocket(Integer.parseInt(args[0]));
	    		}
	    	else //run on stdio
	    		{
	    		server.processMessages(new BufferedReader(new InputStreamReader(System.in)),
	    				new BufferedWriter(new OutputStreamWriter(System.out)));
	    		}
	    	}
        catch(Exception ex)
	    	{
	        System.out.println(ex.getMessage());
	    	}
        }

	/* (non-Javadoc)
	 * @see com.richhickey.foil.IRuntimeServer#proxyCall(int, int, java.lang.reflect.Method, java.lang.Object, java.lang.Object[])
	 */
	public Object proxyCall(int marshallFlags, int marshallDepth, Method method, Object proxy, Object[] args) throws Exception 
		{
		Reader reader = (Reader)proxyReader.get();
		Writer writer = (Writer)proxyWriter.get();
		
		//form the call message:
		//(:proxy-call method-symbol proxy-ref args ...)
		//method-symbol has the form: |package.name|::classname.methodname
		
		String decl = method.getDeclaringClass().getName();
		StringWriter sw = new StringWriter();
		int lastDotIdx = decl.lastIndexOf('.'); 
		sw.write("(:proxy-call |");
		sw.write(decl.substring(0,lastDotIdx));
		sw.write("|::");
		sw.write(decl.substring(lastDotIdx+1));
		sw.write('.');
		sw.write(method.getName());
		
		marshaller.marshallAtom(proxy,sw,IBaseMarshaller.MARSHALL_ID,0);
		
		for(int i=0;args != null && i<args.length;i++)
			{
			marshaller.marshallAtom(args[i],sw,marshallFlags,marshallDepth);
			}
		
		sw.write(')');
		
		writer.write(sw.toString());
		writer.write('\n');
		writer.flush();
		
		
		return processMessages(reader,writer);
		}
    }
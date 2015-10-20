/*
 * Created on Dec 8, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
package com.richhickey.foil;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;
import java.util.*;

/**
 * @author Rich
 *
 */
public class MessageReader implements IReader
    {
    IReferenceManager referenceManager;
    IReflector reflector;
    MessageReader(IReferenceManager referenceManager,IReflector reflector)
    	{
        this.referenceManager = referenceManager;
        this.reflector = reflector;
    	}
    /* (non-Javadoc)
     * @see com.richhickey.foil.IReader#readMessage(java.io.Reader)
     */
    public List readMessage(Reader strm) throws IOException, Exception 
        {
        return readSexpr(strm);
        }

	public List readSexpr(Reader strm)
	throws IOException, Exception
		{
	    return readDelimitedList(strm,'(',')');
        }

	public List readDelimitedList(Reader strm, int startc, int endc)
	throws IOException, Exception
		{
	    
        int c = strm.read();
        while (Character.isWhitespace((char) c))
            c = strm.read();
        
        if (c != startc)
            {
            throw new IOException("expected list to begin with; " + new Character((char)startc));
            }
        List ret = new ArrayList();

        for(strm.mark(1);(c = strm.read()) != endc;strm.mark(1))
            {
            if (Character.isWhitespace((char) c))
                continue;
            switch (c)
                {
                case -1:
                    throw new IOException("unexpected EOF");
                case '(':
                    strm.reset();
                    ret.add(readSexpr(strm));
                    break;
                case '"':
                    strm.reset();
                    ret.add(readString(strm));
                    break;
                case '#':
                    ret.add(readMacro(strm));
                    break;
                default:
                    strm.reset();
                    ret.add(readToken(strm));
                    break;
                }
            }
        return ret;
        }

	Object readMacro(Reader strm)
		throws IOException, Exception
	    {
	    strm.mark(1);
        int c = strm.read();
        if(c == '{')
            {
            strm.reset();
            return processMacroList(readDelimitedList(strm,'{','}'));
            }
        else if(c == '}')	// }id
            {
            Object id = readToken(strm);
            return referenceManager.getObjectForId(id);
            }
        else if(c == '\\')
            {
            c = strm.read();
            return new Character((char)c);
            }
        else
            throw new Exception("unsupported macro sequence");
	    }
	
	Object processMacroList(List args)
		throws Exception
	    {
	    if(RuntimeServer.isMessage(":box",args))
	        {
	        return Reflector.numericConvert(RuntimeServer.typeArg(args.get(1)),args.get(2));
	        }
	    else if(RuntimeServer.isMessage(":vector",args))
	        {
	        return reflector.createVector(RuntimeServer.typeArg(args.get(1)),args.size()-2,
	                					args.subList(2,args.size()));
	        }
       throw new Exception("unsupported macro sequence");
	    }
	
	
    static protected String readString(Reader strm)
            throws IOException
        {
        int c = strm.read();
        if (c != '"')
            {
            throw new IOException("expected string to begin with '\"'");
            }
        StringBuffer sb = new StringBuffer();
        boolean end = false;
        while (!end)
            {
            c = strm.read();
            if (c == -1)
                throw new IOException("unexpected EOF");
            if (c == '"')
                end = true;
            else if (c == '\\')
                {
                c = strm.read();
                if (c == -1)
                    throw new IOException("unexpected EOF");
                if (c == '"' || c == '\\')
                    sb.append((char) c);
                else
                    throw new IOException("unsupported escape character: '"
                            + Character.toString((char) c) + "'");
                }
            else
                sb.append((char) c);
            }
        return sb.toString();
        }

    static protected Object readToken(Reader strm) throws IOException
        {
        StringBuffer sb = new StringBuffer();
        boolean end = false;
        for (strm.mark(1);!end;strm.mark(1))
            {
            int c = strm.read();
            if (c == -1)
                throw new IOException("unexpected EOF");
            if (c == ')' || c == '(' || Character.isWhitespace((char) c)
                    || c == '}')
                {
                strm.reset();
                end = true;
                }
            else
                sb.append((char) c);
            }
        String ret = sb.toString();
        if(isInteger(ret))
            {
            long n = Long.parseLong(ret);
            if(n <= Integer.MAX_VALUE && n >= Integer.MIN_VALUE)
                return new Integer((int)n);
            else
                return new Long(n);
            }
        else if(shouldBeNumber(ret))
            return Double.valueOf(ret);
        else if(ret.equalsIgnoreCase("nil"))
            return null;
        else if(ret.equalsIgnoreCase("t"))
            return Boolean.TRUE;
        return ret;
        }
    
    static boolean isInteger(String s)
        {
        boolean ret = true;
        for(int i=0;ret && i<s.length();++i)
            {
            if(i == 0)
                {
                ret = Character.isDigit(s.charAt(i))
                	|| s.charAt(i) == '-';
                }
            else
                ret = Character.isDigit(s.charAt(i));
            }
        return ret;
        }

    static boolean shouldBeNumber(String s)
        {
        char c = s.charAt(0);
        return Character.isDigit(c) || c == '.' || c == '-';
        }

    public static void main(String[] args)
        {
        IReferenceManager referenceManager = new ReferenceManager();
	    BaseMarshaller baseMarshaller = new BaseMarshaller(referenceManager);
	    baseMarshaller.registerMarshaller(Object.class, new UniversalMarshaller());
        IReflector reflector = new Reflector(baseMarshaller);
        BufferedReader strm = new BufferedReader(new InputStreamReader(System.in));
        IReader rdr = new MessageReader(referenceManager,reflector);
        for(;;)
            {
            try{
                List msg = rdr.readMessage(strm);
                System.out.println(msg.toString());
            	}
            catch(Exception ex)
            	{
                System.out.println(ex.toString());
                break;
            	}
            }
        }
    }

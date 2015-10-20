/*
 * Created on Sep 25, 2004
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
package com.richhickey.lisplets;

import java.io.*;
import java.net.*;
import java.security.Principal;
import java.util.*;
import java.util.Enumeration;
import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.*;


/**
 * @author Rich Hickey
 * 
 */
public class Lisplet extends HttpServlet
	{
	private String host;
	private int port;
	private boolean createSession; 
	private String[] roles;
	private String tagPrefix;
	private String tagSuffix;
	private String trueSym;
	private String falseSym;
	private String nullSym;
	/**
	 *  
	 */
	public Lisplet()
		{
		super();
		}
	
	public void init() throws ServletException
		{
		ServletConfig config = getServletConfig();
		host = config.getInitParameter("lisp-host"); 
		String portInit = config.getInitParameter("lisp-port");
		if(host == null || portInit == null)
			throw new ServletException("lisp-host and lisp-port must be configured as servlet init-params");
		port = Integer.decode(portInit).intValue();
		createSession = "true".equals(config.getInitParameter("lisp-create-session"));
		String rolesInit = config.getInitParameter("lisp-roles"); 
		if(rolesInit != null)
			{
			roles = rolesInit.split(",");
			for(int i = 0;i<roles.length;i++)
				roles[i] = roles[i].trim();
			}
		else
			roles = new String[0];
		tagPrefix = config.getInitParameter("lisp-tag-prefix");
		tagSuffix = config.getInitParameter("lisp-tag-suffix");

		trueSym = ifnull(config.getInitParameter("lisp-true"),"t");
		falseSym = ifnull(config.getInitParameter("lisp-false"),"nil");
		nullSym = ifnull(config.getInitParameter("lisp-null"),"nil");
		
		}

	protected void doGet(HttpServletRequest arg0, HttpServletResponse arg1)
			throws ServletException, IOException
		{
		//route gets to post, since same logic (for now)
		doPost(arg0, arg1);
		}

	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException
		{
		Socket socket = new Socket(host,port);
		try
			{
			Writer writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
			sendRequest(req,writer);
			PushbackInputStream strm = new PushbackInputStream(
					new BufferedInputStream(socket.getInputStream()));
			if(getResponseHeader(req,resp,strm))
				transferResponseBody(resp,strm);
			}
		finally
			{
			socket.close();
			}
		}

	protected void transferResponseBody(HttpServletResponse resp,PushbackInputStream strm)
			throws ServletException, IOException
		{
		OutputStream outs = resp.getOutputStream();
		int b;
		while((b = strm.read()) != -1)
			outs.write(b);
		outs.flush();
		}
	
	protected boolean getResponseHeader(HttpServletRequest req,HttpServletResponse resp,PushbackInputStream strm)
		throws ServletException, IOException
		{
		ArrayList list = readSexpr(strm);
		return processResponse(req,resp,list);
		}

	protected boolean processResponse(HttpServletRequest req,HttpServletResponse resp,ArrayList list)
			throws ServletException, IOException
		{
		if(!processStatusErrorRedirect(resp,list))
			return false;
		processHeaders(resp,list);
		processCookies(resp,list);
		processContext(list);
		processSession(req,list);
		processLog(list);
		return true;
		}

	//returns true if processing should continue
	protected boolean processStatusErrorRedirect(HttpServletResponse resp,ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting only one of 
		//(status an-int)(error an-int ["message"])(redirect "location")
		ArrayList sublist = findSublist("status",responseList);
		if(sublist != null)
			{
			resp.setStatus(Integer.parseInt((String)sublist.get(1)));
			return true;
			}

		sublist = findSublist("redirect",responseList);
		if(sublist != null)
			{
			resp.sendRedirect((String)sublist.get(1));
			return false;
			}

		sublist = findSublist("error",responseList);
		if(sublist != null)
			{
			if(sublist.size() == 3)
				resp.sendError(Integer.parseInt((String)sublist.get(1)),(String)sublist.get(2));
			else
				resp.sendError(Integer.parseInt((String)sublist.get(1)));
			return false;
			}

		return true;
		}
	
	protected void processLog(ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting (log "msg" ["msg" ...])
		ArrayList sublist = findSublist("log",responseList);
		
		if(sublist != null)
			{
			ServletContext context = getServletContext();
			for(int i=1;i<sublist.size();i++)
				{
				context.log((String)sublist.get(i));
				}
			}
		}
	
	protected void processCookies(HttpServletResponse resp,ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting:
		/*
		(cookies 
				((name a-string)(value a-string) [	(comment a-string) 
												(domain a-string) 
												(max-age an-int) 
												(path a-string) 
												(secure a-boolean)
												(version an-int)])
				((name a-string)(value a-string) [	(comment a-string) 
												(domain a-string) 
												(max-age an-int) 
												(path a-string) 
												(secure a-boolean)
												(version an-int)])
												...)
		*/
		ArrayList sublist = findSublist("cookies",responseList);
		if(sublist != null)
			{
			for(int i=1;i<sublist.size();i++)
				{
				//each should be an alist of (cookies-attr cookie-attr-val)
				ArrayList cookie = (ArrayList)sublist.get(i);
				ArrayList name = findSublist("name",cookie);
				ArrayList value = findSublist("value",cookie);
				if(name == null || value == null)
					throw new ServletException("name and value must be supplied for every cookie");
				Cookie c = new Cookie((String)name.get(1),(String)value.get(1));

				ArrayList comment = findSublist("comment",cookie);
				if(comment != null)
					c.setComment((String)comment.get(1));

				ArrayList domain = findSublist("domain",cookie);
				if(domain != null)
					c.setDomain((String)domain.get(1));

				ArrayList maxAge = findSublist("max-age",cookie);
				if(maxAge != null)
					c.setMaxAge(Integer.parseInt((String)maxAge.get(1)));

				ArrayList path = findSublist("path",cookie);
				if(path != null)
					c.setPath((String)path.get(1));

				ArrayList secure = findSublist("secure",cookie);
				if(secure != null)
					c.setSecure(((String)secure.get(1)).equalsIgnoreCase(trueSym));

				ArrayList version = findSublist("version",cookie);
				if(version != null)
					c.setVersion(Integer.parseInt((String)version.get(1)));

				resp.addCookie(c);
				}
			}
		}

	
	protected void processSession(HttpServletRequest req,ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting:
		/*(session
			[(attrs (name val)(name2 val2) ...)]
			[(invalidate)]
			[(max-inactive-interval an-int)])
		
		values can be nil, in which case they will be removed from attr set
		
		 */
		ArrayList sessionlist = findSublist("session",responseList);
		if(sessionlist == null)
			return;
		
		ArrayList sublist = findSublist("invalidate",sessionlist,1);
		if(sublist != null && req.getSession() != null)
			{
			req.getSession().invalidate();
			return;
			}

		//hmm... create a session even if createSession not true,
		//as a side effect of user setting attributes
		HttpSession session = req.getSession(true);
		
		sublist = findSublist("attrs",sessionlist,1);
		if(sublist != null)
			{
			for(int i=1;i<sublist.size();i++)
				{
				//should be a 2 item arraylist, (name val)
				ArrayList entry = (ArrayList)sublist.get(i);
				String value = (String)entry.get(1); 
				session.setAttribute((String)entry.get(0),
						value.equalsIgnoreCase(nullSym)?null:value);
				}
			}

		sublist = findSublist("max-inactive-interval",sessionlist,1);
		if(sublist != null)
			{
			session.setMaxInactiveInterval(Integer.parseInt((String)sublist.get(1)));
			}
		}

	protected void processContext(ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting:
		//(context (attrs (name val)[(name2 val2) ...]))
		//values can be nil, in which case they will be removed from attr set
		ArrayList contextlist = findSublist("context",responseList);
		if(contextlist == null)
			return;
		ArrayList sublist = findSublist("attrs",contextlist,1);
		if(sublist != null)
			{
			ServletContext context = getServletContext();
			for(int i=1;i<sublist.size();i++)
				{
				//should be a 2 item arraylist, (name val)
				ArrayList entry = (ArrayList)sublist.get(i);
				String value = (String)entry.get(1); 
				context.setAttribute((String)entry.get(0),
						value.equalsIgnoreCase(nullSym)?null:value);
				}
			}
		}

	protected void processHeaders(HttpServletResponse resp,ArrayList responseList)
			throws ServletException, IOException
		{
		//expecting:
		//(headers (hname hval)(hname2 hval2) ...)
		ArrayList sublist = findSublist("headers",responseList);
		if(sublist != null)
			{
			for(int i=1;i<sublist.size();i++)
				{
				//should be a 2 item arraylist, (headername headerval)
				ArrayList header = (ArrayList)sublist.get(i);
				resp.addHeader((String)header.get(0),(String)header.get(1));
				}
			}
		}
	
	protected ArrayList findSublist(String tag, ArrayList list)
		{
		return findSublist(tag,list,0);
		}
	
	protected ArrayList findSublist(String tag, ArrayList list, int start)
		{
		String prefixedTag = tagPrefix != null?(tagPrefix + tag):null;
		for(int i = start;i<list.size();i++)
			{
			ArrayList sublist = (ArrayList)list.get(i);
			String atag = (String)sublist.get(0);
			if(atag.equalsIgnoreCase(tag)
					|| (prefixedTag != null && atag.equalsIgnoreCase(prefixedTag)))
				return sublist;
			}
		return null;
		}

	protected ArrayList readSexpr(PushbackInputStream strm)
		throws IOException
		{
		int c = strm.read();
		if(c != '(')
			{
			throw new IOException("expected sexpr to begin with '('");
			}
		ArrayList ret = new ArrayList();
		
		while((c = strm.read()) != ')')
			{
			if(Character.isWhitespace((char)c))
				continue;
			switch(c)
				{
				case -1:
					throw new IOException("unexpected EOF");
				case '(':
					strm.unread(c);
					ret.add(readSexpr(strm));
					break;
				case '"':
					strm.unread(c);
					ret.add(readString(strm));
					break;
				default:
					strm.unread(c);
					ret.add(readToken(strm));
					break;
				}
			}
		return ret;
		}

	static protected String readString(PushbackInputStream strm)
			throws IOException
		{
		int c = strm.read();
		if(c != '"')
			{
			throw new IOException("expected string to begin with '\"'");
			}
		StringBuffer sb = new StringBuffer();
		boolean end = false;
		while(!end)
			{
			c = strm.read();
			if(c == -1)
				throw new IOException("unexpected EOF");
			if(	c == '"')
				end = true;
			else if (c == '\\')
				{
				c = strm.read();
				if(c == -1)
					throw new IOException("unexpected EOF");
				if(c == '"' || c == '\\')
					sb.append((char)c);
				else
					throw new IOException("unsupported escape character: '" + Character.toString((char)c) + "'");
				}
			else
				sb.append((char)c);	
			}
		return sb.toString();
		}

	protected String readToken(PushbackInputStream strm)
			throws IOException
		{
		StringBuffer sb = new StringBuffer();
		boolean end = false;
		while(!end)
			{
			int c = strm.read();
			if(c == -1)
				throw new IOException("unexpected EOF");
			if(		c == ')'
				|| 	c == '('
				|| 	Character.isWhitespace((char)c))
				{
				strm.unread(c);
				end = true;
				}
			else
				sb.append((char)c);	
			}
		String ret = sb.toString();
		if(tagPrefix != null && ret.startsWith(tagPrefix))
			ret = ret.substring(tagPrefix.length());
		return ret;
		}
	
	protected void sendRequest(HttpServletRequest req, Writer writer)
			throws IOException
		{
		//write request as one big sexpr
		writer.write("(");
		
		writeTagValuePair(writer,"method",req.getMethod());
		writeTagValuePair(writer,"uri",req.getRequestURI());
		writeTagValuePair(writer,"protocol",req.getProtocol());
		writeTagLiteralPair(writer,"is-secure",boolSym(req.isSecure()));
		sendHeaders(req,writer);
		sendCookies(req,writer);
		sendParameters(req,writer);
		sendContext(req,writer);
		sendServlet(req,writer);
		sendSession(req,writer);
		sendUser(req,writer);

		writer.write(")");
		writer.flush();
		}

	protected void sendUser(HttpServletRequest req, Writer writer)
			throws IOException
		{
		Principal principal = req.getUserPrincipal();
		if(principal == null)
			return;
		
		writer.write("(");
		writeTag(writer,"user");
		writer.write(" ");
		writeTagValuePair(writer,"name",principal.getName());
		writeTagValuePair(writer,"auth-type",req.getAuthType());
		if(roles.length > 0)
			{
			writer.write("(");
			writeTag(writer,"roles");
			writer.write(" ");
			for(int i = 0;i<roles.length;i++)
				{
				writeTagLiteralPair(writer,roles[i],boolSym(req.isUserInRole(roles[i])));
				}
			writer.write(")");
			}
		writer.write(")");
		}	
	
	protected void sendSession(HttpServletRequest req, Writer writer)
			throws IOException
		{
		
		HttpSession session = req.getSession(createSession);
		if(session == null)
			return;
		
		writer.write("(");
		writeTag(writer,"session");
		writer.write(" ");
		writeTagValuePair(writer,"id",session.getId());
		writeTagLiteralPair(writer,"is-new",boolSym(session.isNew()));
		writeTagLiteralPair(writer,"creation-time",Long.toString(session.getCreationTime()));
		writeTagLiteralPair(writer,"last-accessed-time",Long.toString(session.getLastAccessedTime()));
		writeTagLiteralPair(writer,"max-inactive-interval",Integer.toString(session.getMaxInactiveInterval()));
		
		Enumeration attrNames = session.getAttributeNames();
		if(attrNames.hasMoreElements())
			{
			writer.write("(");
			writeTag(writer,"attrs");
			writer.write(" ");
			while(attrNames.hasMoreElements())
				{
				String aname = (String)attrNames.nextElement();
				writeTagValuePair(writer,aname,session.getAttribute(aname).toString());
				}
			writer.write(")");
			}
		
		writer.write(")");
		}
	
	protected String boolSym(boolean val)
		{
		return val?trueSym:falseSym;
		}
	protected void sendServlet(HttpServletRequest req, Writer writer)
			throws IOException
		{
		writer.write("(");
		writeTag(writer,"servlet");
		writer.write(" ");
		writeTagValuePair(writer,"name",getServletName());
		Enumeration paramNames = getInitParameterNames();
		if(paramNames.hasMoreElements())
			{
			writer.write("(");
			writeTag(writer,"params");
			writer.write(" ");
			while(paramNames.hasMoreElements())
				{
				String pname = (String)paramNames.nextElement();
				writeTagValuePair(writer,pname,getInitParameter(pname));
				}
			writer.write(")");
			}
		writer.write(")");
		}

	protected void sendContext(HttpServletRequest req, Writer writer)
			throws IOException
		{
		ServletContext context = getServletContext();
		writer.write("(");
		writeTag(writer,"context");
		writer.write(" ");
		writeTagValuePair(writer,"name",context.getServletContextName());
		Enumeration paramNames = context.getInitParameterNames();
		if(paramNames.hasMoreElements())
			{
			writer.write("(");
			writeTag(writer,"params");
			writer.write(" ");
			while(paramNames.hasMoreElements())
				{
				String pname = (String)paramNames.nextElement();
				writeTagValuePair(writer,pname,context.getInitParameter(pname));
				}
			writer.write(")");
			}
		Enumeration attrNames = context.getAttributeNames();
		if(attrNames.hasMoreElements())
			{
			writer.write("(");
			writeTag(writer,"attrs");
			writer.write(" ");
			while(attrNames.hasMoreElements())
				{
				String aname = (String)attrNames.nextElement();
				writeTagValuePair(writer,aname,context.getAttribute(aname).toString());
				}
			writer.write(")");
			}
		writer.write(")");
		}
	
	protected void sendCookies(HttpServletRequest req, Writer writer)
			throws IOException
		{
		Cookie[] cookies = req.getCookies();
		if(cookies == null || cookies.length == 0)
			return;
		
		writer.write("(");
		writeTag(writer,"cookies");
		writer.write(" ");
		for(int i=0;i<cookies.length;i++)
			{
			Cookie c = cookies[i];
			writeTagValuePair(writer,c.getName(),c.getValue());
			}
		writer.write(")");
		}
	
	protected void sendParameters(HttpServletRequest req, Writer writer)
			throws IOException
		{
		writer.write("(");
		writeTag(writer,"params");
		writer.write(" ");
	    for (Iterator params = req.getParameterMap().entrySet().iterator() ; params.hasNext() ;) 
	    	{
	    	Map.Entry entry = (Map.Entry)params.next();
			writer.write("(");
	    	writeTag(writer,(String)entry.getKey());
	    	String[] vals = (String[])entry.getValue();
	    	for(int i=0;i<vals.length;i++)
	    		{
	    		writer.write(" ");
		    	writeReadableString(writer,vals[i]);
	    		}
			writer.write(")");
	    	}
		writer.write(")");
		}

	protected void sendHeaders(HttpServletRequest req, Writer writer)
			throws IOException
		{
		writer.write("(");
		writeTag(writer,"headers");
		writer.write(" ");
	    for (Enumeration hnames = req.getHeaderNames() ; hnames.hasMoreElements() ;) 
	    	{
	        String hname = (String)hnames.nextElement();
	        //skip cookie headers since we will package separately
	        if("Cookie".equalsIgnoreCase(hname))
	        	continue;
			writer.write("(");
			writeTag(writer,hname);
		    for (Enumeration headers = req.getHeaders(hname) ; headers.hasMoreElements() ;)
		    	{
				writer.write(" ");
				writeReadableString(writer,(String)headers.nextElement());
		    	}
			writer.write(")");
	    	}		
		writer.write(")");
		}

	protected void writeTagValuePair(Writer writer, String tag, String value)
			throws IOException
		{
		writer.write("(");
		writeTag(writer,tag);
		writer.write(" ");
		writeReadableString(writer,value);
		writer.write(")");
		}

	protected void writeTagLiteralPair(Writer writer, String tag, String value)
			throws IOException
		{
		writer.write("(");
		writeTag(writer,tag);
		writer.write(" ");
		writer.write(value);
		writer.write(")");
		}

	protected void writeTag(Writer writer, String tag)
			throws IOException
		{
		if(tagPrefix != null)
			writer.write(tagPrefix);
		writer.write(tag);
		if(tagSuffix != null)
			writer.write(tagSuffix);
		}
	
	protected void writeReadableString(Writer writer, String s)
		throws IOException
		{
		writer.write("\"");
		writer.write(s.replaceAll("\\\\","\\\\\\\\").replaceAll("\"","\\\\\""));
		writer.write("\"");
		}
	
	protected String ifnull(String s, String s2)
		{
		if(s == null)
			return s2;
		return s;
		}
	}
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
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
/**
 * @author Eric Thorsen
 *
 */
namespace com.richhickey.foil
{
	/// <summary>
	/// The proxy class implements 1 or more interfaces and forwards all calls to a delegate or
	/// a class that implements the Proxy.IInvocationHanlder interface.  Only 1 class per
	/// unique set of interfaces is created.  The interfaces implemented will be an intersection
	/// of all interfaces found hierarchically.  Instances are bound to the passed delegate or 
	/// IInvocationHandler implementation.  The data object field is to allow storing any
	/// arbritrary data.
	/// </summary>
	public class Proxy
	{
		/// <summary>
		/// Derive from this and implement the Invoke method or use the delegate below which 
		/// has the same signature.
		/// </summary>
		public	interface	IInvocationHandler	
		{
			/// <summary>
			/// All functions and property setter/getter funtions get forwared to this call.
			/// </summary>
			/// <param name="proxy">The Proxy instance that was created.</param>
			/// <param name="method">MethodInfo of the function called.</param>
			/// <param name="args">zero or more arguments passed to the function</param>
			/// <returns></returns>
			Object	Invoke(		Object			proxy
							,	MethodInfo		method
							,	Object[]		args);

		};
		/// <summary>
		/// All functions and property setter/getter funtions get forwared to this call.
		/// </summary>
		/// <param name="proxy">The Proxy instance that was created.</param>
		/// <param name="method">MethodInfo of the function called.</param>
		/// <param name="args">zero or more arguments passed to the function</param>
		/// <returns></returns>
		public	delegate Object		InvocationDelegate(		Object			proxy
														,	MethodInfo		method
														,	Object[]		args);
		/// <summary>
		/// The delegate where all calls get forwarded.
		/// </summary>
		protected	InvocationDelegate	_invocationDelegate;
		/// <summary>
		/// When an implementation of a IInvocationHandleris passed in a Proxy.InvocationDelegate
		/// is created bound to the implementations Invoke function.  This enables avoiding any
		/// branching logic required in the emitted code.  This member allows the callee to have
		/// access to the implementation instance via the Proxy object passed in the Invoke call.
		/// </summary>
		protected	IInvocationHandler	_iInvocationHandler;
		/// <summary>
		/// In case the user wants to store any data in the proxy.
		/// </summary>
        public		Object				data;	// opaque data
		// proxy call target.  When inplementing interfaces it's 'this else a delegate
		public		Object				target;	// opaque data
		// For creating unique names.
		static	Random		rand			=	new Random((int)DateTime.Now.Ticks);
		// For the class cache.
		static	Hashtable	metadataCache	=	new Hashtable();
		// For the class cache for Event handlers.
		static	Hashtable	delegateCache	=	new Hashtable();
		/// <summary>
		/// Properties for the callbacks.  If an implementation of IInvocationHandler
		/// is used it is bound to an InvocationDelegate.
		/// </summary>
		public	InvocationDelegate invocationDelegate
		{
			get { return	_invocationDelegate;	}
			set	{	_invocationDelegate	=	value;	}
		}

		public	IInvocationHandler iInvocationHandler
		{
			get { return	_iInvocationHandler;	}
			set	{	_iInvocationHandler	=	value;	
					this._invocationDelegate	=	new InvocationDelegate(this._iInvocationHandler.Invoke);
				}
		}
		/// <summary>
		/// There are 4 flavors for creating Proxies.  All take a set of interface types to implement.
		/// They can be instantiated with a delegate or an interface implementation 
		/// or with and without instance data.
		/// </summary>
		/// <param name="invocationHandler"></param>
		/// <param name="interfaces"></param>
		/// <returns></returns>
		static	public Object BuildProxy(InvocationDelegate invocationHandler,params Type[]	interfaces)
		{
			if(interfaces.Length==0)
				throw new ArgumentOutOfRangeException("interfaces must contain at least 1 interface type");
			return	BuildProxy(invocationHandler,null,recurseInterfaces(interfaces));
		}

		static	public Object BuildProxy(IInvocationHandler _iInvocationHandler,params Type[]	interfaces)
		{
			if(interfaces.Length==0)
				throw new ArgumentOutOfRangeException("interfaces must contain at least 1 interface type");
			return	BuildProxy(new InvocationDelegate(_iInvocationHandler.Invoke),null,recurseInterfaces(interfaces));
		}

		static	public Object BuildProxy(InvocationDelegate invocationHandler,Object data,params Type[]	interfaces)
		{
			if(interfaces.Length==0)
				throw new ArgumentOutOfRangeException("interfaces must contain at least 1 interface type");
			return	createInstance(invocationHandler,data,recurseInterfaces(interfaces));
		}

		static	public Object BuildProxy(IInvocationHandler _iInvocationHandler,Object data,params Type[]	interfaces)
		{
			if(interfaces.Length==0)
				throw new ArgumentOutOfRangeException("interfaces must contain at least 1 interface type");
			Object	p				=	createInstance(new InvocationDelegate(_iInvocationHandler.Invoke),data,recurseInterfaces(interfaces));
			if(p is Proxy)
				((Proxy)p)._iInvocationHandler	=	_iInvocationHandler;
			return	p;
		}
		/// <summary>
		/// Creates an instance of a class.  The class type is determined by the set of interfaces passed.
		/// For each set passed there is 1 type created.  All instances are then created from the same type.
		/// </summary>
		/// <param name="invoker">delegate for the callback on any function or property setter/getter</param>
		/// <param name="data">any data the user wishes to store in the proxy instance</param>
		/// <param name="interfaces">set of interfaces to implement</param>
		/// <returns>an instance of the proxy with the bound callback and data</returns>
		static internal	Object	createInstance(InvocationDelegate invoker,Object data,Type[] interfaces)
		{
			Type	newType				=	getClassFor(interfaces);
			Proxy	proxy				=	(Proxy)Activator.CreateInstance(newType,true);
			proxy._invocationDelegate	=	invoker;
			proxy.data					=	data;
			if(isDelegate(interfaces))
				return	proxy.target	=	Delegate.CreateDelegate(interfaces[0],proxy,"Invoke");
			else
				return	proxy.target	=	proxy;
		}

		static	internal	bool	isDelegate(Type[] types)
		{
			if(types.Length==1)
				return	types[0].BaseType == typeof(System.MulticastDelegate);
			return	false;
		}

		static	internal	bool	areAllInterfaces(Type[] types)
		{
			foreach(Type i in types)
				if(!i.IsInterface)
					throw new ArgumentException("All types for proxies must be interfaces or a single delegate type.");
			return	true;
		}
		/// <summary>
		/// Handles getting a proxy type from the cache or creating it.
		/// </summary>
		/// <param name="interfaces">interfaces to implement</param>
		/// <returns>Type for the proxy</returns>
		static	internal	Type	getClassFor(Type[] interfaces)
		{

			String	k		=	buildHashString(interfaces);
			Object	newType	=	metadataCache[k];
			if(newType==null)
			{
				if(isDelegate(interfaces))
					metadataCache[k]	=	newType	=	buildTypeForDelegate(interfaces[0]);
				else
					metadataCache[k]	=	newType	=	buildTypeForInterfaces(interfaces);
			}
			return	(Type)newType;
		}

		static	Type	buildTypeForInterfaces(Type[] interfaces)
		{
			AppDomain appDomain = AppDomain.CurrentDomain;
			AssemblyName assemblyName = new AssemblyName();
			assemblyName.Name	= "_Dymamic__PROXY_TT_";
			AssemblyBuilder assemblyBuilder = appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
			ModuleBuilder	moduleBuilder	= assemblyBuilder.DefineDynamicModule("_TT_DymanicModule_");
			TypeBuilder builder	= moduleBuilder.DefineType(		String.Format("TTProxy{0}.{1}",rand.Next(),interfaces[0])
				,	TypeAttributes.Public
				,	typeof(Proxy)
				,	interfaces
				);
			Type	proxyType	=	typeof(Proxy);
			FieldInfo	fb		=	proxyType.GetField("_invocationDelegate",BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			FieldInfo	target	=	proxyType.GetField("target",BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			foreach(Type t in interfaces)
			{
				Hashtable	properties	=	new Hashtable();
				// Now define the properties of any given interface.
				foreach(PropertyInfo pi in t.GetProperties(BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public))
				{
					Type[]	args		=	Reflector.getParameterTypes(pi.GetIndexParameters());
					PropertyBuilder	pb	=	builder.DefineProperty(pi.Name,pi.Attributes,pi.PropertyType,args);
					properties[pb.Name]	=	pb;
				}
				foreach(MethodInfo m in t.GetMethods(BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public))
					Implementor.ImplementMethod(t,m,builder,fb,target,properties);
			}
			return	builder.CreateType();
		}

		static	Type	buildTypeForDelegate(Type delegateType)
		{
			AppDomain appDomain = AppDomain.CurrentDomain;
			AssemblyName assemblyName = new AssemblyName();
			assemblyName.Name	= "_Dymamic__PROXY_TT_";
			AssemblyBuilder assemblyBuilder = appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
			ModuleBuilder	moduleBuilder	= assemblyBuilder.DefineDynamicModule("_TT_DymanicModule_");
			TypeBuilder builder	= moduleBuilder.DefineType(		String.Format("TTProxy{0}.{1}",rand.Next(),delegateType)
				,	TypeAttributes.Public
				,	typeof(Proxy)
				);

			Type	proxyType	=	typeof(Proxy);
			FieldInfo	fb		=	proxyType.GetField("_invocationDelegate",BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			FieldInfo	target	=	proxyType.GetField("target",BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			Implementor.ImplementMethod(delegateType,delegateType.GetMethod("Invoke"),builder,fb,target,null);
			return	builder.CreateType();
		}

		/// <summary>
		/// In case there are multiple references to the same interface due to inheritance
		/// flatten out the list.
		/// </summary>
		/// <param name="interfaces">List of interfaces</param>
		/// <returns>The uniqiue set of all interfaces in the hierarchy</returns>
		static	internal	Type[]	recurseInterfaces(Type[] interfaces)
		{
			Hashtable	ht	=	new Hashtable();
			foreach(Type t in interfaces)
				doRecurseInterfaces(ht,t);
			Type[]	retval	=	new Type[ht.Count];
			ArrayList ar	=	new ArrayList();
			foreach(Type t in ht.Values)
				ar.Add(t);
			ar.CopyTo(retval);
			return	retval;
		}

		static	internal	void	doRecurseInterfaces(Hashtable	list,Type intface)
		{
			list.Add(intface.ToString(),intface);
			if(intface.BaseType==null||intface.BaseType==typeof(Object)||
				intface.BaseType==typeof(System.MulticastDelegate)||intface.BaseType==typeof(System.Delegate))
				return;
			doRecurseInterfaces(list,intface.BaseType);
		}

		static	internal String buildHashString(Type[] interfaces)
		{
			Array.Sort(interfaces,new CompTypes());
			StringBuilder	sb	=	new StringBuilder();
			foreach(Type t in interfaces)
				sb.Append(t.ToString());
			return	sb.ToString();
		}
		class CompTypes:IComparer
		{
			public	
				int Compare(object x,object y)
			{	return	String.Compare(x.ToString(),y.ToString());	}
                
		}
	}
}

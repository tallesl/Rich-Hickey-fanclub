using System;
using System.Reflection;

namespace  com.richhickey.foil
{
	/// <summary>
	/// Summary description for ProxyHandler.
	/// </summary>
	public class ProxyHandler
	{
		IRuntimeServer	runtime;
		int				marshallFlags;
		int				marshallDepth;

		public ProxyHandler(IRuntimeServer runtime,int marshallFlags,int marshallDepth)
		{
			this.runtime		= runtime;
			this.marshallFlags	= marshallFlags;
			this.marshallDepth	= marshallDepth;
		}

		public Object invoke(Object proxy, MethodInfo method, Object[] args)
		{
			return runtime.proxyCall(marshallFlags,marshallDepth,method,proxy,args);
		}
	}
}

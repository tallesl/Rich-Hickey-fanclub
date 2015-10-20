//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;
using System.Collections.Specialized;

namespace DotLisp{

internal class CLSLateBoundMember : CLSMember
	{

	internal CLSLateBoundMember(String name)
		{
		this.name = name;
		}


	public override Object Invoke(params Object[] args)
		{
		// instance member gets target from first arg
		Object target = args[0];

		if(target is Record)
			{
			Record rec = (Record)target;

			if(args.Length == 2)	//set call
				{
				rec[name] = args[1];
				return args[1];
				}
			else	//get
				{
				Object ret = rec[name];
				if(ret == null && ! rec.Contains(name))
					throw new Exception("Record does not contain member: " + name);
				return ret;
				}
			}
		else
			{
			//get a real member to do the work
			//first check the cache
			Type targetType = target.GetType();
			CLSMember member = (CLSMember)cache[targetType];
			if(member == null)
				{
				//late-bound members are never static
				cache[targetType] = member = CLSMember.FindMember(name,targetType,false);
				}
			return member.Invoke(args);
			}
		}

	HybridDictionary cache = new HybridDictionary();
	}
}

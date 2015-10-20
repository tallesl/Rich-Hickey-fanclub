//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.Collections.Specialized;

namespace DotLisp
{
internal class BinOpList
	{
	internal Object nullFunc = null; 
	internal HybridDictionary methods = new HybridDictionary();
	internal HybridDictionary cache = new HybridDictionary();
	}

internal class BinOpKey
	{
	internal BinOpKey(Type t1,Type t2)
		{
		this.t1 = t1;
		this.t2 = t2;
		this.hash = t1.GetHashCode() + t2.GetHashCode();
		}
	public override Boolean Equals(Object other)
		{
		BinOpKey ob = other as BinOpKey;
		return ob != null && ob.t1 == this.t1 && ob.t2 == this.t2;
		}
	public override Int32 GetHashCode()
		{
		return hash;
		}

	internal Type t1;
	internal Type t2;
	Int32 hash;
	}

internal class BinOpCacheEntry
	{
	internal BinOpCacheEntry(Type key1,Type key2,Object val,BinOpCacheEntry next)
		{
		this.key1 = key1;
		this.key2 = key2;
		this.val = val;
		this.next = next;
		}
	internal Object val;
	internal Type key1;
	internal Type key2;
	internal BinOpCacheEntry next = null;
	}

//binops don't support dispatch on value for efficiency
public class BinOp : IFunction
	{
	public Object Invoke(params Object[] args)
		{
		if(args.Length != 2)
			throw new Exception("Must pass exactly 2 args to BinOp");
		Object f = FindBestMethod(args[0],args[1]);
		if(f == null)
			{
			throw new Exception("Error - no BinOp method found matching arguments: "
									  + args[0] + " " + args[1]);
			}

		return Util.InvokeObject(f,args);
		}

	public void AddMethod(Object dispatch1,Object dispatch2,Object func)
		{
		BinOpList methods = (BinOpList)methodLists[dispatch1];

		if(methods == null)
			methodLists[dispatch1] = methods = new BinOpList();

		methods.methods[dispatch2] = func;

		clearCache();
		}

	public Object FindBestMethod(Object dispatch1,Object dispatch2)
		{
		Object method = null;
		//BinOpList methods = null;
		//BinOpKey key = new BinOpKey(dispatch1.GetType(),dispatch2.GetType());
		//try to find in cache
		Type t1 = dispatch1.GetType();
		Type t2 = dispatch2.GetType();
		//method = findCachedMethod(key);
		method = findCachedMethod(t1,t2);
		if(method != null)
			return method;
		//try to find type
		//BinOpList methods = (BinOpList)findBestMatch(key.t1,methodLists);
		BinOpList methods = (BinOpList)findBestMatch(t1,methodLists);
		if(methods != null)
			{
			//method = findBestMatch(key.t2,methods.methods);
			method = findBestMatch(t2,methods.methods);
			if(method != null)
				{
				//cacheMethod(key,method);
				cacheMethod(t1,t2,method);
				return method;
				}
			}
		return null;
		}

	static Object findBestMatch(Type argType,HybridDictionary methods)
		{
		//walk through to find best match
		Type bestType = null;
		Object best = null;
		foreach(DictionaryEntry e in methods)
			{
			Type tryType = e.Key as Type;
			if(tryType != null)
				{
				if(tryType.IsAssignableFrom(argType) 
					&& GenericFunction.isMoreSpecific(tryType,bestType))
					{
					bestType = tryType;
					best = e.Value;
					}
				}
			}
		return best;
		}     

	void clearCache()
		{
		//methodCache.Clear();
		cache = null;
		}

	void cacheMethod(BinOpKey key,Object method)
		{
		methodCache[key] = method;
		}

	void cacheMethod(Type t1,Type t2,Object method)
		{
		//if can find an existing entry, swap the value
		BinOpCacheEntry e = findCacheEntry(t1,t2);
		if(e == null)
			cache = new BinOpCacheEntry(t1,t2,method,cache);
		else
			e.val	= method;
		}

	Object findCachedMethod(BinOpKey key)
		{
		return methodCache[key];
		}

	Object findCachedMethod(Type t1,Type t2)
		{
		BinOpCacheEntry entry = findCacheEntry(t1,t2);
		if(entry != null)
			return entry.val;

		return null;
		}

	BinOpCacheEntry findCacheEntry(Type t1,Type t2)
		{
		for(BinOpCacheEntry e = cache;e!=null;e = e.next)
		//for(BinOpCacheEntry e = cache,prev = null;e!=null;prev = e,e = e.next)
			{
			if(e.key1 == t1 && e.key2 == t2)
				{
				//this linked-list method could be a problem with threads
				//move to top of list if found
				//if(prev != null)
				//	{
				//	prev.next = e.next;
				//	e.next = cache;
				//	cache = e;
				//	}
				return e;
				}
			}
		return null;
		}

	//Type -> BinOpList	
	private HybridDictionary methodLists = new HybridDictionary();
	//BinOpKey -> Object
	private HybridDictionary methodCache = new HybridDictionary();
	BinOpCacheEntry cache = null;
	}

public class GenericFunction: IFunction
	{
	public Object Invoke(params Object[] args)
		{
		Object f = FindBestMethod(args[0]);
		if(f == null)
			{
			throw new Exception("Error - no method found matching argument: "
									  + args[0]);
			}
		return Util.InvokeObject(f,args);
		}

	public void AddMethod(Object dispatch,Object func)
		{
		if(dispatch == null)
			nullFunc = func;
		else
			methods[dispatch]	= func;

		cache.Clear();
		basecache.Clear();
		}

	public void RemoveMethod(Object dispatch)
		{
		methods.Remove(dispatch);
		cache.Clear();
		basecache.Clear();
		}

	public Object FindBaseMethod(Type argType)
		{
		//try the base cache
		Object cacheMatch = basecache[argType];
		if(cacheMatch != null)
			return cacheMatch;
		//walk through to find best match
		Type bestType = null;
		Object bestFunc = null;
		foreach(DictionaryEntry e in methods)
			{
			Type tryType = e.Key as Type;
			if(tryType != null)
				{
				if(tryType != argType && tryType.IsAssignableFrom(argType) 
					&& isMoreSpecific(tryType,bestType))
					{
					bestType = tryType;
					bestFunc = e.Value;
					}
				}
			}
		if(bestType != null)
			{
			basecache[argType] = bestFunc;
			}
		return bestFunc;
		}

	public Object FindBestMethod(Object dispatch)
		{
		if(dispatch == null)
			return nullFunc;
		else
			{
			//try to find the value
			Object valueMatch = methods[dispatch];
			if(!(dispatch is Type) && valueMatch != null)
				return valueMatch;
			else
				{
				Type argType = dispatch.GetType();
				//try the cache
				Object cacheMatch = cache[argType];
				if(cacheMatch != null)
					return cacheMatch;
				//walk through to find best match
				Type bestType = null;
				Object bestFunc = null;
				foreach(DictionaryEntry e in methods)
					{
					Type tryType = e.Key as Type;
					if(tryType != null)
						{
						if(tryType.IsAssignableFrom(argType) 
							&& isMoreSpecific(tryType,bestType))
							{
							bestType = tryType;
							bestFunc = e.Value;
							}
						}
					}
				if(bestType != null)
					{
					cache[argType] = bestFunc;
					}
				return bestFunc;
				}
			}
		}

	internal static Boolean isMoreSpecific(Type t1, Type t2)
		{
		if(t2 == null)
			return true;
		return(t2.IsAssignableFrom(t1) && !t1.IsAssignableFrom(t2));
		}

	private Object nullFunc = null;  
	private HybridDictionary methods = new HybridDictionary();
	private HybridDictionary cache = new HybridDictionary();
	private HybridDictionary basecache = new HybridDictionary();
	}
}

//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections;
using System.Text;

namespace DotLisp
{
public class Symbol
	{
	virtual public Object getGlobalValue()
		{
		if(globalValue != UNDEFINED)
			return globalValue;
		throw new Exception("ERROR: undefined variable "+ name);
		}

	public Boolean isDefined()
		{
		return(globalValue!=UNDEFINED);
		}

	internal Boolean isDynamic = false;

	public virtual Object setGlobalValue(Object newval)
		{
		return( globalValue=newval);
		}

	public override String ToString()
		{
		return name;
		}

	public Symbol Setter{
		get{return setter;}
		set{setter = value;}
	}

	private static Int32 nextgen = 0;
	internal static Symbol gensym()
		{
		++nextgen;
		return new Symbol("#G" + nextgen);
		}

	internal Symbol setter = null;
	internal String name;


	protected static Symbol UNDEFINED = new Symbol("#!undefined");

	//every symbol maintains its global value
	protected Object globalValue = UNDEFINED;

	internal Symbol(String name)
		{
		this.name = name;
		} 
	}

internal class SymbolTable
	{
	internal Symbol internConstant(String name,Object val)
		{
		Symbol result = null;
		if(table.ContainsKey(name))
			{
			throw new Exception("Constant: " + name + " already defined");
			}
		table[name] = result = new Constant(name,val);
		return result;
		}

	internal Symbol intern(String name)
		{
		Symbol result = (Symbol)table[name];
		if(result == null)
			{
			if(name.StartsWith(":"))
				{
				table[name] = result = new Keyword(name);
				}
			else
				{
				int firstdot = name.IndexOf('.');
				int lastdot = name.LastIndexOf('.'); 
				int lastcolon = name.LastIndexOf(':'); 
				int nameLength=name.Length;

				// .instance
				// .[namespace.]type:explicitinstance
				// [namespace.]type.
				// [namespace.]type:static
				// obj.member - transformed by reader
				if((firstdot != -1 || lastcolon != -1) && nameLength > 1)
					{
					if(firstdot == 0)	//instance
						{
						if(lastcolon > 0)	  //explicitly qualified
							{
							String memberName = name.Substring(lastcolon+1,nameLength-(lastcolon+1));
							Type type = findType(name.Substring(1,lastcolon-1));
							table[name] = result = new CLSInstanceSymbol(name,memberName,type);
							}
						else
							{
							String memberName = name.Substring(1,nameLength-1);
							table[name] = result = new CLSInstanceSymbol(name,memberName,null);
							}
						}
					else if(lastcolon > 0)	//static
						{
						String memberName = name.Substring(lastcolon+1,nameLength-(lastcolon+1));
						Type type = findType(name.Substring(0,lastcolon));
						table[name] = result = new CLSStaticSymbol(name,memberName,type);
						}
					else if(lastdot==nameLength-1) //type
						{
						Type type = findType(name.Substring(0,lastdot));
						table[name] = result = new CLSTypeSymbol(name,type);
						}
					}
				else
					{
					table[name] = result = new Symbol(name);
					result.isDynamic = (name[0] == '*');
					}
				}
			}
		return result;
		}

	internal void internType(Type t)
		{
		//add the name to both the full and shortNames
		//should be no dupes in fullNames
		if(fullNamesToTypes[t.FullName] == null)
			{
			fullNamesToTypes[t.FullName] = t;

			ArrayList arr = (ArrayList)shortNamesToTypes[t.Name];

			if(arr == null)
				{
				arr = new ArrayList(5);
				shortNamesToTypes[t.Name] = arr;
				}
			arr.Add(t);
			}
		}

	internal Type findType(String name)
		{
		if(name.IndexOf(".") > -1)	//namespace qualified
			{
			Type t = (Type)fullNamesToTypes[name];
			if(t != null)
				return t;
			else
				{
				throw new Exception("Can't find " + name);
				}
			}
		else	//short name
			{
			ArrayList tlist = (ArrayList)shortNamesToTypes[name];
			if(tlist == null)
				{
				throw new Exception("Can't find Type: " + name);
				}
			else if(tlist.Count > 1)
				{
				//todo tell them which names conflict
				StringBuilder sb = new StringBuilder
										 ("Ambiguous Type name: " + name + " ->");
				foreach(Type type in tlist)
					{
					sb.Append(" " + type.FullName);
					}
				throw new Exception(sb.ToString());
				}
			else
				return(Type)tlist[0];
			}
		}

	static Hashtable table = new Hashtable(500);
	// String->Type
	private static Hashtable fullNamesToTypes = new Hashtable(500);
	// String->ArrayList<Type>
	private static Hashtable shortNamesToTypes = new Hashtable(500);

	}
}

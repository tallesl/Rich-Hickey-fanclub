//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Collections.Specialized;
using System.Reflection;
using System.Reflection.Emit;

namespace DotLisp
{
public class Record : HybridDictionary
	{
	public Record():base()
		{
		}      
	public static Type CreateRecordType(String name,Type basetype)
		{
		Type t = (Type)recordTypes[name];
		if(t!=null)
			return t;
		if(!typeof(Record).IsAssignableFrom(basetype))
			throw new Exception("Record types must be derived from Record or another Record type");
		recordTypes[name] = t = makeRecord(name,basetype);
		return t;
		}

	internal static Type makeRecord(String name,Type basetype)
		{
		if(assembly == null)
			{
			AssemblyName assemblyName = new AssemblyName();
			assemblyName.Name = "RecordAssembly";
			assembly = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName,AssemblyBuilderAccess.Run);
			module = assembly.DefineDynamicModule("RecordModule");
			}

		TypeBuilder tb = module.DefineType(name,TypeAttributes.Class|TypeAttributes.Public,basetype);
		Type[] paramTypes = Type.EmptyTypes;
		ConstructorBuilder cb = tb.DefineConstructor(MethodAttributes.Public,
																	CallingConventions.Standard,
																	paramTypes);
		ILGenerator constructorIL = cb.GetILGenerator();   
		constructorIL.Emit(OpCodes.Ldarg_0);
		ConstructorInfo superConstructor = basetype.GetConstructor(Type.EmptyTypes);
		constructorIL.Emit(OpCodes.Call, superConstructor);
		constructorIL.Emit(OpCodes.Ret);


		Type t = tb.CreateType();
		//Import.AddType(t); //must do in lisp
		return t;
		}
	internal static HybridDictionary recordTypes = new HybridDictionary();
	internal static AssemblyBuilder assembly;
	internal static ModuleBuilder module;
	}
}

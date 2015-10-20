using System;
using System.Reflection;
using System.Reflection.Emit;
using System.IO;
using System.Collections;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for TypeBuilder.
	/// </summary>
	public class Implementor
	{

		static	void	checkForPropertyFuncs(MethodBuilder	mb,Hashtable props)
		{
			if(mb.Name.StartsWith("get_"))
				((PropertyBuilder)props[mb.Name.Replace("get_","")]).SetGetMethod(mb);
			if(mb.Name.StartsWith("set_"))
				((PropertyBuilder)props[mb.Name.Replace("set_","")]).SetSetMethod(mb);
		}

		// Not a good way to do this
		static	bool	isPropertyFunc(String mb)
		{
			if(mb.StartsWith("get_")||mb.StartsWith("set_"))
				return	true;
			else
				return	false;
		}

		
		static	public	void	ImplementMethod(Type interfaceType,MethodInfo method,TypeBuilder typeBuilder,FieldInfo invoker,FieldInfo target,Hashtable	props)
		{
			Type[]	args	=	Reflector.getParameterTypes(method.GetParameters());
			// Don't need to do these for Object
			if(	(method.Name=="Equals"
				||method.Name=="ToString"
				||method.Name=="Finalize") && args.Length==0)
				return;
			MethodBuilder methodBuilder =null;
			if(!isPropertyFunc(method.Name))
				methodBuilder	=
					typeBuilder.DefineMethod(	method.Name
											,	MethodAttributes.Public | MethodAttributes.Virtual
											,	method.ReturnType
											,	args);
			else
				methodBuilder
					= typeBuilder.DefineMethod(		method.Name
											,	MethodAttributes.Public | MethodAttributes.Virtual
												|MethodAttributes.HideBySig|MethodAttributes.NewSlot|MethodAttributes.SpecialName
											,	method.ReturnType
											,	args);
			
			// Forward all calls to the invocationHandler delegate
			genDelegateCall(interfaceType,args,methodBuilder,invoker,target);
			checkForPropertyFuncs(methodBuilder,props);//Bind functions to properties if applicable
		}

		static	public	void	genDelegateCall(	Type			interfaceType
													,Type[]			parameters
													,MethodBuilder	methodBuilder
													,FieldInfo		invoker
													,FieldInfo		target)
		{
			ILGenerator	IL	=	methodBuilder.GetILGenerator();
			// Declare and initialize the array for passing the parameters.
			IL.DeclareLocal(typeof(object[]));		//loc.0
			IL.DeclareLocal(typeof(MethodInfo));	//loc.1
			IL.DeclareLocal(typeof(object));		//loc.2

			//If there is a return
			if(methodBuilder.ReturnType!=typeof(void))
				IL.DeclareLocal(methodBuilder.ReturnType);	//loc.3
			// Init the args array
			if(parameters==null)
			{
				IL.Emit(OpCodes.Ldnull);
				storeLocal(IL,0);
			}
			else
			{
				loadInt32(IL,parameters.Length);
				IL.Emit(OpCodes.Newarr, typeof(object));
				storeLocal(IL,0);
				// Store the parameters in the new array.
				for(int i = 0; i < parameters.Length; i++) 
				{
					loadLocal(IL,0);//Load the array reference
					putArgInArray(IL,i, parameters[i]);
				}
			}
			// Store the methodinfo object using the Ldtoken command. Must use the interfaces MethodInfo.
			MethodInfo	mi	=	parameters==null?interfaceType.GetMethod(methodBuilder.Name):interfaceType.GetMethod(methodBuilder.Name,parameters);
			IL.Emit(OpCodes.Ldtoken,mi);
			IL.Emit(OpCodes.Call, typeof(MethodBase).GetMethod("GetMethodFromHandle"));
			storeLocal(IL,1);
			IL.Emit(OpCodes.Ldarg_0);		//	Proxy (this)
			IL.Emit(OpCodes.Ldfld, target);	//	The delegate from the proxy
			storeLocal(IL,2);
			// Setup the stack for the delegate call.
			IL.Emit(OpCodes.Ldarg_0);		//	this needed for the load field opcode
			IL.Emit(OpCodes.Ldfld, invoker);//	The delegate from the proxy
			loadLocal(IL,2);
			//IL.Emit(OpCodes.Ldarg_0);		//	1st arg - Proxy (this)
			loadLocal(IL,1);				//	2nd arg - MethodInfo 
			loadLocal(IL,0);				//	3rd arg - array of arguments
			IL.Emit(OpCodes.Callvirt,(typeof(Proxy.InvocationDelegate).GetMethod("Invoke")));
			emitReturnFromMethod(IL, methodBuilder.ReturnType);
			if(methodBuilder.ReturnType!=typeof(void))
			{	// Not sure I need to so this but the C# compiler seems to generate this code?
				storeLocal(IL,3);
				loadLocal(IL,3);
			}
			IL.Emit(OpCodes.Ret);
			}

		// Helper functions for emitting MSIL
		static	private void emitReturnFromMethod(ILGenerator IL, Type returnType)
		{
			if (returnType == typeof(void)) 
				IL.Emit(OpCodes.Pop);//Don't care about the return value so remove it.
			else
			{	// Box if needed or just cast to the correct type.
				if (returnType.IsValueType) 
					unbox(IL,returnType);
				else
					IL.Emit(OpCodes.Castclass, returnType);
			} //return
			//IL.Emit(OpCodes.Ret);
		}

		static public void putArgInArray(ILGenerator IL,int index, Type arg)
		{
			loadInt32(IL,index);//Load index to assign into
			IL.Emit(OpCodes.Ldarg_S,index+1);//Load the nth argument, skip this

			if (arg.IsByRef)
			{	//If the arg is object, load by reference
				loadRef(IL,arg.GetElementType());
			}

			if (arg.IsPrimitive || arg.IsValueType) 
				IL.Emit(OpCodes.Box,arg);

			IL.Emit(OpCodes.Stelem_Ref);
		}

		static private void loadRef(ILGenerator IL,Type arg)
		{
			switch(Type.GetTypeCode(arg))
			{
				case TypeCode.Single: 
					IL.Emit(OpCodes.Ldind_R4); break;
				case TypeCode.Double: 
					IL.Emit(OpCodes.Ldind_R8); break;
				case TypeCode.SByte: 
					IL.Emit(OpCodes.Ldind_I1); break;				 
				case TypeCode.Int16: 
					IL.Emit(OpCodes.Ldind_I2); break;
				case TypeCode.Int32: 
					IL.Emit(OpCodes.Ldind_I4); break;
				case TypeCode.UInt64:
				case TypeCode.Int64: 
					IL.Emit(OpCodes.Ldind_I8); break;
				case TypeCode.Boolean:
				case TypeCode.Byte: 
					IL.Emit(OpCodes.Ldind_U1); break;
				case TypeCode.Char:
				case TypeCode.UInt16: 
					IL.Emit(OpCodes.Ldind_U2); break;
				case TypeCode.UInt32: 
					IL.Emit(OpCodes.Ldind_U4); break;
				default : 
					IL.Emit(OpCodes.Ldind_Ref); break;
			}
		}

		static	public void loadInt32(ILGenerator IL,Int32 v)
		{
			switch(v)
			{
				case 0:IL.Emit(OpCodes.Ldc_I4_0);
					break;
				case 1:IL.Emit(OpCodes.Ldc_I4_1);
					break;
				case 2:IL.Emit(OpCodes.Ldc_I4_2);
					break;
				case 3:IL.Emit(OpCodes.Ldc_I4_3);
					break;
				case 4:IL.Emit(OpCodes.Ldc_I4_4);
					break;
				case 5:IL.Emit(OpCodes.Ldc_I4_5);
					break;
				case 6:IL.Emit(OpCodes.Ldc_I4_6);
					break;
				case 7:IL.Emit(OpCodes.Ldc_I4_7);
					break;
				case 8:IL.Emit(OpCodes.Ldc_I4_8);
					break;
				default:IL.Emit(OpCodes.Ldc_I4_S, v);
					break;
			}
		}

		static	public void loadLocal(ILGenerator IL,int index)
		{
			switch(index)
			{
				case 0:IL.Emit(OpCodes.Ldloc_0);
					break;
				case 1:IL.Emit(OpCodes.Ldloc_1);
					break;
				case 2:IL.Emit(OpCodes.Ldloc_2);
					break;
				case 3:IL.Emit(OpCodes.Ldloc_3);
					break;
				default:IL.Emit(OpCodes.Ldloc_S, index);
					break;
			}
		}

		static	public void storeLocal(ILGenerator	IL,int index)
		{
			switch(index)
			{
				case 0:IL.Emit(OpCodes.Stloc_0);
					break;
				case 1:IL.Emit(OpCodes.Stloc_1);
					break;
				case 2:IL.Emit(OpCodes.Stloc_2);
					break;
				case 3:IL.Emit(OpCodes.Stloc_3);
					break;
				default:IL.Emit(OpCodes.Stloc_S, index);
					break;
			}
		}

		static	public	void	unbox(ILGenerator IL,Type type)
		{
			IL.Emit(OpCodes.Unbox, type);
			if (type.IsPrimitive) 
				if(type.IsEnum)
					IL.Emit(getPrimitiveBoxOpCode(Enum.GetUnderlyingType(type)));
				else
					IL.Emit(getPrimitiveBoxOpCode(type));
			else
				IL.Emit(OpCodes.Ldobj, type);
		}

		static public	OpCode	getPrimitiveBoxOpCode(Type type)
		{
			if(type==typeof(sbyte))  return  OpCodes.Ldind_I1;
			if(type==typeof(short))  return  OpCodes.Ldind_I2;
			if(type==typeof(int))    return  OpCodes.Ldind_I4;
			if(type==typeof(long))   return  OpCodes.Ldind_I8;
			if(type==typeof(byte))   return  OpCodes.Ldind_U1;
			if(type==typeof(ushort)) return  OpCodes.Ldind_U2;
			if(type==typeof(uint))   return  OpCodes.Ldind_U4;
			if(type==typeof(ulong))  return  OpCodes.Ldind_I8;
			if(type==typeof(float))  return  OpCodes.Ldind_R4;
			if(type==typeof(double)) return  OpCodes.Ldind_R8;
			if(type==typeof(char))   return  OpCodes.Ldind_U2;
			if(type==typeof(bool))   return  OpCodes.Ldind_I1;
			throw new ArgumentOutOfRangeException("no opcode for type {0}",type.ToString());
		}
	}
}

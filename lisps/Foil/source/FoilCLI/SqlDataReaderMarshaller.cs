/*
 * Created on Dec 10, 2004
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
using System.IO;
using System.Reflection;
using System.Data;
using System.Text;
/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil	
{
	/// <summary>
	/// Writes a SqlDataReader to an s-expr with the following format:
	/// The results are a list of result sets.  Each result set has a 
	/// the Columns names positioned to the ordinal of the data they represent
	/// within each row represented as a vector.
	/// (list	'((NAME DATETIME VALUE VALUETYPE DESCRIPTION CONFIGCATEGORYID) 
	///				(#("First row of data" NIL "C:EASTempLogs" "System.String" "Audit trail prefix" 3)
	///				 #("2nd row of data" NIL "C:EASTempLogs" "System.String" "Audit trail prefix" 3))))
	/// </summary>
	public class SqlDataReaderMarshaller:IMarshaller
	{
		public void marshall(Object o, TextWriter w, IBaseMarshaller baseMarshaller,int flags, int depth)  
		{
			if(o is IDataReader)
				throw new ArgumentOutOfRangeException(
							String.Format(	"Expected IDataReader in DataReaderMarshaller but got {0}"
											,o.GetType().ToString()));
			// Start the list of data sets
			w.Write("(:result ");
			// We have to close it when we are done.
			using(IDataReader	dr	=	(IDataReader)o)
			{
				do
				{
					// First write out the fields
					w.Write("(:result-set (:columns #(");
					for(int i=0;i<dr.FieldCount;++i)
						w.Write("((:name {0})(:type {1}))",dr.GetName(i),dr.GetDataTypeName(i));
					w.Write(")) ");
					// Write out the data as a list of vectors
					w.Write("(:rows ");
					// Each row...
					while(dr.Read())	
					{
						w.Write("#(");
						for(int i=0;i<dr.FieldCount;++i)
							baseMarshaller.marshallAtom(dr.GetValue(i),w,flags,depth);
						w.Write(") ");
					}
					w.Write("))");
				} while(dr.NextResult());
			}
			w.Write(")");
		}
	}
}

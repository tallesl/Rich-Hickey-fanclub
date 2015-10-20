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
/**
 * @author Eric Thorsen
 *
 */
namespace	com.richhickey.foil	
{
	public	sealed class IBaseMarshallerFlags	
	{
		public	const	int	MARSHALL_NONE	=	0;
		public	const	int	MARSHALL_ID		=	1;
		public	const	int	MARSHALL_TYPE	=	2;
		public	const	int	MARSHALL_HASH	=	4;
	};

	public interface	IBaseMarshaller
	{
			void marshallAtom(Object o,TextWriter w, int flags,int depth);
			bool canMarshallAsList(Object o);
			void marshallAsList(Object o,TextWriter w, int flags,int depth);
			void marshallAsVector(Object o,TextWriter w, int flags,int depth);
			IMarshaller findMarshallerFor(Type c);
	}
}

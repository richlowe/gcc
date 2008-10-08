/* translates tree gimple into Sun IR.
   Copyright (C) 2007, 2008 by Sun Microsystems, Inc. All rights reserved.  
   File is licensed under the GNU Public License.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _sgcc_ir_types_h
#define _sgcc_ir_types_h

typedef unsigned int PCC_TWORD;

#define PCC_UNDEF	((PCC_TWORD)0)
#define PCC_FARG	((PCC_TWORD)1)
#define PCC_CHAR	((PCC_TWORD)2)
#define PCC_SHORT	((PCC_TWORD)3)
#define PCC_INT	        ((PCC_TWORD)4)
#define PCC_LONG	((PCC_TWORD)5)
#define PCC_LLONG	((PCC_TWORD)6)
#define PCC_FLOAT	((PCC_TWORD)7)
#define PCC_DOUBLE	((PCC_TWORD)8)
#define PCC_STRTY	((PCC_TWORD)9)
#define PCC_UNIONTY	((PCC_TWORD)10)
#define PCC_ENUMTY	((PCC_TWORD)11)
#define PCC_MOETY	((PCC_TWORD)12)
#define PCC_BOOL	((PCC_TWORD)13)
#define PCC_UCHAR	((PCC_TWORD)14)
#define PCC_USHORT	((PCC_TWORD)15)
#define PCC_UNSIGNED	((PCC_TWORD)16)
#define PCC_ULONG	((PCC_TWORD)17)
#define PCC_ULLONG	((PCC_TWORD)18)
#define PCC_TVOID	((PCC_TWORD)19)
#define PCC_LDOUBLE	((PCC_TWORD)20)
#define PCC_FLOAT_COMPLEX	((PCC_TWORD)21)
#define PCC_DOUBLE_COMPLEX	((PCC_TWORD)22)
#define PCC_LDOUBLE_COMPLEX	((PCC_TWORD)23)
#define PCC_FLOAT_IMAGINARY	((PCC_TWORD)24)
#define PCC_DOUBLE_IMAGINARY	((PCC_TWORD)25)
#define PCC_LDOUBLE_IMAGINARY	((PCC_TWORD)26)

#define PCC_MINSTDTYPE	PCC_UNDEF	/* min standard basic type */
#define PCC_MAXSTDTYPE	PCC_LDOUBLE_IMAGINARY	/* max standard basic type KMC*/
#define PCC_MAXTYPE	((PCC_TWORD)31)	/* max basic type, period */

#define PCC_STDTYPE(t) ((unsigned)(t)<=PCC_MAXSTDTYPE)

/* type modifiers */

#define PCC_PTR 	((PCC_TWORD)0100)
#define PCC_FTN 	((PCC_TWORD)0200)
#define PCC_ARY 	((PCC_TWORD)0300)

/* add a most significant type modifier, m, to a type, t */
#define PCC_ADDTYPE(t,m) \
	((((t)&~PCC_BTMASK)<<PCC_TSHIFT) | (m) \
	| ((t)&PCC_BTMASK))

/* type packing constants */
 
#define PCC_TMASK	((PCC_TWORD)0300)	/* most significant qualifier */
#define PCC_BTMASK	((PCC_TWORD)077)	/* basic type */
#define PCC_BTSHIFT	((PCC_TWORD)6)		/* width of basic type field */
#define PCC_TSHIFT	((PCC_TWORD)2)		/* width of qualifiers */

/* operations on types */

#define PCC_MODTYPE(x,y) ((x)=((x)&(~PCC_BTMASK))|(y)) /* set x's basic type */
#define PCC_BTYPE(x) ((x)&PCC_BTMASK)  /* basic type of x */
#define PCC_UNSIGNABLE(x) ((x)<=PCC_LLONG&&(x)>=PCC_CHAR)
#define PCC_ENUNSIGN(x) ((x)+(PCC_UNSIGNED-PCC_INT))
#define PCC_DEUNSIGN(x) ((x)+(PCC_INT-PCC_UNSIGNED))
#define PCC_ISPTR(x) (((x)&PCC_TMASK)==PCC_PTR)
#define PCC_ISFTN(x) (((x)&PCC_TMASK)==PCC_FTN)

#define PCC_TVOIDPTR(x) ((x) == (PCC_PTR|PCC_TVOID)) /* (void*) */
#define PCC_ISARY(x) (((x)&PCC_TMASK)==PCC_ARY)
#define PCC_INCREF(x) PCC_ADDTYPE((x),PCC_PTR)
#define PCC_DECREF(x) ((((x)>>PCC_TSHIFT)&~PCC_BTMASK)|( (x)&PCC_BTMASK))

#define PCC_ISUNSIGNED(x) ((x)>=PCC_BOOL && (x)<=PCC_ULLONG)
#define PCC_ISINTEGRAL(x) (((x)>=PCC_CHAR && (x)<=PCC_LLONG) || PCC_ISUNSIGNED(x))
#define PCC_ISLONG(x) ((x)==PCC_LONG || (x)==PCC_ULONG)
#define PCC_ISLLONG(x) ((x)==PCC_LLONG || (x)==PCC_ULLONG)
#define PCC_ISFLOATING(x) (((x)==PCC_FLOAT || (x)==PCC_DOUBLE) || (x)==PCC_LDOUBLE)
#define PCC_ISCOMPLEX(x) ((x) == PCC_FLOAT_COMPLEX || \
			   (x) == PCC_DOUBLE_COMPLEX || \
			   (x) == PCC_LDOUBLE_COMPLEX)
#define PCC_ISIMAGINARY(x) ((x) == PCC_FLOAT_IMAGINARY || \
       		     (x) == PCC_DOUBLE_IMAGINARY || \
       		     (x) == PCC_LDOUBLE_IMAGINARY)
#define PCC_ISSTRTY(x)	((x) == PCC_STRTY)
#define PCC_ISCHAR(x)     ((x)==PCC_CHAR || (x)==PCC_UCHAR)

#define IR_BOOL	(PCC_MAXSTDTYPE+1)
#define IR_EXTENDEDF	(IR_BOOL+1)
#define IR_COMPLEX	(IR_EXTENDEDF+1)
#define IR_DCOMPLEX	(IR_COMPLEX+1)
#define IR_STRING	(IR_DCOMPLEX+1)
#define IR_LABELNO	(IR_STRING+1)
#define IR_BITFIELD	(IR_LABELNO+1)

/* new IR types: new type identifier for IR type tree node */
#define IR_ARRAY	(IR_BITFIELD+1)
#define IR_ARRAYDIM	(IR_ARRAY+1)
#define IR_POINTER	(IR_ARRAYDIM+1)
#define IR_FUNCTION	(IR_POINTER+1)
#define IR_ELLIPSIS	(IR_FUNCTION+1)

#define IR_FIRSTTYPE   IR_BOOL
#define IR_LASTTYPE    IR_ELLIPSIS

#define IR_ISINT(x)	   ((PCC_ISINTEGRAL(x) && !PCC_ISCHAR(x)) || (x)==IR_LABELNO)
#define IR_ISCHAR(x)	   (PCC_ISCHAR(x) || (x)==IR_STRING)
#define IR_ISPTRFTN(x)    (PCC_ISPTR(x) && PCC_ISFTN(PCC_DECREF(x)))

#endif /* _sgcc_ir_types_h */

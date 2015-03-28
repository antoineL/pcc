/*	$Id$	*/
/*
 * Copyright 2015 Anders Magnusson & Antoine Leca. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* Parts are copied from gdtoa.h, which includes the following: */
/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998-2000 by Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

****************************************************************/

#ifdef SOFTFLOAT
typedef struct softfloat /*{ // declared in node.h
	unsigned long long significand;
	signed short exponent;
	unsigned short kind;
}*/ SF;

enum {	/* SF.kind values; same as STRTODG_* values */
	SF_Zero		= 0,
	SF_Normal	= 1,
	SF_Denormal	= 2,
	SF_Infinite	= 3,
	SF_NaN		= 4, /* default quiet NaN */
	SF_NaNbits	= 5, /* (not used) */
	SF_NoNumber	= 6, /* signaling NaN */
	SF_kmask	= 7,

	/* The following may be or-ed into one of the above values. */
	SF_Neg		= 0x80, /* does not affect SFEXCP_Inex(lo|hi) */
	SFEXCP_Inexlo	= 0x100, /* returned result rounded toward zero */
	SFEXCP_Inexhi	= 0x200, /* returned result rounded away from zero */
	SFEXCP_Inexact	= 0x300,
	SFEXCP_Underflow= 0x400,
	SFEXCP_Overflow	= 0x800,
	SFEXCP_DivByZero= 0x1000,
	SFEXCP_Invalid	= 0x2000,

	SFEXCP_Aborted	= 0x8000, /* Not IEEE; operation not performed */
	SFEXCP_ALLmask	= 0xFF00 /* All exceptions (mask) */
};

typedef struct FPI {
	int nbits;
	int emin;
	int emax;
	int rounding;
	int sudden_underflow:1;
	int explicit_one:1; /* if MSB is explicitely stored */
	int has_inf_nan:1;  /* highest exponent means INF and NaN */
	int has_neg_zero:1;
	int has_radix_16:1;
	int storage;
	int exp_bias;
} FPI;

enum {	/* FPI.rounding values: same as FLT_ROUNDS */
	FPI_Round_zero = 0,	/* same meaning as FE_TOWARDZERO */
	FPI_Round_near = 1,	/* same meaning as FE_TONEAREST */
	FPI_Round_up = 2,	/* same meaning as FE_UPWARD */
	FPI_Round_down = 3,	/* same meaning as FE_DOWNWARD */
/* Warning: if adding new modes, keep same meaning for 2 low bits. */
	FPI_Round_near_from0 = 5, /* to nearest but ties up (Vax) */

	FPI_RoundNotSet = -4,	/* to implement dynamic rounding */
};

extern FPI * fpis[3], /* FLOAT, DOUBLE, LDOUBLE, respectively */
	/* IEEE754 binary formats, and their interchange format encodings: */
	fpi_binary32,
	fpi_binary64,
#ifndef notyet
	fpi_binary128,
#endif
	fpi_binary16,	/* IEEE 754:2008 "half-precision" */
	fpi_binaryx80;	/* usual IEEE double extended */
extern int sf_constrounding,
	sf_exceptions;

SF zerosf(int);
SF infsf(int);
SF nansf(int);
SF hugesf(int kind, TWORD);
CONSZ soft_signbit(SF);
SF soft_neg(SF);
SF soft_copysign(SF, SF);
SF soft_from_int(CONSZ, TWORD src_int_ty, TWORD dst_float_ty);
CONSZ soft_to_int(SF, TWORD);
SF soft_fcast(SF, TWORD);
SF soft_plus(SF, SF, TWORD);
SF soft_minus(SF, SF, TWORD);
SF soft_mul(SF, SF, TWORD);
SF soft_div(SF, SF, TWORD);
int soft_isz(SF);
int soft_isnan(SF);
int soft_fpclassify(SF, TWORD);
int soft_cmp_eq(SF, SF);
int soft_cmp_ne(SF, SF);
int soft_cmp_ge(SF, SF);
int soft_cmp_gt(SF, SF);
int soft_cmp_le(SF, SF);
int soft_cmp_lt(SF, SF);
int soft_cmp_unord(SF, SF);
int soft_pack(SF *, TWORD);

NODE * fcon2icon(NODE *);

NODE	*floatcon(char *);
#define fhexcon(s) floatcon(s)
#endif

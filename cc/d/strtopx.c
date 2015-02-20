/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998, 2000 by Lucent Technologies
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

/* Please send bug reports to David M. Gay (dmg at acm dot org,
 * with " at " changed at "@" and " dot " changed to ".").	*/

#include "gdtoaimp.h"

/* #include "gd_qnan.h" */
#define f_QNAN 0x7fc00000
#define d_QNAN0 0x0
#define d_QNAN1 0x7ff80000

#if 0 /* extracted from proposal pass1.h */

#ifndef f_QNAN
#define f_QNAN 0x7fc00000
#endif

#ifndef d_QNAN0
#if TARGET_ENDIAN == TARGET_LE
#define IEEE_LITTLE_ENDIAN
#define IEEE_8087
#define d_QNAN0 0x0
#define d_QNAN1 0x7ff80000
#define ldus_QNAN0 0x0
#define ldus_QNAN1 0x0
#define ldus_QNAN2 0x0
#define ldus_QNAN3 0xc000
#define ldus_QNAN4 0x7fff
/* 2 bytes of tail padding follow, per i386 ABI */
#if riscv
#define f_QNAN 0x7fe00000
#define d_QNAN0 0x0
#define d_QNAN1 0x7ffc0000
#endif/*riscv*/
#else
#define IEEE_BIG_ENDIAN
#define IEEE_MC68k
#define d_QNAN0 0x7ff80000
#define d_QNAN1 0x0
#ifndef __mc68010__
#define ld_QNAN0 0x7fff0000
#define ld_QNAN1 0x40000000
#define ld_QNAN2 0x0
#endif
#if sh3+mips
#define f_QNAN 0x7fa00000
#define d_QNAN0 0x7ff40000
#define d_QNAN1 0x0
#endif/*sh3,mips,hppa?*/
#endif
#endif/*d_QNANx*/

#endif


/* extern UShort NanDflt_ldus_D2A[5]; */
/* ## FROM g__fmt.c ## */
#ifndef ldus_QNAN0
#define ldus_QNAN0 0x7fff
#endif
#ifndef ldus_QNAN1
#define ldus_QNAN1 0xc000
#endif
#ifndef ldus_QNAN2
#define ldus_QNAN2 0
#endif
#ifndef ldus_QNAN3
#define ldus_QNAN3 0
#endif
#ifndef ldus_QNAN4
#define ldus_QNAN4 0
#endif

ULong NanDflt_Q_D2A[4] = { 0xffffffff, 0xffffffff, 0xffffffff, 0x7fffffff };
ULong NanDflt_d_D2A[2] = { d_QNAN1, d_QNAN0 };
ULong NanDflt_f_D2A[1] = { f_QNAN };
ULong NanDflt_xL_D2A[3] = { 1, 0x80000000, 0x7fff0000 };
UShort NanDflt_ldus_D2A[5] = { ldus_QNAN4, ldus_QNAN3, ldus_QNAN2, ldus_QNAN1, ldus_QNAN0 };
/* ##END## FROM g__fmt.c ## */

#undef _0
#undef _1

/* one or the other of IEEE_MC68k or IEEE_8087 should be #defined */

#ifdef IEEE_MC68k
#define _0 0
#define _1 1
#define _2 2
#define _3 3
#define _4 4
#endif
#ifdef IEEE_8087
#define _0 4
#define _1 3
#define _2 2
#define _3 1
#define _4 0
#endif

 int
strtopx(const char *s, char **sp, void *V)
{
	static FPI fpi0 = { 64, 1-16383-64+1, 32766 - 16383 - 64 + 1, 1, SI };
/*flt	static FPI fpi0 = { 24, 1-127-24+1,  254-127-24+1, 1, SI }; */
/*dbl	static FPI fpi0 = { 53, 1-1023-53+1, 2046-1023-53+1, 1, SI }; */
/*quad	static FPI fpi0 = { 113, 1-16383-113+1, 32766-16383-113+1, 1, SI }; */

	ULong bits[2];
	Long expt;
	int k;
	UShort *L = (UShort*)V;
#ifdef Honor_FLT_ROUNDS
/* #include "gdtoa_fltrnds.h" */
	FPI *fpi, fpi1;
	int Rounding;
#ifdef Trust_FLT_ROUNDS /*{{ only define this if FLT_ROUNDS really works! */
	Rounding = Flt_Rounds;
#else /*}{*/
	Rounding = 1;
	switch(fegetround()) {
	  case FE_TOWARDZERO:	Rounding = 0; break;
	  case FE_UPWARD:	Rounding = 2; break;
	  case FE_DOWNWARD:	Rounding = 3;
	  }
#endif /*}}*/
	fpi = &fpi0;
	if (Rounding != 1) {
		fpi1 = fpi0;
		fpi = &fpi1;
		fpi1.rounding = Rounding;
		}
/* ###END### #include "gdtoa_fltrnds.h" */
#else
#define fpi &fpi0
#endif

	k = strtodg(s, sp, fpi, &expt, bits);
	switch(k & STRTOG_Retmask) {
	  case STRTOG_NoNumber:
	  case STRTOG_Zero:
		L[0] = L[1] = L[2] = L[3] = L[4] = 0;
		break;

	  case STRTOG_Denormal:
		L[_0] = 0;
		goto normal_bits;

	  case STRTOG_Normal:
	  case STRTOG_NaNbits:
		L[_0] = expt + 0x3fff + 63;
 normal_bits:
		L[_4] = (UShort)bits[0];
		L[_3] = (UShort)(bits[0] >> 16);
		L[_2] = (UShort)bits[1];
		L[_1] = (UShort)(bits[1] >> 16);
		break;

	  case STRTOG_Infinite:
		L[_0] = 0x7fff;
		L[_1] = 0x8000;
		L[_2] = L[_3] = L[_4] = 0;
		break;

	  case STRTOG_NaN:
		L[_4] = NanDflt_ldus_D2A[0];
		L[_3] = NanDflt_ldus_D2A[1];
		L[_2] = NanDflt_ldus_D2A[2];
		L[_1] = NanDflt_ldus_D2A[3];
		L[_0] = NanDflt_ldus_D2A[4];
	  }
	if (k & STRTOG_Neg)
		L[_0] |= 0x8000;
	return k;
	}

#if 0 /* strtod */
	union { ULong L[2]; double d; } u;

	k = strtodg(s, sp, &fpi, &expt, bits);
	if (k == STRTOG_NoMemory) {
		errno = ERANGE;
		u.L[0] = Big0;
		u.L[1] = Big1;
		return u.d;
	}
	switch(k & STRTOG_Retmask) {
	  case STRTOG_NoNumber:
	  case STRTOG_Zero:
		u.L[0] = u.L[1] = 0;
		break;

	  case STRTOG_Normal:
		u.L[_1] = bits[0];
		u.L[_0] = (bits[1] & ~0x100000) | ((expt + 0x3ff + 52) << 20);
		break;

	  case STRTOG_Denormal:
		u.L[_1] = bits[0];
		u.L[_0] = bits[1];
		break;

	  case STRTOG_Infinite:
		u.L[_0] = 0x7ff00000;
		u.L[_1] = 0;
		break;

	  case STRTOG_NaN:
		u.L[0] = d_QNAN0;
		u.L[1] = d_QNAN1;
		break;

	  case STRTOG_NaNbits:
		u.L[_0] = 0x7ff00000 | bits[1];
		u.L[_1] = bits[0];
	  }
	if (k & STRTOG_Neg)
		u.L[_0] |= 0x80000000L;
	return u.d;
#endif
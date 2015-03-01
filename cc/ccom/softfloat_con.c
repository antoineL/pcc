/*	$Id$	*/

/*
 * Copyright (c) 2008 Anders Magnusson. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
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

#define SOFTFLOAT_CONSTANT 1
#ifdef SOFTFLOAT_CONSTANT

#include "pass1.h"
#include <assert.h>

#ifndef Long
#define Long int
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef UShort
/* XXX still useful? */
typedef unsigned short UShort;
#endif

int strtodg (const char*, char**, FPI*, Long*, ULong*);
int strhextodg (const char*, char**, FPI*, Long*, ULong*);

/* IEEE binary formats, and their interchange format encodings */
FPI fpi_binary16 = { 11, 1-15-11+1,  30-15-11+1, 1, 0,
           0, 1, 1,  16,   15+11-1 };
FPI fpi_binary32 = { 24, 1-127-24+1,  254-127-24+1, 1, 0,
           0, 1, 1,  32,   127+24-1 };
FPI fpi_binary64 = { 53, 1-1023-53+1, 2046-1023-53+1, 1, 0,
           0, 1, 1,  64,   1023+53-1 };
#ifndef notyet
FPI fpi_binary128 = { 113, 1-16383-113+1, 32766-16383-113+1, 1, 0,
           0, 1, 1,   128,   16383+113-1 };
#endif
/* IEEE double extended in its usual form, for example Intel 387 */
FPI fpi_binaryx80 = { 64, 1-16383-64+1, 32766-16383-64+1, 1, 0,
           1, 1, 1,   80,   16383+64-1 };

#if defined(_MSC_VER) && _MSC_VER<=1600
#define TS(x)
#else
#define TS(x) [x-FLOAT] =
#endif
FPI * fpis[3] = {
	TS(FLOAT)	&FPI_FLOAT,
	TS(DOUBLE)	&FPI_DOUBLE,
	TS(LDOUBLE)	&FPI_LDOUBLE
};

/*
 * Prepare a SF value for use in packed (interchange) format.
 * Expect a correctly rounded (for the target type) representation.
 * Returns the combined sign+exponent part, ready to be shifted.
 *
 * The .significand bits are left in place, ready to be used by
 * the endian-aware code. The MSB one is still there.
 * SF_NaN is expanded into the IEEE754:2008 default representation
 * (1 as most significant bit, rest all zeroes); if not appropriate
 * for the target, the calling code should replace it with SF_NaNbits
 * with the adequate bits into the .significand member.
 */
int
packIEEE(SF *psf, FPI *fpi)
{
	int biasedexp;

	/* XXX normalize? */
	biasedexp = psf->exponent + fpi->exp_bias;
	switch(psf->kind & SF_kmask) {
	  case SF_Zero:
		psf->significand = 0;
		biasedexp = 0;
		break;

	  case SF_Normal:
		assert((psf->significand >> (fpi->nbits-1)) == 1);
		assert(psf->exponent >= fpi->emin);
		assert(psf->exponent <= fpi->emax);
		break;

	  case SF_Denormal:
		assert(! fpi->sudden_underflow);
		assert((psf->significand >> (fpi->nbits-1)) == 0);
		assert(psf->exponent == fpi->emin);
		biasedexp = 0;
		break;

	  case SF_Infinite:
		assert(fpi->has_inf_nan);
		psf->significand = 1ull << (fpi->nbits-1);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;

	  case SF_NoNumber:
		/* Can it happen? Debug_Warns? ICE? */
		/* Let decay as NaN */
	  case SF_NaN:
		psf->significand = 1ull << (fpi->nbits-2);
		/* FALLTHROUGH */
	  case SF_NaNbits:
		assert(fpi->has_inf_nan);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;
	}
	if (psf->kind & SF_Neg)
		biasedexp |= 1 << (fpi->storage - fpi->nbits - fpi->explicit_one);
	return biasedexp;

}

static char*
suffix(char *str, int *im, TWORD *tw)
{
	char *suf;

	*im = 0;
	*tw = DOUBLE;
	suf = str + strlen(str) - 1;
	if (*suf == 'i') {
		*im = 1;
		--suf;
	}
	switch (*suf) {
	case 'f':
	case 'F':
		*tw = FLOAT;
		break;
	case 'l':
	case 'L':
#ifdef notyet
		/* Needed to properly support e.g. _Generic() */
		*tw = LDOUBLE;
#else
		/* Some MD backends are assuming LDOUBLE cannot happen... */
		*tw = ctype(LDOUBLE);
#endif
		break;
	default:
		return suf+1;
	}
	if (suf[-1] == 'i') {
		++*im;
		--suf;
	}
	return suf;
}

static NODE *
f3(char *str, int (*strtodg_p)())
{
	TWORD tw;
	NODE *p;
	char *sfx, *eptr;
	FPI *fpi;
	ULong bits[2];
	Long expt;
	int k, im;

	sfx = suffix(str, &im, &tw);
	fpi = fpis[tw-FLOAT];
	k = (*strtodg_p)(str, &eptr, fpi, &expt, bits);
	if (eptr != sfx)
		uerror("Botch in floatcon, sfx-eptr=%d", (int)(sfx-eptr));
	if (k & SFEXCP_Overflow)
		werror("Overflow in floating-point constant");
#ifndef NO_COMPLEX
	if (im)
		tw += (FIMAG-FLOAT);
#endif
	p = block(FCON, NIL, NIL, tw, 0, 0);
	p->n_dcon.kind = k;
	p->n_dcon.significand = bits[0];
	if (fpi->nbits > 32)
		p->n_dcon.significand |= ((U_CONSZ)bits[1] << 32);
#if 0
	switch(k & SF_kmask) {
	  case SF_Zero:
		p->n_dcon.exponent = p->n_dcon.significand = 0;
		break;

	  case SF_Denormal:
		/* XXX always expt=emin? */
	  case SF_Normal:
		p->n_dcon.exponent = expt;
		break;

	  case SF_Infinite:
		p->n_dcon.significand = 1ull << (fpi->nbits-1);
		/* FALLTHROUGH */
	  case SF_NaNbits:
		p->n_dcon.exponent = fpi->emax+1;
		break;

	  case SF_NaN:
		p->n_dcon.significand = 1ull << (fpi->nbits-2);
		p->n_dcon.exponent = fpi->emax+1;
		break;
	}
#else
	p->n_dcon.exponent = expt;
#endif
	return p;
}

NODE *
floatcon(char *str)
{
	return f3(str, strtodg);
}

NODE *
fhexcon(char *str)
{
	return f3(str, strhextodg);
}
#endif

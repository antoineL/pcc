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

#ifdef SOFTFLOAT

#include "pass1.h"
#ifndef PCC_DEBUG
#define assert(e) ((void)0)
#else
#define assert(e) (!(e)?cerror("assertion failed " #e " at softfloat:%d",__LINE__):(void)0)
#endif
/*
 * Floating point emulation, to not depend on the characteristics (and bugs)
 * of the host floating-point implementation when compiling.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */

#ifdef FDFLOAT

/*
 * Supports F- and D-float, used in DEC machines.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */

/*
 * Useful macros to manipulate the float.
 */
#define DSIGN(w)	(((w).fd1 >> 15) & 1)
#define DSIGNSET(w,s)	((w).fd1 = (s << 15) | ((w).fd1 & 077777))
#define DEXP(w)		(((w).fd1 >> 7) & 0377)
#define DEXPSET(w,e)	((w).fd1 = (((e) & 0377) << 7) | ((w).fd1 & 0100177))
#define DMANTH(w)	((w).fd1 & 0177)
#define DMANTHSET(w,m)	((w).fd1 = ((m) & 0177) | ((w).fd1 & 0177600))

typedef unsigned int lword;
typedef unsigned long long dword;

#define MAXMANT 0x100000000000000LL

/*
 * Returns a zero dfloat.
 */
static SF
nulldf(void)
{
	SF rv;

	rv.fd1 = rv.fd2 = rv.fd3 = rv.fd4 = 0;
	return rv;
}

/*
 * Convert a (u)longlong to dfloat.
 * XXX - fails on too large (> 55 bits) numbers.
 */
SF
soft_cast(CONSZ ll, TWORD t)
{
	int i;
	SF rv;

	rv = nulldf();
	if (ll == 0)
		return rv;  /* fp is zero */
	if (ll < 0)
		DSIGNSET(rv,1), ll = -ll;
	for (i = 0; ll > 0; i++, ll <<= 1)
		;
	DEXPSET(rv, 192-i);
	DMANTHSET(rv, ll >> 56);
	rv.fd2 = ll >> 40;
	rv.fd3 = ll >> 24;
	rv.fd4 = ll >> 8;
	return rv;
}

/*
 * multiply two dfloat. Use chop, not round.
 */
SF
soft_mul(SF p1, SF p2)
{
	SF rv;
	lword a1[2], a2[2], res[4];
	dword sum;

	res[0] = res[1] = res[2] = res[3] = 0;

	/* move mantissa into lwords */
	a1[0] = p1.fd4 | (p1.fd3 << 16);
	a1[1] = p1.fd2 | DMANTH(p1) << 16 | 0x800000;

	a2[0] = p2.fd4 | (p2.fd3 << 16);
	a2[1] = p2.fd2 | DMANTH(p2) << 16 | 0x800000;

#define MULONE(x,y,r) sum += (dword)a1[x] * (dword)a2[y]; sum += res[r]; \
	res[r] = sum; sum >>= 32;

	sum = 0;
	MULONE(0, 0, 0);
	MULONE(1, 0, 1);
	res[2] = sum;
	sum = 0;
	MULONE(0, 1, 1);
	MULONE(1, 1, 2);
	res[3] = sum;

	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(p1) ^ DSIGN(p2));
	DEXPSET(rv, DEXP(p1) + DEXP(p2) - 128);
	if (res[3] & 0x8000) {
		res[3] = (res[3] << 8) | (res[2] >> 24);
		res[2] = (res[2] << 8) | (res[1] >> 24);
	} else {
		DEXPSET(rv, DEXP(rv) - 1);
		res[3] = (res[3] << 9) | (res[2] >> 23);
		res[2] = (res[2] << 9) | (res[1] >> 23);
	}
	DMANTHSET(rv, res[3] >> 16);
	rv.fd2 = res[3];
	rv.fd3 = res[2] >> 16;
	rv.fd4 = res[2];
	return rv;
}

SF
soft_div(SF t, SF n)
{
	SF rv;
	dword T, N, K;
	int c;

#define SHL(x,b) ((dword)(x) << b)
	T = SHL(1,55) | SHL(DMANTH(t), 48) |
	    SHL(t.fd2, 32) | SHL(t.fd3, 16) | t.fd4;
	N = SHL(1,55) | SHL(DMANTH(n), 48) |
	    SHL(n.fd2, 32) | SHL(n.fd3, 16) | n.fd4;

	c = T > N;
	for (K = 0; (K & 0x80000000000000ULL) == 0; ) {
		if (T >= N) {
			T -= N;
			K |= 1;
		}
		T <<= 1;
		K <<= 1;
	}
	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(t) ^ DSIGN(n));
	DEXPSET(rv, DEXP(t) - DEXP(n) + 128 + c);
	DMANTHSET(rv, K >> 48);
	rv.fd2 = K >> 32;
	rv.fd3 = K >> 16;
	rv.fd4 = K;
	return rv;
}

/*
 * Negate a float number. Easy.
 */
SF
soft_neg(SF sf)
{
	int sign = DSIGN(sf) == 0;
	DSIGNSET(sf, sign);
	return sf;
}

/*
 * Return true if fp number is zero.
 */
int
soft_isz(SF sf)
{
	return (DEXP(sf) == 0);
}

int
soft_cmp_eq(SF x1, SF x2)
{
	cerror("soft_cmp_eq");
	return 0;
}

int
soft_cmp_ne(SF x1, SF x2)
{
	cerror("soft_cmp_ne");
	return 0;
}

int
soft_cmp_le(SF x1, SF x2)
{
	cerror("soft_cmp_le");
	return 0;
}

int
soft_cmp_lt(SF x1, SF x2)
{
	cerror("soft_cmp_lt");
	return 0;
}

int
soft_cmp_ge(SF x1, SF x2)
{
	cerror("soft_cmp_ge");
	return 0;
}

int
soft_cmp_gt(SF x1, SF x2)
{
	cerror("soft_cmp_gt");
	return 0;
}

/*
 * Convert a fp number to a CONSZ.
 */
CONSZ
soft_val(SF sf)
{
	CONSZ mant;
	int exp = DEXP(sf) - 128;

	mant = SHL(1,55) | SHL(DMANTH(sf), 48) |
            SHL(sf.fd2, 32) | SHL(sf.fd3, 16) | sf.fd4;

	while (exp < 0)
		mant >>= 1, exp++;
	while (exp > 0)
		mant <<= 1, exp--;
	return mant;
}

SF
soft_plus(SF x1, SF x2)
{
	cerror("soft_plus");
	return x1;
}

SF
soft_minus(SF x1, SF x2)
{
	cerror("soft_minus");
	return x1;
}

/*
 * Convert a hex constant to floating point number.
 */
NODE *
fhexcon(char *s)
{
	cerror("fhexcon");
	return NULL;
}

/*
 * Convert a floating-point constant to D-float and store it in a NODE.
 */
NODE *
floatcon(char *s)
{
	NODE *p;
	dword mant;
	SF fl, flexp, exp5;
	int exp, negexp, bexp;

	exp = 0;
	mant = 0;
#define ADDTO(sum, val) sum = sum * 10 + val - '0'
	for (; *s >= '0' && *s <= '9'; s++) {
		if (mant<MAXMANT)
			ADDTO(mant, *s);
		else
			exp++;
	}
	if (*s == '.') {
		for (s++; *s >= '0' && *s <= '9'; s++) {
			if (mant<MAXMANT) {
				ADDTO(mant, *s);
				exp--;
			}
		}
	}

	if ((*s == 'E') || (*s == 'e')) {
		int eexp = 0, sign = 0;
		s++;
		if (*s == '+')
			s++;
		else if (*s=='-')
			sign = 1, s++;

		for (; *s >= '0' && *s <= '9'; s++)
			ADDTO(eexp, *s);
		if (sign)
			eexp = -eexp;
		exp = exp + eexp;
	}

	negexp = 1;
	if (exp<0) {
		negexp = -1;
		exp = -exp;
	}


	flexp = soft_cast(1, INT);
	exp5 = soft_cast(5, INT);
	bexp = exp;
	fl = soft_cast(mant, INT);

	for (; exp; exp >>= 1) {
		if (exp&01)
			flexp = soft_mul(flexp, exp5);
		exp5 = soft_mul(exp5, exp5);
	}
	if (negexp<0)
		fl = soft_div(fl, flexp);
	else
		fl = soft_mul(fl, flexp);

	DEXPSET(fl, DEXP(fl) + negexp*bexp);
	p = block(FCON, NIL, NIL, DOUBLE, 0, 0); /* XXX type */
	p->n_dcon = fl;
	return p;
}
#else

/*
 * Use parametric floating-point representation, as used in the package gdtoa
 * published by David M. Gay and generally available as gdtoa.tgz at
 * http://www.netlib.org/fp/ ; see also strtodg.c introduction.
 *
 * Arithmetic characteristics are described in struct FPI (explained below);
 * the actual numbers are represented (stored) in struct SF.
 * Floating-point numbers have fpi->nbits bits.
 * These numbers are regarded as integers multiplied by 2^e
 * (i.e., 2 to the power of the exponent e), where e is stored in
 * *exp by strtodg.  The minimum and maximum exponent values fpi->emin
 * and fpi->emax for normalized floating-point numbers reflect this
 * arrangement.  For example, the IEEE 754 standard for binary arithmetic
 * specifies doubles (also known as binary64) as having 53 bits, with
 * normalized values of the form 1.xxxxx... times 2^(b-1023), with 52 bits
 * (the x's) and the biased exponent b represented explicitly;
 * b is an unsigned integer in the range 1 <= b <= 2046 for normalized
 * finite doubles, b = 0 for denormals, and b = 2047 for Infinities and NaNs.
 * To turn an IEEE double into the representation used here, we multiply
 * 1.xxxx... by 2^52 (to make it an integer) and reduce the exponent
 * e = (b-1023) by 52:
 *	fpi->emin = 1 - 1023 - 52
 *	fpi->emax = 1046 - 1023 - 52
 * For fpi_binary64 initialization, we actually write -53 + 1 rather than
 * -52, to emphasize that there are 53 bits including one implicit bit which
 * is at the left of the binary point.
 * Field fpi->rounding indicates the desired rounding direction, with
 * possible values
 *	FPI_Round_zero = toward 0,
 *	FPI_Round_near = unbiased rounding -- the IEEE default,
 *	FPI_Round_up = toward +Infinity, and
 *	FPI_Round_down = toward -Infinity
 * given in pass1.h.
 *
 * Field fpi->sudden_underflow indicates whether computations should return
 * denormals or flush them to zero.  Normal floating-point numbers have
 * bit fpi->nbits in the significand on.  Denormals have it off, with
 * exponent = fpi->emin.
 *
 * Fields fpi->explicit_one, fpi->storage, and fpi->exp_bias are only
 * relevant when the number are finally packed into interchange format.
 * 
 * Some architectures do not use IEEE arithmetic but can nevertheless use
 * the same parametrization. They should provide their own FPI objects.
 * Fields fpi->has_inf_nan and fpi->has_neg_zero cover the non-IEEE cases
 * of lacking respectively the use of infinities and NaN, and negative zero.
 *
 * In this implementation, the bits are stored in one large integer
 * (unsigned long long); this limits the number of bits to 64.
 *
 * XXX - assumes that:
 *	- (obviously) 2's complement
 *	- long long is (at least) 64 bits
 *	- CONSZ is (at least) 64 bits
 */

/*
 * API restrictions:
 *	- type information should be between FLOAT and LDOUBLE
 * XXX	- operations (+, -, *, /) miss the type
 * XXX	- missing complex * and /
 */

/* XXX explanations about FLT_EVAL_METHOD */
#if defined(TARGET_FLT_EVAL_METHOD) && TARGET_FLT_EVAL_METHOD > 0
#define	EVAL_TYPE(t)	((t)-FLOAT > TARGET_FLT_EVAL_METHOD ? t : \
				TARGET_FLT_EVAL_METHOD+FLOAT)
#else
#define	EVAL_TYPE(t)	(t)
#endif

#ifndef Long
#define Long int
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef ULLong
typedef unsigned long long ULLong;
#endif

#define ONEZEROES(n)	(1ull << (n))
#define ONES(n) 	(ONEZEROES(n) | (ONEZEROES(n)-1))

/* XXX #define WORKBITS	((int)sizeof(ULLong) * 8) */
#define WORKBITS	64
#define NORMALMANT	ONEZEROES(WORKBITS-1)

#define SF_NEG(sf)	((sf).kind ^= SF_Neg, sf)
#define SF_ROUND(sf, t)	(round_extra(sf, 0, t))

typedef struct DULLong {
	ULLong hi, lo;
} DULLong;

int strtodg (const char*, char**, FPI*, Long*, ULong*);

static SF zerosf(int);
static SF infsf(int);
static SF nansf(void);
static SF hugesf(int, FPI *);
static DULLong rshiftd_rndodd(ULLong, ULLong, int);
static DULLong lshiftd(ULLong, ULLong, int);
static SF round_extra(SF, ULLong, TWORD);

/* IEEE binary formats, and their interchange format encodings */
FPI fpi_binary16 = { 11, 1-15-11+1,
                        30-15-11+1, 1, 0,
        0, 1, 1, 0,  16,   15+11-1 };
FPI fpi_binary32 = { 24,  1-127-24+1,
                        254-127-24+1, 1, 0,
        0, 1, 1, 0,  32,    127+24-1 };
FPI fpi_binary64 = { 53,   1-1023-53+1,
                        2046-1023-53+1, 1, 0,
        0, 1, 1, 0,  64,     1023+53-1 };
#ifndef notyet
FPI fpi_binary128 = { 113,   1-16383-113+1,
                         32766-16383-113+1, 1, 0,
        0, 1, 1, 0,   128,     16383+113-1 };
#endif
/* IEEE double extended in its usual form, for example Intel 387 */
FPI fpi_binaryx80 = { 64,   1-16383-64+1,
                        32766-16383-64+1, 1, 0,
        1, 1, 1, 0,   80,     16383+64-1 };

#ifndef FPI_FLOAT
#define FPI_FLOAT	fpi_binary32
#endif
#ifndef FPI_DOUBLE
#define FPI_DOUBLE	fpi_binary64
#endif
#ifndef FPI_LDOUBLE
#define FPI_LDOUBLE	FPI_DOUBLE
#endif

FPI * fpis[3] = {
	&FPI_FLOAT,
	&FPI_DOUBLE,
	&FPI_LDOUBLE
};

/*
 * Returns a (signed) zero softfloat.
 */
static SF
zerosf(int neg)
{
	SF rv;

	assert(SF_Zero == 0);
	rv.significand = rv.exponent = rv.kind = 0;
	if (neg) rv.kind |= SF_Neg;
	return rv;
}

/*
 * Returns a (signed) infinite softfloat.
 */
static SF
infsf(int neg)
{
	SF rv;

/* XXX this is the result of an "division by zero" operation; warns? */
/* XXX what to do if the platform does not handle Inf? */
	rv.kind = SF_Infinite;
	if (neg) rv.kind |= SF_Neg;
	rv.significand = NORMALMANT;
	rv.exponent = 24576;
	return rv;
}

/*
 * Returns a NaN softfloat.
 */
static SF
nansf(void)
{
	SF rv;

/* XXX this is the result of an "invalid" operation; warns? */
/* XXX what to do if the platform does not handle NaN, and source has 0/0? */
	rv.kind = SF_NaN;
	rv.significand = 3 * ONEZEROES(WORKBITS-2);
	rv.exponent = 24576;
	return rv;
}

/*
 * Returns a (signed) huge but finite softfloat.
 * "kind" (sign, exceptions) is merged into.
 */
static SF
hugesf(int kind, FPI *fpi)
{
	SF rv;

	rv.kind = (kind & ~SF_kmask) | SF_Normal;
	rv.significand = ONES(fpi->nbits - 1);
	rv.exponent = fpi->emax;
	return rv;
}

/*
 * The algorithms for the operations were derived from John Hauser's 
 * SoftFloat package (which is also used in libpcc/libsoftfloat.) 
 *
 * Results are rounded to odd ("jamming" in John Hauser's code)
 * in order to avoid double rounding errors. See
 *	Sylvie Boldo, Guillaume Melquiond, "When double rounding is odd",
 *	Research report No 2004-48, Normale Sup', Lyon, Nov 2004;
 *	http://www.ens-lyon.fr/LIP/Pub/Rapports/RR/RR2004/RR2004-48.pdf
 *	17th IMACS World Congress, Paris, Jul 2005; pp.11;
 *	http://hal.inria.fr/inria-00070603/document
 */

/*
 * Shift right double, rounding to odd.
 */
static DULLong
rshiftd_rndodd(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert(count >= 0);
	if ((unsigned)count >= 2*WORKBITS) {
		z.hi = 0;
		z.lo = (a != 0) | (b != 0);
	}
	else if (count >= WORKBITS) {
		z.hi = 0;
		z.lo = (a >> (count - WORKBITS)) | (b != 0);
	}
	else {
		z.hi = a >> count;
		z.lo = (a << (WORKBITS - count)) | (b >> count) | (b != 0);
	}
	return z;
}

/*
 * Shift left double.
 */
static DULLong
lshiftd(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert((unsigned)count < WORKBITS);
	z.hi = a << count;
	z.lo = (a >> (WORKBITS-count)) | (b << count);
	return z;
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
typedef int bool;

static SF
round_extra(SF sf, ULLong extra, TWORD t)
{
	FPI *fpi;
	ULLong minmant, mant, maxmant;
	DULLong z;
	int exp = sf.exponent, excess, doinc, rd;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
		if (! fpi->has_neg_zero)
			sf.kind &= ~SF_Neg;
		/* FALLTHROUGH */
	  default:
		return sf;
	  case SF_Infinite:
		return fpi->has_inf_nan ? sf : hugesf(sf.kind, fpi);
	  case SF_NaNbits:
		cerror("Unexpected softfloat NaNbits");
		sf.kind -= SF_NaNbits - SF_NaN;
		return sf;
	  case SF_Denormal:
		if (exp != fpi->emin || extra) {
			assert(SF_Denormal > SF_Normal);
			sf.kind -= SF_Denormal - SF_Normal;
		}
		break;
	  case SF_Normal:
		break;
	}

	maxmant = ONES(fpi->nbits - 1);
	if (fpi->has_radix_16)
		minmant = ONEZEROES(fpi->nbits - 4);
	else
		minmant = ONEZEROES(fpi->nbits - 1);
	assert(sf.significand);
	excess = 0;
	if (sf.significand < minmant) {
		/* Not normalized */
		if ((sf.kind & SF_kmask) != SF_Denormal) {
			mant = sf.significand;
			for (; mant < minmant; --excess)
				mant <<= 1;
		}
	}
	else {
		if ((sf.kind & SF_kmask) == SF_Denormal)
			sf.kind -= SF_Denormal - SF_Normal;
		if ((sf.significand & ~maxmant) != 0) {
			/* More bits than allowed in significand */
			if (sf.significand & NORMALMANT)
				excess = WORKBITS - fpi->nbits;
			else {
				mant = sf.significand;
				for (; mant > maxmant; ++excess)
					mant >>= 1;
			}
		}
	}
	if (fpi->has_radix_16 && (exp + excess) & 3)
		excess += 3 - ((exp + excess) & 3);
	if (excess) {
		if (excess < 0)
			z = lshiftd(sf.significand, extra, -excess);
		else
			z = rshiftd_rndodd(sf.significand, extra, excess);
		sf.significand = z.hi;
		extra = z.lo;
		exp += excess;
	}
	assert((sf.kind & SF_kmask) == SF_Denormal ? sf.significand < minmant
	    : (sf.significand & ~(minmant-1)) == minmant || fpi->has_radix_16);

/* XXX check limit cases (emin, emin-1, emin-2) to avoid double rounding... */

	rd = fpi->rounding;
	if (sf.kind & SF_Neg && rd == FPI_Round_down)
		rd = FPI_Round_up;
	else if (sf.kind & SF_Neg && rd == FPI_Round_up)
		rd = FPI_Round_down;
	if (extra != 0) {
		doinc = rd == FPI_Round_up;
		if (rd == FPI_Round_near && extra == NORMALMANT)
			doinc = sf.significand & 1;
		else if ((rd & 3) == FPI_Round_near && extra >= NORMALMANT)
			doinc = 1;
/* XXX set SFEXCP_Inex(hi|lo) ? */
		if (doinc) {
			if (sf.significand == maxmant) {
				sf.significand = minmant;
				++exp;
			}
			else
				sf.significand++;
		}
	}
	else doinc = 0;

	if (exp < fpi->emin) {
		if (fpi->sudden_underflow || exp < fpi->emin - fpi->nbits) {
			sf = zerosf(sf.kind & SF_Neg);
			sf.kind |= SFEXCP_Inexlo | SFEXCP_Underflow;
			return sf;
		}
		if (doinc) {
			if (sf.significand == minmant) {
				sf.significand = maxmant;
				--exp;
			}
			else
				sf.significand--;
		}
		excess = fpi->emin - exp;
		z = rshiftd_rndodd(sf.significand, extra, excess);
		doinc = rd == FPI_Round_up;
		if ((rd & 3) == FPI_Round_near) {
			if (z.lo > NORMALMANT)
				doinc = 1;
			else if (rd == FPI_Round_near && z.lo == NORMALMANT)
				doinc = z.hi & 1;
			else if (rd == FPI_Round_near_ties_up && z.lo == NORMALMANT)
				doinc = 1;
		}
		if (doinc) z.hi++;
		if (z.hi) {
			sf.significand = z.hi;
			sf.kind += SF_Denormal - SF_Normal;
			exp = fpi->emin;
		}
		else {
			sf = zerosf(sf.kind & SF_Neg);
			exp = 0;
		}
		sf.kind |= SFEXCP_Underflow;
	}
	else if (exp > fpi->emax) {
		sf.kind |= SFEXCP_Overflow | SFEXCP_Inexhi;
		if (! fpi->has_inf_nan || rd == FPI_Round_zero || 
		    rd == FPI_Round_down) {
			sf.significand = maxmant;
			exp = fpi->emax;
		}
		else {
			sf.kind = SF_Infinite | (sf.kind & ~SF_kmask);
			sf.significand = minmant;
			exp = fpi->emax+1;
		}
	}
	sf.exponent = exp;
	return sf;
}

/*
 * Conversions.
 */

/*
 * Convert from integer type f to floating-point type t.
 * Rounds correctly to the target type (subject to FLT_EVAL_METHOD.)
 */
SF
soft_from_int(CONSZ ll, TWORD f, TWORD t)
{
	SF rv;

	rv = zerosf(0);
	if (ll == 0)
		return rv;  /* fp is zero */
	rv.kind = SF_Normal;
	if (!ISUNSIGNED(f) && ll < 0)
		rv.kind |= SF_Neg, ll = -ll;
	rv.significand = ll; /* rv.exponent already 0 */
/* XXX warning if SFEXCP_Inex? */
	return SF_ROUND(rv, EVAL_TYPE(t));
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
SF
soft_cast(SF sf, TWORD t)
{
/* XXX warning if SFEXCP_Underflow/Overflow ? */
	return SF_ROUND(sf, t);
}

/*
 * Convert a fp number to a CONSZ. Always chop toward zero.
 * XXX Should warns correctly if out-of-range.
 */
CONSZ
soft_to_int(SF sf, TWORD t)
{
	ULLong mant;
	int exp = sf.exponent;

	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
	  case SF_Denormal:
		return 0;

	  case SF_Normal:
		if (exp < - WORKBITS - 1)
			return 0;
		if (exp < WORKBITS)
			break;
		/* FALLTHROUGH */
	  case SF_Infinite:
		/* Officially entering undefined behaviour! */
		uerror("Conversion of huge FP constant into integer");
		/* FALLTHROUGH */

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* FALLTHROUGH */
	  case SF_NaN:
	  case SF_NaNbits:
		uerror("Undefined FP conversion into an integer, replaced with 0");
		return 0;
	}
	mant = sf.significand;
	while (exp < 0)
/* XXX check overflow */
		mant >>= 1, exp++;
	while (exp > 0)
		mant <<= 1, exp--;
	if (sf.kind & SF_Neg)
		mant = -(long long)mant;
	return mant;
}

/*
 * Operations.
 */

/*
 * Negate a softfloat. Easy.
 * XXX Do not work correctly for SF_Zero when negative zero are not supported.
 *     Need to have access to fpi-> (i.e. TWORD t) to solve this.
 */
SF
soft_neg(SF sf)
{
	sf.kind ^= SF_Neg;
	return sf;
}

/*
 * Add two numbers of same sign.
 * The devil is in the details, like those negative zeroes...
 */
static SF
soft_add(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (soft_isinf(x1))
		return x1;
	if (soft_isinf(x2))
		return x2;
	if (soft_isz(x2)) /* catches all signed zeroes, delivering x1 */
		return x1;
	if (soft_isz(x1))
		return x2;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, x1.exponent--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, x2.exponent--;
	if (x1.exponent - WORKBITS > x2.exponent)
		return x1;
	if (x2.exponent - WORKBITS > x1.exponent)
		return x2;
	diff = x1.exponent - x2.exponent;
	if (diff < 0) {
		rv = x2;
		z = rshiftd_rndodd(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftd_rndodd(x2.significand, 0, diff );
	}
	if ((rv.kind & SF_kmask) == SF_Denormal)
		rv.kind -= SF_Denormal - SF_Normal;
	rv.significand += z.hi;
	if (rv.significand < NORMALMANT) {
		/* target mantissa overflows */
		z = rshiftd_rndodd(rv.significand, z.lo, 1);
		rv.significand = z.hi | NORMALMANT;
		++rv.exponent;
	}
	return round_extra(rv, z.lo, t);
}

/*
 * Substract two positive numbers.
 * Special hack when the type number t being negative, to indicate
 * that rounding rules should be reversed (because the operands were.)
 */
static SF
soft_sub(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (soft_isinf(x1) && soft_isinf(x2))
/* XXX "invalid"; warns? */
		return nansf();
	if (soft_isinf(x1))
		return x1;
	if (soft_isinf(x2))
		return SF_NEG(x2);
	if (soft_isz(x2)) /* catches 0 - 0, delivering +0 */
		return x1;
	if (soft_isz(x1))
		return SF_NEG(x2);
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, x1.exponent--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, x2.exponent--;
	if (x1.exponent - WORKBITS > x2.exponent)
		return x1;
	if (x2.exponent - WORKBITS > x1.exponent)
		return SF_NEG(x2);
	diff = x1.exponent - x2.exponent;
	if (diff == 0 && x1.significand == x2.significand) {
		if ((int)t < 0)
			t = -(int)t;
		if ((fpis[t-FLOAT]->rounding & 3) == FPI_Round_down)
			return zerosf(SF_Neg);
		return zerosf(0); /* +0, x1==x2==-0 done elsewhere */
	}
	if (diff < 0 || (diff == 0 && x1.significand < x2.significand)) {
		rv = SF_NEG(x2);
		z = rshiftd_rndodd(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftd_rndodd(x2.significand, 0, diff );
	}
	if ((rv.kind & SF_kmask) == SF_Denormal)
		rv.kind -= SF_Denormal - SF_Normal;
	rv.significand -= z.hi;
	if ((int)t < 0) {
		int rd;

		t = -(int)t;
		rd = fpis[t-FLOAT]->rounding;
		if ((rd & 3) == FPI_Round_up || (rd & 3) == FPI_Round_down) {
			rv = round_extra(SF_NEG(rv), z.lo, t);
			return SF_NEG(rv);
		}
	}
	return round_extra(rv, z.lo, t);
}

SF
soft_plus(SF x1, SF x2, TWORD t)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	t = EVAL_TYPE(t);
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return soft_sub(x2, SF_NEG(x1), - (int)t);
	  case 0 - SF_Neg:
		return soft_sub(x1, SF_NEG(x2), t);
	}
	return soft_add(x1, x2, t);
}

SF
soft_minus(SF x1, SF x2, TWORD t)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	t = EVAL_TYPE(t);
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return soft_add(x1, SF_NEG(x2), t);
	else if (soft_isz(x1) && soft_isz(x2))
		return x1; /* special case -0 - -0, should return -0 */
	else if (x1.kind & SF_Neg)
		return soft_sub(SF_NEG(x2), SF_NEG(x1), - (int)t);
	else
		return soft_sub(x1, x2, t);
}

/*
 * Multiply two softfloats.
 */
SF
soft_mul(SF x1, SF x2, TWORD t)
{
	ULong x1hi, x2hi;
	ULLong mid1, mid, extra;

	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	if ((soft_isinf(x1) && soft_isz(x2))
	 || (soft_isz(x1) && soft_isinf(x2))) {
/* XXX "invalid"; warns? */
		return nansf();
	}
	if (soft_isinf(x1) || soft_isz(x1)) {
		x1.kind ^= x2.kind & SF_Neg;
		return x1;
	}
	if (soft_isinf(x2) || soft_isz(x2)) {
		x2.kind ^= x1.kind & SF_Neg;
		return x2;
	}
	t = EVAL_TYPE(t);
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, x1.exponent--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, x2.exponent--;
	if ((x1.kind & SF_kmask) == SF_Denormal)
		x1.kind -= SF_Denormal - SF_Normal;
	x1.kind ^= x2.kind & SF_Neg;
/* XXX copy SFEXCP_ flags? */
	x1.exponent += x2.exponent + WORKBITS;
	x1hi = x1.significand >> 32;
	x1.significand &= ONES(32);
	x2hi = x2.significand >> 32;
	x2.significand &= ONES(32);
	extra = x1.significand * x2.significand;
	mid1 = x1hi * x2.significand;
	mid = mid1 + x1.significand * x2hi;
	x1.significand = (ULLong) x1hi * x2hi;
	x1.significand += ((ULLong)(mid < mid1) << 32) | (mid >> 32);
	mid <<= 32;
	extra += mid;
#ifdef LONGLONG_WIDER_THAN_WORKBITS
	if (extra < mid || (extra>>WORKBITS)) {
		x1.significand++;
		extra &= ONES(WORKBITS);
	}
#else
	x1.significand += (extra < mid);
#endif
	if (x1.significand < NORMALMANT) {
		x1.exponent--;
		x1.significand <<= 1;
		if (extra >= NORMALMANT) {
			x1.significand++;
			extra -= NORMALMANT;
		}
		extra <<= 1;
	}
	return round_extra(x1, extra, t);
}

SF
soft_div(SF x1, SF x2, TWORD t)
{
	ULLong q, r, oppx2;
	int exp;

	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	if ((soft_isinf(x1) && soft_isinf(x2))
	 || (soft_isz(x1) && soft_isz(x2))) {
/* XXX "invalid"; warns? */
		return nansf();
	}
	if (soft_isinf(x1) || soft_isz(x1)) {
		x1.kind ^= x2.kind & SF_Neg;
		return x1;
	}
	else if (soft_isinf(x2))
		return zerosf((x1.kind & SF_Neg) ^ (x1.kind & SF_Neg));
	else if (soft_isz(x2)) {
/* XXX warns? */
		return infsf((x1.kind & SF_Neg) ^ (x1.kind & SF_Neg));
	}
	t = EVAL_TYPE(t);
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, x1.exponent--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, x2.exponent--;
	if ((x1.kind & SF_kmask) == SF_Denormal)
		x1.kind -= SF_Denormal - SF_Normal;
	x1.kind ^= x2.kind & SF_Neg;
	exp = x1.exponent - x2.exponent - WORKBITS;
	if (exp < -30000) {
		/* huge underflow, flush to 0 to avoid issues */
		x1 = zerosf(x1.kind & SF_Neg);
		x1.kind |= SFEXCP_Inexlo | SFEXCP_Underflow;
		return x1;
	}
/* XXX copy SFEXCP_ flags? */
	q = 0;
	if (x1.significand >= x2.significand) {
		++exp;
		++q;
		x1.significand -= x2.significand;
	}
	r = x1.significand;
	oppx2 = (ONES(WORKBITS-1) - x2.significand) + 1;
	do {
		q <<= 1;
		if (r & NORMALMANT) {
			r &= ~NORMALMANT;
			r <<= 1;
			r += oppx2;
			++q;
		}
		else {
			r <<= 1;
			if (r >= x2.significand) {
				r -= x2.significand;		
				++q;
			}
		}
	} while((q & NORMALMANT) == 0);
	x1.significand = q;
	x1.exponent = exp;
	if (r) {
		/* be sure to set correctly highest bit of extra */
		r += oppx2 / 2;
		r |= 1; /* rounds to odd */ /* XXX is there special case if power-of-2? */
	}
	return round_extra(x1, r, t);
}

/*
 * Comparisons.
 */

/*
 * Return true if fp number is zero. Easy.
 */
int
soft_isz(SF sf)
{
	return (sf.kind & SF_kmask) == SF_Zero;
}

int
soft_isinf(SF sf)
{
	return (sf.kind & SF_kmask) == SF_Infinite;
}

int
soft_isnan(SF sf)
{
	return (sf.kind & SF_kmask) == SF_NaN;
}

/*
 * Return true if either operand is NaN.
 */
int
soft_cmp_unord(SF x1, SF x2)
{
	return soft_isnan(x1) || soft_isnan(x2);
}

/*
 * IEEE754 states that between any two floating-point numbers,
 * four mutually exclusive relations are possible:
 * less than, equal, greater than, or unordered.
 */

int
soft_cmp_ne(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_eq(x1, x2);
}

/* XXX for _le and _ge, having NaN operand is "invalid"; warns? */
int
soft_cmp_le(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_gt(x1, x2);
}

int
soft_cmp_ge(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_lt(x1, x2);
}

int
soft_cmp_eq(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
		return 0; /* IEEE says "quiet" */
	if (soft_isz(x1))
		/* special case: +0 == -0 (discard sign) */
		return soft_isz(x2);
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return 0;
	if (soft_isinf(x1))
		return soft_isinf(x2);
	/* Both operands are finite, nonzero, same sign. */
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 == exp2 && x1.significand == x2.significand;
}

int
soft_cmp_lt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? */
		return 0;
	if (soft_isz(x1) && soft_isz(x2))
		return 0; /* special case: -0 !> +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 < 0 < x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 > 0 > x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (soft_isinf(x2))
		return ! soft_isinf(x1);
	if (soft_isz(x1))
		return 1;
	if (soft_isz(x2) || soft_isinf(x1))
		return 0;
	/* Both operands are finite, nonzero, same sign. Test |x1| < |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 < exp2 || x1.significand < x2.significand;
}

int
soft_cmp_gt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? */
		return 0;
	if (soft_isz(x1) && soft_isz(x2))
		return 0; /* special case: -0 !< +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 > 0 > x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 < 0 < x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (soft_isinf(x1))
		return ! soft_isinf(x2);
	if (soft_isz(x1) || soft_isinf(x2))
		return 0;
	if (soft_isz(x2))
		return 1;
	/* Both operands are finite, nonzero, same sign. Test |x1| > |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 > exp2 || x1.significand > x2.significand;
}

/*
 * Prepare a SF value for use in packed (interchange) format.
 * Correctly rounds for the target type representation.
 * Returns the biased exponent, ready to be shifted.
 *
 * The .significand bits are left in place, ready to be used by
 * the endian-aware code. The MSB one is still there.
 * Sign is indicated with psf->kind & SF_neg, as usual.
 *
 * SF_NaN is expanded into the IEEE754:2008 default representation
 * (11 as most significant bits, rest all zeroes); if not appropriate
 * for the target, the calling code should replace it with SF_NaNbits
 * with the adequate bits into the .significand member.
 */
/* XXX TODO: implement the NaNbits stuff in round_extra() above... */
int
soft_pack(SF *psf, TWORD t)
{
	FPI *fpi;
	int biasedexp;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	*psf = SF_ROUND(*psf, t);
	biasedexp = psf->exponent + fpi->exp_bias;
	switch(psf->kind & SF_kmask) {
	  case SF_Zero:
		psf->significand = 0;
		biasedexp = 0;
		if (! fpi->has_neg_zero)
			psf->kind &= ~SF_Neg;
		break;

	  case SF_Normal:
		if (fpi->has_radix_16) {
			assert((psf->significand >> (fpi->nbits-4)) >= 1);
			assert((psf->significand >> (fpi->nbits-4)) <= 15);
		}
		else
			assert((psf->significand >> (fpi->nbits-1)) == 1);
		assert(psf->exponent >= fpi->emin);
		assert(psf->exponent <= fpi->emax);
		break;

	  case SF_Denormal:
		assert(! fpi->sudden_underflow);
		assert(! fpi->has_radix_16);
		assert((psf->significand >> (fpi->nbits-1)) == 0);
		assert(psf->exponent == fpi->emin);
		biasedexp = 0;
		break;

	  case SF_Infinite:
/* XXX Should rather deliver corresponding dbl_MAX (HUGE_VAL), with error message */
		assert(fpi->has_inf_nan);
		psf->significand = ONEZEROES(fpi->nbits-1);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* Let decay as NaN */
	  case SF_NaN:
/* XXX can be the result of float f = 0./0.; !!! What to do on a VAX?!? */
		psf->significand = 3 * ONEZEROES(fpi->nbits-2);
		/* FALLTHROUGH */
	  case SF_NaNbits:
		assert(fpi->has_inf_nan);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;
	}
	if (fpi->has_radix_16) {
		assert((biasedexp & 3) == 0);
		biasedexp >>= 2;
	}
	return biasedexp;

}

/*
 * Conversions from decimal and hexadecimal strings.
 */
NODE *
floatcon(char *str)
{
	TWORD tw;
	NODE *p;
	char *sfx, *eptr;
	FPI *fpi;
	ULong bits[2] = { 0, 0 };
	Long expt;
	int k, im;

	im = 0;
	tw = DOUBLE;
	sfx = str + strlen(str) - 1;
	if (*sfx == 'i') {
#ifndef NO_COMPLEX
/* XXX warns? */
#endif
	    im = 1;
		--sfx;
	}
	switch (*sfx) {
	case 'f':
	case 'F':
		tw = FLOAT;
		break;
	case 'l':
	case 'L':
#ifdef notyet
		/* Needed to properly support e.g. _Generic() */
		tw = LDOUBLE;
#else
		/* Some MD backends are assuming LDOUBLE cannot happen... */
		tw = ctype(LDOUBLE);
#endif
		break;
	default:
		++sfx;
		break;
	}
	if (sfx[-1] == 'i') {
/* XXX warns if two 'i'? (accepted through scan.l right now */
#ifndef NO_COMPLEX
/* XXX warns? */
#endif
		++im;
		--sfx;
	}
	fpi = fpis[EVAL_TYPE(tw)-FLOAT];
	k = strtodg(str, &eptr, fpi, &expt, bits);
	if (eptr != sfx) /* XXX can happen with "strange" FP constants, like 1.0ifi or 2.LL */
		uerror("Botch in floatcon, sfx-eptr=%d", (int)(sfx-eptr));
	if (k & SFEXCP_Overflow)
		werror("Overflow in floating-point constant");
/* XXX F.7.2 recommends (indirectly) diagnostic for underflow; might be verbose though */
	if (k & SFEXCP_Inexact && (str[1] == 'x' || str[1] == 'X'))
		werror("Hexadecimal floating-point constant not represented exactly");
#ifndef NO_COMPLEX
	if (im)
		tw += (FIMAG-FLOAT);
#endif
	p = block(FCON, NIL, NIL, tw, 0, 0);
	p->n_dcon.kind = k;
	p->n_dcon.significand = bits[0];
	if (fpi->nbits > 32)
		p->n_dcon.significand |= ((U_CONSZ)bits[1] << 32);
	p->n_dcon.exponent = expt;
	return p;
}
#endif
#endif

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
#define assert(e) ((e)?cerror("assertion failed " #e " at softfloat:%d",__LINE__):(void)0)
#endif
/*
 * Floating point emulation, to not depend on the characteristics and bugs
 * of the host implementation when compiling.
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
 * Use parametric floating-point representation, as used in
 *
 * 	David M. Gay, "Correctly Rounded Binary-Decimal and
 *	Decimal-Binary Conversions", Numerical Analysis Manuscript
 *	No. 90-10, Bell Labs, Murray Hill, 1990;
 *	http://cm.bell-labs.com/cm/cs/what/ampl/REFS/rounding.ps.gz
 *
 * Arithmetic characteristics are described in struct FPI.
 * The actual numbers are represented (stored) in struct SF.
 * Floating-point numbers have fpi->nbits bits.
 * In this implementation, the bits are stored in one large integer
 * (of type CONSZ); this limits the number of bits to 64.
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
 * In the fpi_binary64, we actually write -53 + 1 rather than -52, to
 * emphasize that there are 53 bits including one implicit bit which is
 * at the left of the binary point.
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
 * XXX - assumes that:
 *	- (obviously) 2's complement, char are 8 bits
 *	- long long is (at least) 64 bits
 *	- CONSZ is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */
/*
 * The algorithms for the operations were derived from John Hauser's 
 * SoftFloat package (which is also used in libpcc/libsoftfloat.)
 */

/* XXX explanations about FLT_EVAL_METHOD */

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

/* XXX some macros:
#define ONEZEROES(n) (1ull << (n))
#define ONES(n) (ONEZEROES(n) | (ONEZEROES(n)-1))
*/

int strtodg (const char*, char**, FPI*, Long*, ULong*);
int strhextodg (const char*, char**, FPI*, Long*, ULong*);

/* IEEE binary formats, and their interchange format encodings */
FPI fpi_binary16 = { 11, 1-15-11+1,
                        30-15-11+1, 1, 0,
           0, 1, 1,  16,   15+11-1 };
FPI fpi_binary32 = { 24,  1-127-24+1,
                        254-127-24+1, 1, 0,
           0, 1, 1,  32,    127+24-1 };
FPI fpi_binary64 = { 53,   1-1023-53+1,
                        2046-1023-53+1, 1, 0,
           0, 1, 1,  64,     1023+53-1 };
#ifndef notyet
FPI fpi_binary128 = { 113,   1-16383-113+1,
                         32766-16383-113+1, 1, 0,
           0, 1, 1,   128,     16383+113-1 };
#endif
/* IEEE double extended in its usual form, for example Intel 387 */
FPI fpi_binaryx80 = { 64,   1-16383-64+1,
                        32766-16383-64+1, 1, 0,
           1, 1, 1,   80,     16383+64-1 };

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
 * Returns a zero softfloat.
 */
static SF
nullsf(void)
{
	SF rv;

	assert(SF_Zero == 0);
	rv.significand = rv.exponent = rv.kind = 0;
	return rv;
}

/*
 * Conversions.
 */

/*
 * Convert from integer to floating-point.
 * XXX - should rounds correctly to the target type
 * (assuming FLT_EVAL_METHOD does not mandate otherwise.)
 */
SF
soft_from_int(CONSZ ll, TWORD t)
{
	FPI *fpi;
	SF rv;
	unsigned long long normal1;

	rv = nullsf();
	if (ll == 0)
		return rv;  /* fp is zero */
	rv.kind = SF_Normal;
	if (ll < 0)
		rv.kind |= SF_Neg, ll = -ll;
	rv.significand = ll;
	assert(t>=FLOAT && t<=LDOUBLE);
/* XXX FLT_EVAL_CONSTANT might force t up */
	fpi = fpis[t-FLOAT];
	normal1 = 1ull << (fpi->nbits - 1);
	while (rv.significand < normal1)
		rv.significand <<= 1, --rv.exponent;
	if ((rv.significand & (normal1-1)) != normal1) {
		/* More bits than allowed in significand */

	/* XXX Should round correctly... Work to do. */
	/* XXX Also should check if not overflow (think binary16) */
		cerror("soft_from_int: precision loss");
	}
	return rv;
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
SF
soft_cast(SF sf, TWORD t)
{
	FPI *fpi;
	int exp = sf.exponent;

	switch(sf.kind & SF_kmask) {
	  default:
		return sf;
	/* XXX in fact SF_Nanbits might be affected as well; but representation is not yet fixed!
	 *   So should choose the representation in order to adapt to this code!
	 */
	  case SF_Denormal:
		assert(SF_Denormal > SF_Normal);
		sf.kind -= SF_Denormal - SF_Normal;
		break;
	  case SF_Normal:
		break;
	}
	while (sf.significand < (1ull<<63))
		sf.significand <<= 1, exp--;
	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	if (fpi->nbits < 64) {
		int excess = 64-fpi->nbits;
		if (sf.significand & ((1ull << excess)-1)) {
			/* precision loss */

			cerror("soft_cast: precision loss");
		}
		sf.significand >>= excess;
		exp += excess;
	}
	if (exp > fpi->emax) {
		cerror("soft_cast: overflow");
	}
	else if (exp < fpi->emin) {
		cerror("soft_cast: underflow / denormal");
	}
	sf.exponent = exp;
	return sf;
}

/*
 * Convert a fp number to a CONSZ. Always chop toward zero.
 * XXX Should warns correctly if out-of-range.
 */
CONSZ
soft_to_int(SF sf, TWORD t)
{
	unsigned long long mant;
	int exp = sf.exponent;

	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
	  case SF_Denormal:
		return 0;

	  case SF_Normal:
		if (exp < - (int)sizeof(mant) * 8 - 1)
			return 0;
		if (exp < sizeof(mant)*8)
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
		mant = -mant;
	return mant;
}

/*
 * Operations.
 */

/*
 * Negate a softfloat. Easy.
 * XXX Do not work correctly for SF_Zero when negative zero are not supported.
 */
SF
soft_neg(SF sf)
{
	sf.kind ^= SF_Neg;
	return sf;
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
 * multiply two dfloat.
 */
SF
soft_mul(SF x1, SF x2)
{
	cerror("soft_mul");
	return x1;
}

SF
soft_div(SF x1, SF x2)
{
	cerror("soft_div");
	return x1;
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
soft_cmp_eq(SF x1, SF x2)
{
	int exp1 = x1.exponent, exp2 = x2.exponent;

/* Special cases: +0 == -0 ; NaN != NaN ; normalized */
	if ((x1.kind & SF_kmask) != (x2.kind & SF_kmask)) {
		/* XXX only case where it can match is when Norm double == denorm float */
		return 0;
	}
	while (x1.significand < (1ull<<63))
		x1.significand <<= 1, exp1--;
	while (x2.significand < (1ull<<63))
		x2.significand <<= 1, exp2--;

	cerror("soft_cmp_eq");
	return 0;
}

int
soft_cmp_ne(SF x1, SF x2)
{
	/* XXX strategy: test NaN, else returns ! soft_cmp_eq(SF x1, SF x2) */
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
 * Prepare a SF value for use in packed (interchange) format.
#ifdef notyet
 * Correctly rounds (for the target type) representation.
#else
 * Expect a correctly rounded (for the target type) representation.
#endif
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
soft_pack(SF *psf, FPI *fpi)
{
	int biasedexp;

	/* XXX normalize */
	biasedexp = psf->exponent + fpi->exp_bias;
	switch(psf->kind & SF_kmask) {
	  case SF_Zero:
		psf->significand = 0;
		biasedexp = 0;
		if (! fpi->has_neg_zero)
			psf->kind &= ~SF_Neg;
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
	  default:
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

/*
 * Conversions from decimal and hexadecimal strings.
 */

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
fcon(char *str, int (*strtodg_p)())
{
	TWORD tw;
	NODE *p;
	char *sfx, *eptr;
	FPI *fpi;
	ULong bits[2];
	Long expt;
	int k, im;

#if 0
	sfx = suffix(str, &im, &tw);
#else
	im = 0;
	tw = DOUBLE;
	sfx = str + strlen(str) - 1;
	if (*sfx == 'i') {
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
		++im;
		--sfx;
	}
#endif
/* XXX FLT_EVAL_CONSTANT might force tw up */
	fpi = fpis[tw-FLOAT];
	k = (*strtodg_p)(str, &eptr, fpi, &expt, bits);
	if (eptr != sfx) /* XXX I believe this can happen with "strange" FP constants, accepted through scan.l */
		uerror("Botch in floatcon, sfx-eptr=%d", (int)(sfx-eptr));
	if (k & SFEXCP_Overflow)
		werror("Overflow in floating-point constant");
/* XXX F.7.2 recommends diagnostic for underflow, but the definition of it in IEEE754 is a bit hairy */
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

/* XXX TODO: merge the two functions in scan.l (when only using softfloat) */
NODE *
floatcon(char *str)
{
	return fcon(str, strtodg);
}

NODE *
fhexcon(char *str)
{
	return fcon(str, strhextodg);
}
#endif
#endif

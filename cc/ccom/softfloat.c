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
 * http://www.netlib.org/fp/ ; see also strtodg.c where 
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
 */

/* XXX explanations about FLT_EVAL_METHOD */

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
 * The algorithms for the operations were derived from John Hauser's 
 * SoftFloat package (which is also used in libpcc/libsoftfloat.) 
 */

struct sig_extra {
	ULLong sig, extra;
};

static struct sig_extra
rshift_extra(ULLong a, ULLong extra, int count)
{
	struct sig_extra z;
	z.sig = a >> count;
	z.extra = (a << (WORKBITS-count)) | (extra != 0);
	return z;
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
	ULLong normal1;

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
	normal1 = ONEZEROES(fpi->nbits - 1);
	assert(rv.significand);
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
typedef int bool;

static SF
round_extra(SF sf, ULLong extra, TWORD t)
{
	FPI *fpi;
	ULLong normal1;
	int exp = sf.exponent, doinc;

    int roundingMode;
    bool roundNearEven;
    ULLong roundIncrement, roundMask, roundBits;
    bool isTiny, doIncrement;
    struct sig_extra sig64Extra;


	switch(sf.kind & SF_kmask) {
#ifndef notneeded
	  case SF_Zero:
		if (! fpi->has_neg_zero)
			sf.kind &= ~SF_Neg;
		/* FALLTHROUGH */
#endif
	  default:
		return sf;
	  case SF_NaNbits:
		cerror("Unexpected softfloat NaNbits");
		sf.kind -= SF_NaNbits - SF_NaN;
		return sf;
	  case SF_Denormal:
		assert(SF_Denormal > SF_Normal);
		sf.kind -= SF_Denormal - SF_Normal;
		break;
	  case SF_Normal:
		break;
	}
	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];

/*
    roundingMode = softfloat_roundingMode;
    roundNearEven = (roundingMode == softfloat_round_near_even);

    if ( roundingPrecision == 80 ) goto precision80;
    if ( roundingPrecision == 64 ) {
        roundIncrement = UINT64_C( 0x0000000000000400 );
        roundMask = UINT64_C( 0x00000000000007FF );
    } else if ( roundingPrecision == 32 ) {
        roundIncrement = UINT64_C( 0x0000008000000000 );
        roundMask = UINT64_C( 0x000000FFFFFFFFFF );
    } else {
        goto precision80;
    }
    sig |= (sigExtra != 0);
    if ( ! roundNearEven && (roundingMode != softfloat_round_near_maxMag) ) {
        roundIncrement =
            (roundingMode
                 == (sign ? softfloat_round_min : softfloat_round_max))
                ? roundMask
                : 0;
    }
    roundBits = sig & roundMask;
    if ( 0x7FFD <= (uint32_t) (exp - 1) ) {
        if ( exp <= 0 ) {
            isTiny =
                   (softfloat_detectTininess
                        == softfloat_tininess_beforeRounding)
                || (exp < 0)
                || (sig <= (uint64_t) (sig + roundIncrement));
            sig = softfloat_shiftRightJam64( sig, 1 - exp );
            roundBits = sig & roundMask;
            if ( isTiny && roundBits ) {
                softfloat_raiseFlags( softfloat_flag_underflow );
            }
            if ( roundBits ) {
                softfloat_exceptionFlags |= softfloat_flag_inexact;
            }
            sig += roundIncrement;
            exp = ((sig & UINT64_C( 0x8000000000000000 )) != 0);
            roundIncrement = roundMask + 1;
            if ( roundNearEven && (roundBits<<1 == roundIncrement) ) {
                roundMask |= roundIncrement;
            }
            sig &= ~roundMask;
            goto packReturn;
        }
        if (
               (0x7FFE < exp)
            || ((exp == 0x7FFE) && ((uint64_t) (sig + roundIncrement) < sig))
        ) {
            goto overflow;
        }
    }
    if ( roundBits ) softfloat_exceptionFlags |= softfloat_flag_inexact;
    sig = (uint64_t) (sig + roundIncrement);
    if ( sig < roundIncrement ) {
        ++exp;
        sig = UINT64_C( 0x8000000000000000 );
    }
    roundIncrement = roundMask + 1;
    if ( roundNearEven && (roundBits<<1 == roundIncrement) ) {
        roundMask |= roundIncrement;
    }
    sig &= ~roundMask;
    if ( ! sig ) exp = 0;
    goto packReturn;
 precision80:


    doIncrement = (UINT64_C( 0x8000000000000000 ) <= sigExtra);
    if ( ! roundNearEven && (roundingMode != softfloat_round_near_maxMag) ) {
        doIncrement =
            (roundingMode
                 == (sign ? softfloat_round_min : softfloat_round_max))
                && sigExtra;
    }
 */

	normal1 = ONEZEROES(fpi->nbits - 1);

	assert(sf.significand);
	while (sf.significand < NORMALMANT)
/* XXX does not work if extra!=0 */
		sf.significand <<= 1, exp--;
	if (fpi->nbits < WORKBITS) {
		int excess = WORKBITS-fpi->nbits;
		if (sf.significand & ONES(excess)) {
			/* precision loss */

			cerror("soft_cast: precision loss");
		}
		/* rshift_extra(); */
		sf.significand >>= excess;
		exp += excess;
	}

	if (exp < fpi->emin) {
		cerror("soft_cast: underflow / denormal");

/*
            isTiny =
                   (softfloat_detectTininess
                        == softfloat_tininess_beforeRounding)
                || (exp < 0)
                || ! doIncrement
                || (sig < UINT64_C( 0xFFFFFFFFFFFFFFFF ));
            sig64Extra =
                softfloat_shiftRightJam64Extra( sig, sigExtra, 1 - exp );
            sig = sig64Extra.v;
            sigExtra = sig64Extra.extra;
            if ( isTiny && sigExtra ) {
                softfloat_raiseFlags( softfloat_flag_underflow );
            }
            if ( sigExtra ) softfloat_exceptionFlags |= softfloat_flag_inexact;
            doIncrement = (UINT64_C( 0x8000000000000000 ) <= sigExtra);
            if (
                   ! roundNearEven
                && (roundingMode != softfloat_round_near_maxMag)
            ) {
                doIncrement =
                    (roundingMode
                         == (sign ? softfloat_round_min : softfloat_round_max))
                        && sigExtra;
            }
            exp = 0;
            if ( doIncrement ) {
                ++sig;
                sig &=
                    ~(! (sigExtra & UINT64_C( 0x7FFFFFFFFFFFFFFF ))
                          & roundNearEven);
                exp = ((sig & UINT64_C( 0x8000000000000000 )) != 0);
            }
*/

	}
/* XXX also check if emax-1 && maxmantissa && doincrement */
	else if (exp > fpi->emax) {
		roundMask = 0;
overflow:
		if (fpi->rounding == FPI_Round_zero || fpi->rounding ==
		    (sf.kind & SF_Neg ? FPI_Round_up : FPI_Round_down)) {
cerror("soft_cast: overflow but return ?dbl_MAX (should not happen)");
			sf.kind |= SFEXCP_Overflow | SFEXCP_Inexhi;
			exp = fpi->emax;
			sf.significand = ~roundMask;
			return sf;
		}
		else {
			sf.kind = SF_Infinite | (sf.kind & SF_Neg)
			    | SFEXCP_Overflow | SFEXCP_Inexhi;
			sf.significand = ONEZEROES(fpi->nbits-1);
			exp = fpi->emax+1;
		}
	}
	else {

/*
    if ( sigExtra ) softfloat_exceptionFlags |= softfloat_flag_inexact;
    if ( doIncrement ) {
        ++sig;
        if ( ! sig ) {
            ++exp;
            sig = UINT64_C( 0x8000000000000000 );
        } else {
            sig &=
                ~(! (sigExtra & UINT64_C( 0x7FFFFFFFFFFFFFFF ))
                      & roundNearEven);
        }
    } else {
        if ( ! sig ) exp = 0;
    }
*/

	}
	sf.exponent = exp;
	return sf;
}


SF
soft_cast(SF sf, TWORD t)
{
	return round_extra(sf, 0, t);
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

/*
 * Add two numbers of same sign.
 */
static SF
soft_add(SF x1, SF x2, TWORD t)
{
	SF rv;
	struct sig_extra z;
	int diff;

	if (soft_isinf(x1))
		return x1;
	if (soft_isinf(x2) || soft_isz(x1))
		return x2;
	if (soft_isz(x2))
		return x1;
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
		if ((x2.kind & SF_kmask) == SF_Denormal)
			x2.kind -= SF_Denormal - SF_Normal;
		rv = x2;
		z = rshift_extra(x1.significand, 0, -diff );
	}
	else {
		if ((x1.kind & SF_kmask) == SF_Denormal)
			x1.kind -= SF_Denormal - SF_Normal;
		rv = x1;
		z = rshift_extra(x2.significand, 0, diff );
	}
	rv.significand += z.sig;
	if (rv.significand < NORMALMANT) {
		/* target mantissa overflows */
		z = rshift_extra(rv.significand, z.extra, 1);
		rv.significand = z.sig | NORMALMANT;
		++rv.exponent;
	}
	return round_extra(rv, z.extra, t);
}

/*
 * Substract two positive numbers.
 */
static SF
soft_sub(SF x1, SF x2, TWORD t)
{
	cerror("soft_sub");
	return x1;

/*
    expDiff = expA - expB;
    if ( 0 < expDiff ) goto expABigger;
    if ( expDiff < 0 ) goto expBBigger;
    if ( expA == 0x7FFF ) {
        if ( (sigA | sigB) & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            goto propagateNaN;
        }
        softfloat_raiseFlags( softfloat_flag_invalid );
        uiZ64 = defaultNaNExtF80UI64;
        uiZ0  = defaultNaNExtF80UI0;
        goto uiZ;
    }
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
    expZ = expA;
    if ( ! expZ ) expZ = 1;
    sigExtra = 0;
    if ( sigB < sigA ) goto aBigger;
    if ( sigA < sigB ) goto bBigger;
    uiZ64 =
        packToExtF80UI64( (softfloat_roundingMode == softfloat_round_min), 0 );
    uiZ0 = 0;
    goto uiZ;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 expBBigger:
    if ( expB == 0x7FFF ) {
        if ( sigB & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
        uiZ64 = packToExtF80UI64( signZ ^ 1, 0x7FFF );
        uiZ0  = UINT64_C( 0x8000000000000000 );
        goto uiZ;
    }
    if ( ! expA ) {
        ++expDiff;
        sigExtra = 0;
        if ( ! expDiff ) goto newlyAlignedBBigger;
    }
    sig128 = softfloat_shiftRightJam128( sigA, 0, -expDiff );
    sigA = sig128.v64;
    sigExtra = sig128.v0;
 newlyAlignedBBigger:
    expZ = expB;
 bBigger:
    signZ ^= 1;
    sig128 = softfloat_sub128( sigB, 0, sigA, sigExtra );
    goto normRoundPack;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 expABigger:
    if ( expA == 0x7FFF ) {
        if ( sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
        uiZ64 = uiA64;
        uiZ0  = uiA0;
        goto uiZ;
    }
    if ( ! expB ) {
        --expDiff;
        sigExtra = 0;
        if ( ! expDiff ) goto newlyAlignedABigger;
    }
    sig128 = softfloat_shiftRightJam128( sigB, 0, expDiff );
    sigB = sig128.v64;
    sigExtra = sig128.v0;
 newlyAlignedABigger:
    expZ = expA;
 aBigger:
    sig128 = softfloat_sub128( sigA, 0, sigB, sigExtra );
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 normRoundPack:
    return
        softfloat_normRoundPackToExtF80(
            signZ, expZ, sig128.v64, sig128.v0, extF80_roundingPrecision );
*/

}

SF
soft_plus(SF x1, SF x2)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	switch (x1.kind & SF_Neg - x2.kind & SF_Neg) {
	  case SF_Neg - 0:
		return soft_sub(x2, soft_neg(x1), /*XXX*/LDOUBLE);
	  case 0 - SF_Neg:
		return soft_sub(x1, soft_neg(x2), /*XXX*/LDOUBLE);
	}
	return soft_add(x1, x2, /*XXX*/LDOUBLE);
}

SF
soft_minus(SF x1, SF x2)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return soft_add(x1, soft_neg(x2), /*XXX*/LDOUBLE);
	else if (x1.kind & SF_Neg)
		return soft_sub(soft_neg(x2), soft_neg(x1), /*XXX*/LDOUBLE);
	else
		return soft_sub(x1, x2, /*XXX*/LDOUBLE);
}

/*
 * multiply two dfloat.
 */
SF
soft_mul(SF x1, SF x2)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	cerror("soft_mul");
	return x1;

/*
    if ( ! expA ) expA = 1;
    sigA = aSPtr->signif;
    if ( ! (sigA & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sigA ) goto zero;
        expA += softfloat_normExtF80SigM( &sigA );
    }
    if ( ! expB ) expB = 1;
    sigB = bSPtr->signif;
    if ( ! (sigB & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sigB ) goto zero;
        expB += softfloat_normExtF80SigM( &sigB );
    }

// version f128
    if ( ! expA ) {
        if ( ! (sigA.v64 | sigA.v0) ) goto zero;
        normExpSig = softfloat_normSubnormalF128Sig( sigA.v64, sigA.v0 );
        expA = normExpSig.exp;
        sigA = normExpSig.sig;
    }
    if ( ! expB ) {
        if ( ! (sigB.v64 | sigB.v0) ) goto zero;
        normExpSig = softfloat_normSubnormalF128Sig( sigB.v64, sigB.v0 );
        expB = normExpSig.exp;
        sigB = normExpSig.sig;
    }

    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
    expZ = expA + expB - 0x3FFE;
//    softfloat_mul64To128M( sigA, sigB, sigProd );
void softfloat_mul64To128M( uint64_t a, uint64_t b, uint32_t *zPtr )
{
    uint32_t a32, a0, b32, b0;
    uint64_t z0, mid1, z64, mid;

    a32 = a>>32;
    a0 = a;
    b32 = b>>32;
    b0 = b;
    z0 = (uint64_t) a0 * b0;
    mid1 = (uint64_t) a32 * b0;
    mid = mid1 + (uint64_t) a0 * b32;
    z64 = (uint64_t) a32 * b32;
    z64 += (uint64_t) (mid < mid1)<<32 | mid>>32;
    mid <<= 32;
    z0 += mid;
    zPtr[indexWord( 4, 1 )] = z0>>32;
    zPtr[indexWord( 4, 0 )] = z0;
    z64 += (z0 < mid);
    zPtr[indexWord( 4, 3 )] = z64>>32;
    zPtr[indexWord( 4, 2 )] = z64;

}

struct uint128 softfloat_mul64To128( uint64_t a, uint64_t b )
{
    uint32_t a32, a0, b32, b0;
    struct uint128 z;
    uint64_t mid1, mid;

    a32 = a>>32;
    a0 = a;
    b32 = b>>32;
    b0 = b;
    z.v0 = (uint_fast64_t) a0 * b0;
    mid1 = (uint_fast64_t) a32 * b0;
    mid = mid1 + (uint_fast64_t) a0 * b32;
    z.v64 = (uint_fast64_t) a32 * b32;
    z.v64 += (uint_fast64_t) (mid < mid1)<<32 | mid>>32;
    mid <<= 32;
    z.v0 += mid;
    z.v64 += (z.v0 < mid);
    return z;
}
//**
    if ( sigProd[indexWordLo( 4 )] ) sigProd[indexWord( 4, 1 )] |= 1;
    extSigZPtr = &sigProd[indexMultiwordHi( 4, 3 )];
    if ( sigProd[indexWordHi( 4 )] < 0x80000000 ) {
        --expZ;
        softfloat_add96M( extSigZPtr, extSigZPtr, extSigZPtr );
    }
    softfloat_roundPackMToExtF80M(
        signZ, expZ, extSigZPtr, extF80_roundingPrecision, zSPtr );
    return;

//**
#define softfloat_add96M( aPtr, bPtr, zPtr ) softfloat_addM( 3, aPtr, bPtr, zPtr )

void softfloat_addM(
     uint_fast8_t size_words,
     const uint32_t *aPtr,
     const uint32_t *bPtr,
     uint32_t *zPtr
 )
{
    unsigned int index, lastIndex;
    uint_fast8_t carry;
    uint32_t wordA, wordZ;

    index = indexWordLo( size_words );
    lastIndex = indexWordHi( size_words );
    carry = 0;
    for (;;) {
        wordA = aPtr[index];
        wordZ = wordA + bPtr[index] + carry;
        zPtr[index] = wordZ;
        if ( index == lastIndex ) break;
        carry = carry ? (wordZ <= wordA) : (wordZ < wordA);
        index += wordIncr;
    }

}
//**

// version f128
    expZ = expA + expB - 0x4000;
    sigA.v64 |= UINT64_C( 0x0001000000000000 );
    sigB = softfloat_shortShiftLeft128( sigB.v64, sigB.v0, 16 );
    softfloat_mul128To256M( sigA.v64, sigA.v0, sigB.v64, sigB.v0, sig256Z );
    sigZExtra = sig256Z[indexWord( 4, 1 )] | (sig256Z[indexWord( 4, 0 )] != 0);
    sigZ =
        softfloat_add128(
            sig256Z[indexWord( 4, 3 )], sig256Z[indexWord( 4, 2 )],
            sigA.v64, sigA.v0
        );
    if ( UINT64_C( 0x0002000000000000 ) <= sigZ.v64 ) {
        ++expZ;
        sig128Extra =
            softfloat_shortShiftRightJam128Extra(
                sigZ.v64, sigZ.v0, sigZExtra, 1 );
        sigZ = sig128Extra.v;
        sigZExtra = sig128Extra.extra;
    }
    return
        softfloat_roundPackToF128( signZ, expZ, sigZ.v64, sigZ.v0, sigZExtra );
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 propagateNaN:
    uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, uiB64, uiB0 );
    goto uiZ;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 infArg:
    if ( ! magBits ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        uiZ.v64 = defaultNaNF128UI64;
        uiZ.v0  = defaultNaNF128UI0;
        goto uiZ;
    }
    uiZ.v64 = packToF128UI64( signZ, 0x7FFF, 0 );
    goto uiZ0;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 zero:
    uiZ.v64 = packToF128UI64( signZ, 0, 0 );
 uiZ0:
    uiZ.v0 = 0;
 uiZ:
    uZ.ui = uiZ;
*/

}

SF
soft_div(SF x1, SF x2)
{
	if (soft_isnan(x1))
		return x1;
	else if (soft_isnan(x2))
		return x2;
	cerror("soft_div");
	return x1;

/*
    if ( ! expB ) expB = 1;
    if ( ! (sigB & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sigB ) {
            if ( ! sigA ) goto invalid;
            softfloat_raiseFlags( softfloat_flag_infinite );
            goto infinity;
        }
        normExpSig = softfloat_normSubnormalExtF80Sig( sigB );
        expB += normExpSig.exp;
        sigB = normExpSig.sig;
    }
    if ( ! expA ) expA = 1;
    if ( ! (sigA & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sigA ) goto zero;
        normExpSig = softfloat_normSubnormalExtF80Sig( sigA );
        expA += normExpSig.exp;
        sigA = normExpSig.sig;
    }
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
    expZ = expA - expB + 0x3FFF;
    if ( sigA < sigB ) {
        --expZ;
        rem = softfloat_shortShiftLeft128( 0, sigA, 32 );
    } else {
        rem = softfloat_shortShiftLeft128( 0, sigA, 31 );
    }
    recip32 = softfloat_approxRecip32_1( sigB>>32 );
    sigZ = 0;
    ix = 2;
    for (;;) {
        q64 = (uint_fast64_t) (uint32_t) (rem.v64>>2) * recip32;
        q = (q64 + 0x80000000)>>32;
        --ix;
        if ( ix < 0 ) break;
        rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
        term = softfloat_mul64ByShifted32To128( sigB, q );
        rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
        if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
            --q;
            rem = softfloat_add128( rem.v64, rem.v0, sigB>>32, sigB<<32 );
        }
        sigZ = (sigZ<<29) + q;
    }
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
    if ( ((q + 1) & 0x3FFFFF) < 2 ) {
        rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
        term = softfloat_mul64ByShifted32To128( sigB, q );
        rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
        term = softfloat_shortShiftLeft128( 0, sigB, 32 );
        if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
            --q;
            rem = softfloat_add128( rem.v64, rem.v0, term.v64, term.v0 );
        } else if ( softfloat_le128( term.v64, term.v0, rem.v64, rem.v0 ) ) {
            ++q;
            rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
        }
        if ( rem.v64 | rem.v0 ) q |= 1;
    }
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
    sigZ = (sigZ<<6) + (q>>23);
    sigZExtra = (uint64_t) ((uint_fast64_t) q<<41);
    return
        softfloat_roundPackToExtF80(
            signZ, expZ, sigZ, sigZExtra, extF80_roundingPrecision );
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 propagateNaN:
    uiZ = softfloat_propagateNaNExtF80UI( uiA64, uiA0, uiB64, uiB0 );
    uiZ64 = uiZ.v64;
    uiZ0  = uiZ.v0;
    goto uiZ;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    uiZ64 = defaultNaNExtF80UI64;
    uiZ0  = defaultNaNExtF80UI0;
    goto uiZ;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 infinity:
    uiZ64 = packToExtF80UI64( signZ, 0x7FFF );
    uiZ0  = UINT64_C( 0x8000000000000000 );
    goto uiZ;
    //*------------------------------------------------------------------------
    //*------------------------------------------------------------------------
 zero:
    uiZ64 = packToExtF80UI64( signZ, 0 );
    uiZ0  = 0;
 uiZ:
    uZ.s.signExp = uiZ64;
    uZ.s.signif  = uiZ0;
    return uZ.f;
*/

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
 * IEEE754 states that between any two floating-point numbers,
 * four mutually exclusive relations are possible:
 * less than, equal, greater than, and unordered.
 */

int
soft_cmp_ne(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_eq(x1, x2);
}

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

/*
 * Return true if either operand is NaN.
 */
int
soft_cmp_unord(SF x1, SF x2)
{
	return soft_isnan(x1) || soft_isnan(x2);
}

int
soft_cmp_eq(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
		return 0;
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
		return 0;
	if (soft_isz(x1) && soft_isz(x2))
		return 0; /* special case: -0 !> +0 */
	switch (x1.kind & SF_Neg - x2.kind & SF_Neg) {
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
		return 0;
	if (soft_isz(x1) && soft_isz(x2))
		return 0; /* special case: -0 !< +0 */
	switch (x1.kind & SF_Neg - x2.kind & SF_Neg) {
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
 * (11 as most significant bits, rest all zeroes); if not appropriate
 * for the target, the calling code should replace it with SF_NaNbits
 * with the adequate bits into the .significand member.
 */
int
soft_pack(SF *psf, TWORD t)
{
	FPI *fpi;
	int biasedexp;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
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
	ULong bits[2] = { 0, 0 };
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
		p->n_dcon.exponent = fpi->emax+1;
		break;

	  case SF_NaN:
		p->n_dcon.significand = 3ull << (fpi->nbits-2);
		/* FALLTHROUGH */
	  case SF_NaNbits:
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

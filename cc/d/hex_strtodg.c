/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998-2001 by Lucent Technologies
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

#ifdef USE_LOCALE
#include "locale.h"
#endif

#if 0
 static CONST int
fivesbits[] = {	 0,  3,  5,  7, 10, 12, 14, 17, 19, 21,
		24, 26, 28, 31, 33, 35, 38, 40, 42, 45,
		47, 49, 52
#ifdef VAX
		, 54, 56
#endif
		};
#endif

 Bigint *
#ifdef KR_headers
increment(b) Bigint *b;
#else
increment(Bigint *b)
#endif
{
	ULong *x, *xe;
	Bigint *b1;
#ifdef Pack_16
	ULong carry = 1, y;
#endif

	x = b->x;
	xe = x + b->wds;
#ifdef Pack_32
	do {
		if (*x < (ULong)0xffffffffL) {
			++*x;
			return b;
			}
		*x++ = 0;
		} while(x < xe);
#else
	do {
		y = *x + carry;
		carry = y >> 16;
		*x++ = y & 0xffff;
		if (!carry)
			return b;
		} while(x < xe);
	if (carry)
#endif
	{
		if (b->wds >= b->maxwds) {
			b1 = Balloc(b->k+1);
			Bcopy(b1,b);
			Bfree(b);
			b = b1;
			}
		b->x[b->wds++] = 1;
		}
	return b;
	}

 void
#ifdef KR_headers
decrement(b) Bigint *b;
#else
decrement(Bigint *b)
#endif
{
	ULong *x, *xe;
#ifdef Pack_16
	ULong borrow = 1, y;
#endif

	x = b->x;
	xe = x + b->wds;
#ifdef Pack_32
	do {
		if (*x) {
			--*x;
			break;
			}
		*x++ = 0xffffffffL;
		}
		while(x < xe);
#else
	do {
		y = *x - borrow;
		borrow = (y & 0x10000) >> 16;
		*x++ = y & 0xffff;
		} while(borrow && x < xe);
#endif
	}

 static int
#ifdef KR_headers
all_on(b, n) Bigint *b; int n;
#else
all_on(Bigint *b, int n)
#endif
{
	ULong *x, *xe;

	x = b->x;
	xe = x + (n >> kshift);
	while(x < xe)
		if ((*x++ & ALL_ON) != ALL_ON)
			return 0;
	if (n &= kmask)
		return ((*x | (ALL_ON << n)) & ALL_ON) == ALL_ON;
	return 1;
	}

 Bigint *
#ifdef KR_headers
set_ones(b, n) Bigint *b; int n;
#else
set_ones(Bigint *b, int n)
#endif
{
	int k;
	ULong *x, *xe;

	k = (n + ((1 << kshift) - 1)) >> kshift;
	if (b->k < k) {
		Bfree(b);
		b = Balloc(k);
		}
	k = n >> kshift;
	if (n &= kmask)
		k++;
	b->wds = k;
	x = b->x;
	xe = x + k;
	while(x < xe)
		*x++ = ALL_ON;
	if (n)
		x[-1] >>= ULbits - n;
	return b;
	}

 static int
rvOK
#ifdef KR_headers
 (d, fpi, exp, bits, exact, rd, irv)
 U *d; FPI *fpi; Long *exp; ULong *bits; int exact, rd, *irv;
#else
 (U *d, FPI *fpi, Long *exp, ULong *bits, int exact, int rd, int *irv)
#endif
{
	Bigint *b;
	ULong carry, inex, lostbits;
	int bdif, e, j, k, k1, nb, rv;

	carry = rv = 0;
	b = d2b(dval(d), &e, &bdif);
	bdif -= nb = fpi->nbits;
	e += bdif;
	if (bdif <= 0) {
		if (exact)
			goto trunc;
		goto ret;
		}
	if (P == nb) {
		if (
#ifndef IMPRECISE_INEXACT
			exact &&
#endif
			fpi->rounding ==
#ifdef RND_PRODQUOT
					FPI_Round_near
#else
					Flt_Rounds
#endif
			) goto trunc;
		goto ret;
		}
	switch(rd) {
	  case 1: /* round down (toward -Infinity) */
		goto trunc;
	  case 2: /* round up (toward +Infinity) */
		break;
	  default: /* round near */
		k = bdif - 1;
		if (k < 0)
			goto trunc;
		if (!k) {
			if (!exact)
				goto ret;
			if (b->x[0] & 2)
				break;
			goto trunc;
			}
		if (b->x[k>>kshift] & ((ULong)1 << (k & kmask)))
			break;
		goto trunc;
	  }
	/* "break" cases: round up 1 bit, then truncate; bdif > 0 */
	carry = 1;
 trunc:
	inex = lostbits = 0;
	if (bdif > 0) {
		if ( (lostbits = any_on(b, bdif)) !=0)
			inex = STRTOG_Inexlo;
		rshift(b, bdif);
		if (carry) {
			inex = STRTOG_Inexhi;
			b = increment(b);
			if ( (j = nb & kmask) !=0)
				j = ULbits - j;
			if (hi0bits(b->x[b->wds - 1]) != j) {
				if (!lostbits)
					lostbits = b->x[0] & 1;
				rshift(b, 1);
				e++;
				}
			}
		}
	else if (bdif < 0)
		b = lshift(b, -bdif);
	if (e < fpi->emin) {
		k = fpi->emin - e;
		e = fpi->emin;
		if (k > nb || fpi->sudden_underflow) {
			b->wds = inex = 0;
			*irv = STRTOG_Underflow | STRTOG_Inexlo;
			}
		else {
			k1 = k - 1;
			if (k1 > 0 && !lostbits)
				lostbits = any_on(b, k1);
			if (!lostbits && !exact)
				goto ret;
			lostbits |=
			  carry = b->x[k1>>kshift] & (1 << (k1 & kmask));
			rshift(b, k);
			*irv = STRTOG_Denormal;
			if (carry) {
				b = increment(b);
				inex = STRTOG_Inexhi | STRTOG_Underflow;
				}
			else if (lostbits)
				inex = STRTOG_Inexlo | STRTOG_Underflow;
			}
		}
	else if (e > fpi->emax) {
		e = fpi->emax + 1;
		*irv = STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
#ifndef NO_ERRNO
		errno = ERANGE;
#endif
		b->wds = inex = 0;
		}
	*exp = e;
	copybits(bits, nb, b);
	*irv |= inex;
	rv = 1;
 ret:
	Bfree(b);
	return rv;
	}

 static int
#ifdef KR_headers
mantbits(d) U *d;
#else
mantbits(U *d)
#endif
{
	ULong L;
#ifdef VAX
	L = word1(d) << 16 | word1(d) >> 16;
	if (L)
#else
	if ( (L = word1(d)) !=0)
#endif
		return P - lo0bits(&L);
#ifdef VAX
	L = word0(d) << 16 | word0(d) >> 16 | Exp_msk11;
#else
	L = word0(d) | Exp_msk1;
#endif
	return P - 32 - lo0bits(&L);
	}

 int
hextodg
#ifdef KR_headers
	(s00, se, fpi, exp, bits)
	CONST char *s00; char **se; FPI *fpi; Long *exp; ULong *bits;
#else
	(CONST char *s00, char **se, FPI *fpi, Long *exp, ULong *bits)
#endif
{
	int abe, abits, asub;
	int bb0, bb2, bb5, bbe, bd2, bd5, bbbits, bs2, c, decpt, denorm;
	int dsign, e, e1, e2, emin, esign, finished, i, inex, irv;
	int j, k, nbits, nd, nd0, nf, nz, nz0, rd, rvbits, rve, rve1, sign;
	int sudden_underflow;
	CONST char *s, *s0, *s1;
	double adj0, tol;
	Long L;
	U adj, rv;
	ULong *b, *be, y, z;
	Bigint *ab, *bb, *bb1, *bd, *bd0, *bs, *delta, *rvb, *rvb0;
#ifdef USE_LOCALE /*{{*/
#ifdef NO_LOCALE_CACHE
	char *decimalpoint = localeconv()->decimal_point;
	int dplen = strlen(decimalpoint);
#else
	char *decimalpoint;
	static char *decimalpoint_cache;
	static int dplen;
	if (!(s0 = decimalpoint_cache)) {
		s0 = localeconv()->decimal_point;
		if ((decimalpoint_cache = (char*)MALLOC(strlen(s0) + 1))) {
			strcpy(decimalpoint_cache, s0);
			s0 = decimalpoint_cache;
			}
		dplen = strlen(s0);
		}
	decimalpoint = (char*)s0;
#endif /*NO_LOCALE_CACHE*/
#else  /*USE_LOCALE}{*/
#define dplen 1
#endif /*USE_LOCALE}}*/

	irv = STRTOG_Zero;
	denorm = sign = nz0 = nz = 0;
	dval(&rv) = 0.;
	rvb = 0;
	nbits = fpi->nbits;
/* XXX no? skip_spaces */
	for(s = s00;;s++) switch(*s) {
		case '-':
			sign = 1;
			/* no break */
		case '+':
			if (*++s)
				goto break2;
			/* no break */
		case 0:
			sign = 0;
			irv = STRTOG_NoNumber;
			s = s00;
			goto ret;
		case '\t':
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case ' ':
			continue;
		default:
			goto break2;
		}
 break2:
/* XXX end of skip_spaces? */
	if (*s == '0') {
/*#ifndef NO_HEX_FP*/
		switch(s[1]) {
		  case 'x':
		  case 'X':
			irv = gethex(&s, fpi, exp, &rvb, sign);
			if (irv == STRTOG_NoNumber) {
				s = s00;
				sign = 0;
				}
			/* XXX denorm ? */
			goto ret;
		  }
/*#endif*/
/* . . . */
	}
/* SKIPPING
.
.
.
*/
 ret:
	if (denorm) {
		if (sudden_underflow) {
			rvb->wds = 0;
			irv = STRTOG_Underflow | STRTOG_Inexlo;
#ifndef NO_ERRNO
			errno = ERANGE;
#endif
			}
		else  {
			irv = (irv & ~STRTOG_Retmask) |
				(rvb->wds > 0 ? STRTOG_Denormal : STRTOG_Zero);
			if (irv & STRTOG_Inexact) {
				irv |= STRTOG_Underflow;
#ifndef NO_ERRNO
				errno = ERANGE;
#endif
				}
			}
		}
	if (se)
		*se = (char *)s;
	if (sign)
		irv |= STRTOG_Neg;
	if (rvb) {
		copybits(bits, nbits, rvb);
		Bfree(rvb);
		}
	return irv;
}


/* ## gethex.c ## */

 int
#ifdef KR_headers
gethex(sp, fpi, exp, bp, sign)
	CONST char **sp; FPI *fpi; Long *exp; Bigint **bp; int sign;
#else
gethex( CONST char **sp, FPI *fpi, Long *exp, Bigint **bp, int sign)
#endif
{
	Bigint *b;
	CONST unsigned char *decpt, *s0, *s, *s1;
	int big, esign, havedig, irv, j, k, n, n0, nbits, up, zret;
	ULong L, lostbits, *x;
	Long e, e1;
#ifdef USE_LOCALE
	int i;
#ifdef NO_LOCALE_CACHE
	const unsigned char *decimalpoint = (unsigned char*)localeconv()->decimal_point;
#else
	const unsigned char *decimalpoint;
	static unsigned char *decimalpoint_cache;
	if (!(s0 = decimalpoint_cache)) {
		s0 = (unsigned char*)localeconv()->decimal_point;
		if ((decimalpoint_cache = (char*)MALLOC(strlen(s0) + 1))) {
			strcpy(decimalpoint_cache, s0);
			s0 = decimalpoint_cache;
			}
		}
	decimalpoint = s0;
#endif
#endif

	/**** if (!hexdig['0']) hexdig_init_D2A(); ****/
	*bp = 0;
	havedig = 0;
	s0 = *(CONST unsigned char **)sp + 2;
	while(s0[havedig] == '0')
		havedig++;
	s0 += havedig;
	s = s0;
	decpt = 0;
	zret = 0;
	e = 0;
	if (hexdig[*s])
		havedig++;
	else {
		zret = 1;
#ifdef USE_LOCALE
		for(i = 0; decimalpoint[i]; ++i) {
			if (s[i] != decimalpoint[i])
				goto pcheck;
			}
		decpt = s += i;
#else
		if (*s != '.')
			goto pcheck;
		decpt = ++s;
#endif
		if (!hexdig[*s])
			goto pcheck;
		while(*s == '0')
			s++;
		if (hexdig[*s])
			zret = 0;
		havedig = 1;
		s0 = s;
		}
	while(hexdig[*s])
		s++;
#ifdef USE_LOCALE
	if (*s == *decimalpoint && !decpt) {
		for(i = 1; decimalpoint[i]; ++i) {
			if (s[i] != decimalpoint[i])
				goto pcheck;
			}
		decpt = s += i;
#else
	if (*s == '.' && !decpt) {
		decpt = ++s;
#endif
		while(hexdig[*s])
			s++;
		}/*}*/
	if (decpt)
		e = -(((Long)(s-decpt)) << 2);
 pcheck:
	s1 = s;
	big = esign = 0;
	switch(*s) {
	  case 'p':
	  case 'P':
		switch(*++s) {
		  case '-':
			esign = 1;
			/* no break */
		  case '+':
			s++;
		  }
		if ((n = hexdig[*s]) == 0 || n > 0x19) {
			s = s1;
			break;
			}
		e1 = n - 0x10;
		while((n = hexdig[*++s]) !=0 && n <= 0x19) {
			if (e1 & 0xf8000000)
				big = 1;
			e1 = 10*e1 + n - 0x10;
			}
		if (esign)
			e1 = -e1;
		e += e1;
	  }
	*sp = (char*)s;
	if (!havedig)
		*sp = (char*)s0 - 1;
	if (zret)
		return STRTOG_Zero;
	if (big) {
		if (esign) {
			switch(fpi->rounding) {
			  case FPI_Round_up:
				if (sign)
					break;
				goto ret_tiny;
			  case FPI_Round_down:
				if (!sign)
					break;
				goto ret_tiny;
			  }
			goto retz;
 ret_tiny:
			b = Balloc(0);
			b->wds = 1;
			b->x[0] = 1;
			goto dret;
			}
		switch(fpi->rounding) {
		  case FPI_Round_near:
			goto ovfl1;
		  case FPI_Round_up:
			if (!sign)
				goto ovfl1;
			goto ret_big;
		  case FPI_Round_down:
			if (sign)
				goto ovfl1;
			goto ret_big;
		  }
 ret_big:
		nbits = fpi->nbits;
		n0 = n = nbits >> kshift;
		if (nbits & kmask)
			++n;
		for(j = n, k = 0; j >>= 1; ++k);
		*bp = b = Balloc(k);
		b->wds = n;
		for(j = 0; j < n0; ++j)
			b->x[j] = ALL_ON;
		if (n > n0)
			b->x[j] = ULbits >> (ULbits - (nbits & kmask));
		*exp = fpi->emin;
		return STRTOG_Normal | STRTOG_Inexlo;
		}
	n = s1 - s0 - 1;
	for(k = 0; n > (1 << (kshift-2)) - 1; n >>= 1)
		k++;
	b = Balloc(k);
	x = b->x;
	n = 0;
	L = 0;
#ifdef USE_LOCALE
	for(i = 0; decimalpoint[i+1]; ++i);
#endif
	while(s1 > s0) {
#ifdef USE_LOCALE
		if (*--s1 == decimalpoint[i]) {
			s1 -= i;
			continue;
			}
#else
		if (*--s1 == '.')
			continue;
#endif
		if (n == ULbits) {
			*x++ = L;
			L = 0;
			n = 0;
			}
		L |= (hexdig[*s1] & 0x0f) << n;
		n += 4;
		}
	*x++ = L;
	b->wds = n = x - b->x;
	n = ULbits*n - hi0bits(L);
	nbits = fpi->nbits;
	lostbits = 0;
	x = b->x;
	if (n > nbits) {
		n -= nbits;
		if (any_on(b,n)) {
			lostbits = 1;
			k = n - 1;
			if (x[k>>kshift] & 1 << (k & kmask)) {
				lostbits = 2;
				if (k > 0 && any_on(b,k))
					lostbits = 3;
				}
			}
		rshift(b, n);
		e += n;
		}
	else if (n < nbits) {
		n = nbits - n;
		b = lshift(b, n);
		e -= n;
		x = b->x;
		}
	if (e > fpi->emax) {
 ovfl:
		Bfree(b);
 ovfl1:
#ifndef NO_ERRNO
		errno = ERANGE;
#endif
		return STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
		}
	irv = STRTOG_Normal;
	if (e < fpi->emin) {
		irv = STRTOG_Denormal;
		n = fpi->emin - e;
		if (n >= nbits) {
			switch (fpi->rounding) {
			  case FPI_Round_near:
				if (n == nbits && (n < 2 || any_on(b,n-1)))
					goto one_bit;
				break;
			  case FPI_Round_up:
				if (!sign)
					goto one_bit;
				break;
			  case FPI_Round_down:
				if (sign) {
 one_bit:
					x[0] = b->wds = 1;
 dret:
					*bp = b;
					*exp = fpi->emin;
#ifndef NO_ERRNO
					errno = ERANGE;
#endif
					return STRTOG_Denormal | STRTOG_Inexhi
						| STRTOG_Underflow;
					}
			  }
			Bfree(b);
 retz:
#ifndef NO_ERRNO
			errno = ERANGE;
#endif
			return STRTOG_Zero | STRTOG_Inexlo | STRTOG_Underflow;
			}
		k = n - 1;
		if (lostbits)
			lostbits = 1;
		else if (k > 0)
			lostbits = any_on(b,k);
		if (x[k>>kshift] & 1 << (k & kmask))
			lostbits |= 2;
		nbits -= n;
		rshift(b,n);
		e = fpi->emin;
		}
	if (lostbits) {
		up = 0;
		switch(fpi->rounding) {
		  case FPI_Round_zero:
			break;
		  case FPI_Round_near:
			if (lostbits & 2
			 && (lostbits | x[0]) & 1)
				up = 1;
			break;
		  case FPI_Round_up:
			up = 1 - sign;
			break;
		  case FPI_Round_down:
			up = sign;
		  }
		if (up) {
			k = b->wds;
			b = increment(b);
			x = b->x;
			if (irv == STRTOG_Denormal) {
				if (nbits == fpi->nbits - 1
				 && x[nbits >> kshift] & 1 << (nbits & kmask))
					irv =  STRTOG_Normal;
				}
			else if (b->wds > k
			 || ((n = nbits & kmask) !=0
			      && hi0bits(x[k-1]) < 32-n)) {
				rshift(b,1);
				if (++e > fpi->emax)
					goto ovfl;
				}
			irv |= STRTOG_Inexhi;
			}
		else
			irv |= STRTOG_Inexlo;
		}
	*bp = b;
	*exp = e;
	return irv;
	}

/* ## hd_init.c ## (shared with hexnan.c, not used) */

#if 0
 unsigned char hexdig[256];

 static void
#ifdef KR_headers
htinit(h, s, inc) unsigned char *h; unsigned char *s; int inc;
#else
htinit(unsigned char *h, unsigned char *s, int inc)
#endif
{
	int i, j;
	for(i = 0; (j = s[i]) !=0; i++)
		h[j] = i + inc;
	}

 void
hexdig_init_D2A(Void)	/* Use of hexdig_init omitted 20121220 to avoid a */
			/* race condition when multiple threads are used. */
{
#define USC (unsigned char *)
	htinit(hexdig, USC "0123456789", 0x10);
	htinit(hexdig, USC "abcdef", 0x10 + 10);
	htinit(hexdig, USC "ABCDEF", 0x10 + 10);
	}
#else
 unsigned char hexdig[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	16,17,18,19,20,21,22,23,24,25,0,0,0,0,0,0,
	0,26,27,28,29,30,31,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,26,27,28,29,30,31,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};
#endif

/* ##END## hd_init.c ## */

#ifdef not_in /* Reference: scan.l */

/* %}
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
P			[Pp][+-]?{D}+
FS			(f|F|l|L|i)*
%{ */

/* %%
{D}+{E}{FS}?		{ yylval.nodep = floatcon(yytext); return(C_FCON); }
{D}*"."{D}+({E})?{FS}?	{ yylval.nodep = floatcon(yytext); return(C_FCON); }
{D}+"."{D}*({E})?{FS}?	{ yylval.nodep = floatcon(yytext); return(C_FCON); }
0[xX]{H}*"."{H}+{P}{FS}? { yylval.nodep = fhexcon(yytext); return(C_FCON); }
0[xX]{H}+"."{P}{FS}?	{ yylval.nodep = fhexcon(yytext); return(C_FCON); }
0[xX]{H}+{P}{FS}?	{ yylval.nodep = fhexcon(yytext); return(C_FCON); }
%% */

/* #ifndef SOFTFLOAT */

static long double
typround(long double dc, char *e, TWORD *tw)
{
	int im = 0;

	*tw = DOUBLE;
	for (; *e; e++) {
		switch (*e) {
		case 'f':
		case 'F':
			*tw = FLOAT;
			dc = (float)dc;
			break;
		case 'l':
		case 'L':
			*tw = ctype(LDOUBLE);
			break;
		case 'i':
		case 'I':
			im = 1;
			break;
		}
	}
	if (*tw == DOUBLE)
		dc = (double)dc;
#ifndef NO_COMPLEX
	if (im)
		*tw += (FIMAG-FLOAT);
#endif
	return dc;
}

/*
 * XXX floatcon() and fhexcon() should be in support libraries for
 * the target floating point.
 */
static NODE *
f2(char *str)
{
	TWORD tw;
	NODE *p;
	long double dc;
	char *eptr;

#ifdef HAVE_STRTOLD
	dc = strtold(str, &eptr); /* XXX - avoid strtod() */
#else
	dc = strtod(str, &eptr); /* XXX - avoid strtod() */
#endif
	dc = typround(dc, eptr, &tw);
	p = block(FCON, NIL, NIL, tw, 0, 0);
	p->n_dcon = dc;
	return p;
}

NODE *
floatcon(char *s)
{
	return f2(s);
}

static int
h2n(int ch)
{
	if (ch >= '0' && ch <= '9')
		return ch - '0';
	if (ch >= 'a' && ch <= 'f')
		return ch - 'a' + 10;
	return ch - 'A' + 10;
	
}

/*
 * Get exponent.  Max size is 16 bits.
 */
static int
fdecl(char *c, char **ep)
{
	int minus = 0;
	int val = 0;

	if (*c == '+')
		c++;
	else if (*c == '-')
		minus = 1, c++;

	while (isdigit((int)*c)) {
		val *= 10;
		val += *c++ - '0';
		if (val > 32769)
			val = 32769;
	}
	if (minus)
		val = -val;
	*ep = c;
	return val;
}

NODE *
fhexcon(char *c)
{
	TWORD tw;
	char *ep;
	long double d;
	int i, ed;
	NODE *p;

	d = 0.0;
	ed = 0;
	c+= 2; /* skip 0x */
#define FSET(n) { d *= 2; if (i & n) d += 1.0; }
	for (; *c != '.' && *c != 'p' && *c != 'P'; c++) {
		i = h2n(*c);
		FSET(8); FSET(4); FSET(2); FSET(1);
	}
	if (*c != '.' && *c != 'p' && *c != 'P')
		cerror("fhexcon");
	if (*c == '.') {
		c++;
		for (; *c != 'p' && *c != 'P'; c++) {
			i = h2n(*c);
			FSET(8); FSET(4); FSET(2); FSET(1);
			ed -= 4;
		}
	}
	if (*c != 'P' && *c != 'p')
		cerror("fhexcon2");
	c++;
	ed += fdecl(c, &ep);

	/* avoid looping in vain. Idea from Fred J. Tydeman */
	if (ed > 32769) ed = 32769;
	if (ed < -32769) ed = -32769;

	while (ed > 0)
		d *= 2, ed--;
	while (ed < 0)
		d /= 2, ed++;
	d = typround(d, ep, &tw);
	p = block(FCON, NIL, NIL, tw, 0, 0);
	p->n_dcon = d;
	return p;
}
#endif

#ifndef MODTYPE
#define MODTYPE(x,y)	x = ((x)&(~BTMASK))|(y)	/* set basic type of x to y */
#endif
/* From arch/vax/local.c */
#ifdef for_reference
/* map types which are not defined on the local machine */
TWORD ctype(TWORD type) {
	switch( BTYPE(type) ){

	case LONG:
		MODTYPE(type,INT);
		break;

	case ULONG:
		MODTYPE(type,UNSIGNED);
		break;

	case LDOUBLE:	/* for now */
		MODTYPE(type,DOUBLE);
		}
	return( type );
	}
#endif


/*
 * Generate defines for the needed hardops.
 */
#include "pass2.h"

int chkop[MAXOP];

void mktables(void);

char *cname = "external.c";
char *hname = "external.h";
FILE *fc, *fh;

/*
 * masks for matching dope with shapes
 */
int mamask[] = {
        SIMPFLG,                /* OPSIMP */
        SIMPFLG|ASGFLG,         /* ASG OPSIMP */
        COMMFLG,        /* OPCOMM */
        COMMFLG|ASGFLG, /* ASG OPCOMM */
        MULFLG,         /* OPMUL */
        MULFLG|ASGFLG,  /* ASG OPMUL */
        DIVFLG,         /* OPDIV */
        DIVFLG|ASGFLG,  /* ASG OPDIV */
        UTYPE,          /* OPUNARY */
        TYFLG,          /* ASG OPUNARY is senseless */
        LTYPE,          /* OPLEAF */
        TYFLG,          /* ASG OPLEAF is senseless */
        0,              /* OPANY */
        ASGOPFLG|ASGFLG,        /* ASG OPANY */
        LOGFLG,         /* OPLOG */
        TYFLG,          /* ASG OPLOG is senseless */
        FLOFLG,         /* OPFLOAT */
        FLOFLG|ASGFLG,  /* ASG OPFLOAT */
        SHFFLG,         /* OPSHFT */
        SHFFLG|ASGFLG,  /* ASG OPSHIFT */
        SPFLG,          /* OPLTYPE */
        TYFLG,          /* ASG OPLTYPE is senseless */
        };


struct checks {
	int op, type;
	char *name;
} checks[] = {
	{ MUL, TLONGLONG, "SMULLL", },
	{ DIV, TLONGLONG, "SDIVLL", },
	{ MOD, TLONGLONG, "SMODLL", },
	{ PLUS, TLONGLONG, "SPLUSLL", },
	{ MINUS, TLONGLONG, "SMINUSLL", },
	{ MUL, TULONGLONG, "UMULLL", },
	{ DIV, TULONGLONG, "UDIVLL", },
	{ MOD, TULONGLONG, "UMODLL", },
	{ PLUS, TULONGLONG, "UPLUSLL", },
	{ MINUS, TULONGLONG, "UMINUSLL", },
	{ 0, 0, 0, },
};

int rstatus[] = { RSTATUS };
int roverlap[MAXREGS][MAXREGS] = { ROVERLAP };
int regclassmap[NUMCLASS][MAXREGS];

static void
compl(struct optab *q, char *str)
{
	printf("table entry %d, op %s: %s\n", q - table, opst[q->op], str);
}

int
main(int argc, char *argv[])
{
	struct optab *q;
	struct checks *ch;
	int i, j;
	char *bitary;
	int bitsz, rval;

	mkdope();

	for (q = table; q->op != FREE; q++) {
		if (q->op >= OPSIMP)
			continue;
		if ((q->ltype & TLONGLONG) &&
		    (q->rtype & TLONGLONG))
			chkop[q->op] |= TLONGLONG;
		if ((q->ltype & TULONGLONG) &&
		    (q->rtype & TULONGLONG))
			chkop[q->op] |= TULONGLONG;
	}
	if ((fc = fopen(cname, "w")) == NULL) {
		perror("open cfile");
		return(1);
	}
	if ((fh = fopen(hname, "w")) == NULL) {
		perror("open hfile");
		return(1);
	}
	for (ch = checks; ch->op != 0; ch++) {
		if ((chkop[ch->op] & ch->type) == 0)
			fprintf(fh, "#define NEED_%s\n", ch->name);
	}

	/* create fast-lookup tables */
	mktables();

	/* create efficient bitset sizes */
	if (sizeof(long) == 8) { /* 64-bit arch */
		bitary = "long";
		bitsz = 64;
	} else {
		bitary = "int";
		bitsz = sizeof(int) == 4 ? 32 : 16;
	}
	fprintf(fh, "#define NUMBITS %d\n", bitsz);
	fprintf(fh, "#define BITSET(arr, bit) "
	     "(arr[bit/NUMBITS] |= (1 << (bit & (NUMBITS-1))))\n");
	fprintf(fh, "#define BITCLEAR(arr, bit) "
	     "(arr[bit/NUMBITS] &= ~(1 << (bit & (NUMBITS-1))))\n");
	fprintf(fh, "#define TESTBIT(arr, bit) "
	     "(arr[bit/NUMBITS] & (1 << (bit & (NUMBITS-1))))\n");
	fprintf(fh, "typedef %s bittype;\n", bitary);

	/* register class definitions, used by graph-coloring */
	/* TODO */

	/* Sanity-check the table */
	rval = 0;
	for (q = table; q->op != FREE; q++) {
		if (q->op == ASSIGN) {
#define	F(x) (q->visit & x && q->rewrite & (RLEFT|RRIGHT) && \
		    q->lshape & ~x && q->rshape & ~x)
			if (F(INAREG) || F(INBREG) || F(INCREG) || F(INDREG)) {
				compl(q, "may match without result register");
				rval++;
			}
#undef F
			if (q->rewrite & RESC1)
				compl(q, "reclaim of needs in ASSIGN node");
			if (q->needs & (NAREG|NBREG))
				compl(q, "needs in ASSIGN node");
		}
	}

	/* print out list of scratched and permanent registers */
	fprintf(fh, "extern int tempregs[], permregs[];\n");
	fprintf(fc, "int tempregs[] = { ");
	for (i = j = 0; i < MAXREGS; i++)
		if (rstatus[i] & TEMPREG)
			fprintf(fc, "%d, ", i), j++;
	fprintf(fc, "-1 };\n");
	fprintf(fh, "#define NTEMPREG %d\n", j+1);
	fprintf(fh, "#define FREGS %d\n", j);	/* XXX - to die */
	fprintf(fc, "int permregs[] = { ");
	for (i = j = 0; i < MAXREGS; i++)
		if (rstatus[i] & PERMREG)
			fprintf(fc, "%d, ", i), j++;
	fprintf(fc, "-1 };\n");
	fprintf(fh, "#define NPERMREG %d\n", j+1);

	/*
	 * The register allocator uses bitmasks of registers for each class.
	 */
	areg = breg = creg = dreg = 0;
	for (i = 0; i < MAXREGS; i++) {
		regclassmap[0][i] = regclassmap[1][i] = regclassmap[2][i] = 
		    regclassmap[3][i] = -1;
		if (rstatus[i] & SAREG) regclassmap[0][i] = areg++;
		if (rstatus[i] & SBREG) regclassmap[1][i] = breg++;
		if (rstatus[i] & SCREG) regclassmap[2][i] = creg++;
		if (rstatus[i] & SDREG) regclassmap[3][i] = dreg++;
	}
	fprintf(fh, "#define AREGCNT %d\n", areg);
	fprintf(fh, "#define BREGCNT %d\n", breg);
	fprintf(fh, "#define CREGCNT %d\n", creg);
	fprintf(fh, "#define DREGCNT %d\n", dreg);
	if (areg > bitsz)
		printf("%d regs in class A (max %d)\n", areg, bitsz), rval++;
	if (breg > bitsz)
		printf("%d regs in class B (max %d)\n", breg, bitsz), rval++;
	if (creg > bitsz)
		printf("%d regs in class C (max %d)\n", creg, bitsz), rval++;
	if (dreg > bitsz)
		printf("%d regs in class D (max %d)\n", dreg, bitsz), rval++;

static int amap[MAXREGS][NUMCLASS] = {
	{ 0,1,2,3 },

	for (i = 0; i < MAXREGS; i++) {
		fprintf(fc, "\t{ ");
		bm = 0;
		for (j = 0; roverlay[i][j] >= 0; j++)
			if (rstatus[roverlay[i][j]] & SAREG)
				bm |= (1 << regclassmap[0][i]);


		F�r varje register i roverlay[i]:
			om registret ing�r i klassen:
				s�tt regnumbitten f�r print.



	fprintf(fc, "int\naliasmap(int thisclass, int regnum)\n}\n");
	fprintf(fc, "	return amap[regnum][thisclass];\n}\n");

	fclose(fc);
	fclose(fh);
	return rval;
}

#define	P(x)	fprintf x

void
mktables()
{
	struct optab *op;
	int mxalen = 0, curalen;
	int i;

//	P((fc, "#include \"pass2.h\"\n\n"));
	for (i = 0; i <= MAXOP; i++) {
		curalen = 0;
		P((fc, "static int op%d[] = { ", i));
		if (dope[i] != 0)
		for (op = table; op->op != FREE; op++) {
			if (op->op < OPSIMP) {
				if (op->op == i) {
					P((fc, "%d, ", op - table));
					curalen++;
				}
			} else {
				int opmtemp;
				if ((opmtemp=mamask[op->op - OPSIMP])&SPFLG) {
					if (i==NAME || i==ICON || i==TEMP ||
					    i==OREG || i == REG) {
						P((fc, "%d, ", op - table));
						curalen++;
					}
				} else if ((dope[i]&(opmtemp|ASGFLG))==opmtemp){
					P((fc, "%d, ", op - table));
					curalen++;
				}
			}
		}
		if (curalen > mxalen)
			mxalen = curalen;
		P((fc, "-1 };\n"));
	}
	P((fc, "\n"));

	P((fc, "int *qtable[] = { \n"));
	for (i = 0; i <= MAXOP; i++) {
		P((fc, "	op%d,\n", i));
	}
	P((fc, "};\n"));
	P((fh, "#define MAXOPLEN %d\n", mxalen+1));
}

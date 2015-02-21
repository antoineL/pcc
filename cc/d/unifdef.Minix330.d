--- unifdef.c.txt	Wed Aug 16 19:17:40 2006
+++ unifdef.Minix330.c	Sat Jan 03 13:39:36 2015
@@ -1,14 +1,12 @@
+/*	$NetBSD: unifdef.c,v 1.22 2012/10/13 18:26:03 christos Exp $	*/
+
 /*
- * Copyright (c) 2002 - 2005 Tony Finch <dot@dotat.at>.  All rights reserved.
- *
- * This code is derived from software contributed to Berkeley by Dave Yost.
- * It was rewritten to support ANSI C by Tony Finch. The original version of
- * unifdef carried the following copyright notice. None of its code remains
- * in this version (though some of the names remain).
- *
  * Copyright (c) 1985, 1993
  *	The Regents of the University of California.  All rights reserved.
  *
+ * This code is derived from software contributed to Berkeley by
+ * Dave Yost. It was rewritten to support ANSI C by Tony Finch.
+ *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
@@ -17,11 +15,14 @@
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in the
  *    documentation and/or other materials provided with the distribution.
+ * 3. Neither the name of the University nor the names of its contributors
+ *    may be used to endorse or promote products derived from this software
+ *    without specific prior written permission.
  *
- * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
+ * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
- * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
+ * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
@@ -31,12 +32,58 @@
  * SUCH DAMAGE.
  */
 
-static const char * const copyright[] = {
-    "@(#) Copyright (c) 1985, 1993\n\
-	The Regents of the University of California.  All rights reserved.\n",
-    "@(#)unifdef.c	8.1 (Berkeley) 6/6/93",
-    "$dotat: things/unifdef.c,v 1.171 2005/03/08 12:38:48 fanf2 Exp $",
-};
+/*
+ * Copyright (c) 2002, 2003 Tony Finch <dot@dotat.at>
+ *
+ * This code is derived from software contributed to Berkeley by
+ * Dave Yost. It was rewritten to support ANSI C by Tony Finch.
+ *
+ * Redistribution and use in source and binary forms, with or without
+ * modification, are permitted provided that the following conditions
+ * are met:
+ * 1. Redistributions of source code must retain the above copyright
+ *    notice, this list of conditions and the following disclaimer.
+ * 2. Redistributions in binary form must reproduce the above copyright
+ *    notice, this list of conditions and the following disclaimer in the
+ *    documentation and/or other materials provided with the distribution.
+ * 3. All advertising materials mentioning features or use of this software
+ *    must display the following acknowledgement:
+ *	This product includes software developed by the University of
+ *	California, Berkeley and its contributors.
+ * 4. Neither the name of the University nor the names of its contributors
+ *    may be used to endorse or promote products derived from this software
+ *    without specific prior written permission.
+ *
+ * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
+ * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
+ * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
+ * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
+ * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
+ * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
+ * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
+ * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
+ * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
+ * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
+ * SUCH DAMAGE.
+ */
+
+#include <sys/cdefs.h>
+
+#ifndef lint
+#if 0
+static const char copyright[] =
+"@(#) Copyright (c) 1985, 1993\n\
+	The Regents of the University of California.  All rights reserved.\n";
+#endif
+#ifdef __IDSTRING
+__IDSTRING(Berkeley, "@(#)unifdef.c	8.1 (Berkeley) 6/6/93");
+__IDSTRING(NetBSD, "$NetBSD: unifdef.c,v 1.22 2012/10/13 18:26:03 christos Exp $");
+__IDSTRING(dotat, "$dotat: things/unifdef.c,v 1.161 2003/07/01 15:32:48 fanf2 Exp $");
+#endif
+#endif /* not lint */
+#ifdef __FBSDID
+__FBSDID("$FreeBSD: src/usr.bin/unifdef/unifdef.c,v 1.18 2003/07/01 15:30:43 fanf Exp $");
+#endif
 
 /*
  * unifdef - remove ifdef'ed lines
@@ -47,6 +94,7 @@
  *      provide an option which will check symbols after
  *        #else's and #endif's to see that they match their
  *        corresponding #ifdef or #ifndef
+ *      generate #line directives in place of deleted code
  *
  *   The first two items above require better buffer handling, which would
  *     also make it possible to handle all "dodgy" directives correctly.
@@ -54,13 +102,18 @@
 
 #include <ctype.h>
 #include <err.h>
+#include <libgen.h>
 #include <stdarg.h>
-#include <stdbool.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <unistd.h>
 
+#include <sys/param.h>
+#include <sys/stat.h>
+
+#include "stdbool.h"
+
 /* types of input lines: */
 typedef enum {
 	LT_TRUEI,		/* a true #if with ignore flag */
@@ -118,13 +171,11 @@
 	C_COMMENT,		/* in a comment like this one */
 	CXX_COMMENT,		/* between // and end of line */
 	STARTING_COMMENT,	/* just after slash-backslash-newline */
-	FINISHING_COMMENT,	/* star-backslash-newline in a C comment */
-	CHAR_LITERAL,		/* inside '' */
-	STRING_LITERAL		/* inside "" */
+	FINISHING_COMMENT	/* star-backslash-newline in a C comment */
 } Comment_state;
 
 static char const * const comment_name[] = {
-	"NO", "C", "CXX", "STARTING", "FINISHING", "CHAR", "STRING"
+	"NO", "C", "CXX", "STARTING", "FINISHING"
 };
 
 /* state of preprocessor line parser */
@@ -160,7 +211,6 @@
 static bool             iocccok;		/* -e: fewer IOCCC errors */
 static bool             killconsts;		/* -k: eval constant #ifs */
 static bool             lnblank;		/* -l: blank deleted lines */
-static bool             lnnum;			/* -n: add #line directives */
 static bool             symlist;		/* -s: output symbol list */
 static bool             text;			/* -t: this is a text file */
 
@@ -170,8 +220,12 @@
 static int              nsyms;			/* number of symbols */
 
 static FILE            *input;			/* input file pointer */
+static FILE            *output;			/* output file pointer */
 static const char      *filename;		/* input file name */
+static char            *ofilename;		/* output file name */
+static char             tmpname[MAXPATHLEN];	/* used when overwriting */
 static int              linenum;		/* current line number */
+static int              overwriting;		/* output overwrites input */
 
 static char             tline[MAXLINE+EDITSLOP];/* input buffer plus space */
 static char            *keyword;		/* used for editing #elif's */
@@ -182,30 +236,28 @@
 static bool             ignoring[MAXDEPTH];	/* ignore comments state */
 static int              stifline[MAXDEPTH];	/* start of current #if */
 static int              depth;			/* current #if nesting */
-static int              delcount;		/* count of deleted lines */
 static bool             keepthis;		/* don't delete constant #if */
 
 static int              exitstat;		/* program exit status */
 
 static void             addsym(bool, bool, char *);
-static void             debug(const char *, ...);
-static void             done(void);
-static void             error(const char *);
+static void             debug(const char *, ...) __printflike(1, 2);
+__dead static void      done(void);
+__dead static void      error(const char *);
 static int              findsym(const char *);
 static void             flushline(bool);
-static Linetype         getline(void);
+static Linetype         get_line(void);
 static Linetype         ifeval(const char **);
 static void             ignoreoff(void);
 static void             ignoreon(void);
 static void             keywordedit(const char *);
 static void             nest(void);
-static void             process(void);
+__dead static void      process(void);
 static const char      *skipcomment(const char *);
 static const char      *skipsym(const char *);
 static void             state(Ifstate);
 static int              strlcmp(const char *, const char *, size_t);
-static void             unnest(void);
-static void             usage(void);
+__dead static void      usage(void);
 
 #define endsym(c) (!isalpha((unsigned char)c) && !isdigit((unsigned char)c) && c != '_')
 
@@ -216,8 +268,9 @@
 main(int argc, char *argv[])
 {
 	int opt;
+	struct stat isb, osb;
 
-	while ((opt = getopt(argc, argv, "i:D:U:I:cdeklnst")) != -1)
+	while ((opt = getopt(argc, argv, "i:D:U:I:o:cdeklst")) != -1)
 		switch (opt) {
 		case 'i': /* treat stuff controlled by these symbols as text */
 			/*
@@ -257,8 +310,8 @@
 		case 'l': /* blank deleted lines instead of omitting them */
 			lnblank = true;
 			break;
-		case 'n': /* add #line directive after deleted lines */
-			lnnum = true;
+		case 'o': /* output to a file */
+			ofilename = optarg;
 			break;
 		case 's': /* only output list of symbols that control #ifs */
 			symlist = true;
@@ -271,6 +324,10 @@
 		}
 	argc -= optind;
 	argv += optind;
+	if (nsyms == 0 && !symlist) {
+		warnx("must -D or -U at least one symbol");
+		usage();
+	}
 	if (argc > 1) {
 		errx(2, "can only do one file");
 	} else if (argc == 1 && strcmp(*argv, "-") != 0) {
@@ -282,6 +339,32 @@
 		filename = "[stdin]";
 		input = stdin;
 	}
+	if (ofilename == NULL) {
+		output = stdout;
+	} else {
+		if (stat(ofilename, &osb) == 0) {
+			if (fstat(fileno(input), &isb) != 0)
+				err(2, "can't fstat %s", filename);
+
+			overwriting = (osb.st_dev == isb.st_dev &&
+			    osb.st_ino == isb.st_ino);
+		}
+		if (overwriting) {
+			int ofd;
+
+			snprintf(tmpname, sizeof(tmpname), "%s/unifdef.XXXXXX",
+				 dirname(ofilename));
+			if ((ofd = mkstemp(tmpname)) != -1)
+				output = fdopen(ofd, "w+");
+			if (output == NULL)
+				err(2, "can't create temporary file");
+			fchmod(ofd, isb.st_mode & ACCESSPERMS);
+		} else {
+			output = fopen(ofilename, "w");
+			if (output == NULL)
+				err(2, "can't open %s", ofilename);
+		}
+	}
 	process();
 	abort(); /* bug */
 }
@@ -289,7 +372,7 @@
 static void
 usage(void)
 {
-	fprintf(stderr, "usage: unifdef [-cdeklnst]"
+	fprintf(stderr, "usage: unifdef [-cdeklst] [-o output]"
 	    " [-Dsym[=val]] [-Usym] [-iDsym[=val]] [-iUsym] ... [file]\n");
 	exit(2);
 }
@@ -326,11 +409,11 @@
 typedef void state_fn(void);
 
 /* report an error */
-static void Eelif (void) { error("Inappropriate #elif"); }
-static void Eelse (void) { error("Inappropriate #else"); }
-static void Eendif(void) { error("Inappropriate #endif"); }
-static void Eeof  (void) { error("Premature EOF"); }
-static void Eioccc(void) { error("Obfuscated preprocessor control line"); }
+__dead static void Eelif (void) { error("Inappropriate #elif"); }
+__dead static void Eelse (void) { error("Inappropriate #else"); }
+__dead static void Eendif(void) { error("Inappropriate #endif"); }
+__dead static void Eeof  (void) { error("Premature EOF"); }
+__dead static void Eioccc(void) { error("Obfuscated preprocessor control line"); }
 /* plain line handling */
 static void print (void) { flushline(true); }
 static void drop  (void) { flushline(false); }
@@ -341,21 +424,21 @@
 /* print/pass this block */
 static void Pelif (void) { print(); ignoreoff(); state(IS_PASS_MIDDLE); }
 static void Pelse (void) { print();              state(IS_PASS_ELSE); }
-static void Pendif(void) { print(); unnest(); }
+static void Pendif(void) { print(); --depth; }
 /* discard this block */
 static void Dfalse(void) { drop();  ignoreoff(); state(IS_FALSE_TRAILER); }
 static void Delif (void) { drop();  ignoreoff(); state(IS_FALSE_MIDDLE); }
 static void Delse (void) { drop();               state(IS_FALSE_ELSE); }
-static void Dendif(void) { drop();  unnest(); }
+static void Dendif(void) { drop();  --depth; }
 /* first line of group */
 static void Fdrop (void) { nest();  Dfalse(); }
 static void Fpass (void) { nest();  Pelif(); }
 static void Ftrue (void) { nest();  Strue(); }
 static void Ffalse(void) { nest();  Sfalse(); }
 /* variable pedantry for obfuscated lines */
-static void Oiffy (void) { if (!iocccok) Eioccc(); Fpass(); ignoreon(); }
-static void Oif   (void) { if (!iocccok) Eioccc(); Fpass(); }
-static void Oelif (void) { if (!iocccok) Eioccc(); Pelif(); }
+static void Oiffy (void) { if (iocccok) Fpass(); else Eioccc(); ignoreon(); }
+static void Oif   (void) { if (iocccok) Fpass(); else Eioccc(); }
+static void Oelif (void) { if (iocccok) Pelif(); else Eioccc(); }
 /* ignore comments in this block */
 static void Idrop (void) { Fdrop();  ignoreon(); }
 static void Itrue (void) { Ftrue();  ignoreon(); }
@@ -420,13 +503,21 @@
 {
 	if (incomment)
 		error("EOF in comment");
+	if (fclose(output)) {
+		if (overwriting) {
+			unlink(tmpname);
+			errx(2, "%s unchanged", ofilename);
+		}
+	}
+	if (overwriting && rename(tmpname, ofilename)) {
+		unlink(tmpname);
+		errx(2, "%s unchanged", ofilename);
+	}
 	exit(exitstat);
 }
 static void
 ignoreoff(void)
 {
-	if (depth == 0)
-		abort(); /* bug */
 	ignoring[depth] = ignoring[depth-1];
 }
 static void
@@ -449,13 +540,6 @@
 	stifline[depth] = linenum;
 }
 static void
-unnest(void)
-{
-	if (depth == 0)
-		abort(); /* bug */
-	depth -= 1;
-}
-static void
 state(Ifstate is)
 {
 	ifstate[depth] = is;
@@ -469,16 +553,12 @@
 {
 	if (symlist)
 		return;
-	if (keep ^ complement) {
-		if (lnnum && delcount > 0)
-			printf("#line %d\n", linenum);
-		fputs(tline, stdout);
-		delcount = 0;
-	} else {
+	if (keep ^ complement)
+		fputs(tline, output);
+	else {
 		if (lnblank)
-			putc('\n', stdout);
+			putc('\n', output);
 		exitstat = 1;
-		delcount += 1;
 	}
 }
 
@@ -492,7 +572,7 @@
 
 	for (;;) {
 		linenum++;
-		lineval = getline();
+		lineval = get_line();
 		trans_table[ifstate[depth]][lineval]();
 		debug("process %s -> %s depth %d",
 		    linetype_name[lineval],
@@ -506,7 +586,7 @@
  * help from skipcomment().
  */
 static Linetype
-getline(void)
+get_line(void)
 {
 	const char *cp;
 	int cursym;
@@ -575,9 +655,6 @@
 			if (incomment)
 				linestate = LS_DIRTY;
 		}
-		/* skipcomment should have changed the state */
-		if (linestate == LS_HASH)
-			abort(); /* bug */
 	}
 	if (linestate == LS_DIRTY) {
 		while (*cp != '\0')
@@ -656,31 +733,31 @@
 
 	cp = skipcomment(*cpp);
 	if (*cp == '!') {
-		debug("eval%d !", ops - eval_ops);
+		debug("eval%td !", ops - eval_ops);
 		cp++;
 		if (eval_unary(ops, valp, &cp) == LT_IF)
 			return (LT_IF);
 		*valp = !*valp;
 	} else if (*cp == '(') {
 		cp++;
-		debug("eval%d (", ops - eval_ops);
+		debug("eval%td (", ops - eval_ops);
 		if (eval_table(eval_ops, valp, &cp) == LT_IF)
 			return (LT_IF);
 		cp = skipcomment(cp);
 		if (*cp++ != ')')
 			return (LT_IF);
 	} else if (isdigit((unsigned char)*cp)) {
-		debug("eval%d number", ops - eval_ops);
+		debug("eval%td number", ops - eval_ops);
 		*valp = strtol(cp, &ep, 0);
 		cp = skipsym(cp);
 	} else if (strncmp(cp, "defined", 7) == 0 && endsym(cp[7])) {
 		cp = skipcomment(cp+7);
-		debug("eval%d defined", ops - eval_ops);
+		debug("eval%td defined", ops - eval_ops);
 		if (*cp++ != '(')
 			return (LT_IF);
 		cp = skipcomment(cp);
 		sym = findsym(cp);
-		if (sym < 0)
+		if (sym < 0 || symlist)
 			return (LT_IF);
 		*valp = (value[sym] != NULL);
 		cp = skipsym(cp);
@@ -689,9 +766,9 @@
 			return (LT_IF);
 		keepthis = false;
 	} else if (!endsym(*cp)) {
-		debug("eval%d symbol", ops - eval_ops);
+		debug("eval%td symbol", ops - eval_ops);
 		sym = findsym(cp);
-		if (sym < 0)
+		if (sym < 0 || symlist)
 			return (LT_IF);
 		if (value[sym] == NULL)
 			*valp = 0;
@@ -703,12 +780,12 @@
 		cp = skipsym(cp);
 		keepthis = false;
 	} else {
-		debug("eval%d bad expr", ops - eval_ops);
+		debug("eval%td bad expr", ops - eval_ops);
 		return (LT_IF);
 	}
 
 	*cpp = cp;
-	debug("eval%d = %d", ops - eval_ops, *valp);
+	debug("eval%td = %d", ops - eval_ops, *valp);
 	return (*valp ? LT_TRUE : LT_FALSE);
 }
 
@@ -722,7 +799,7 @@
 	const char *cp;
 	int val;
 
-	debug("eval%d", ops - eval_ops);
+	debug("eval%td", ops - eval_ops);
 	cp = *cpp;
 	if (ops->inner(ops+1, valp, &cp) == LT_IF)
 		return (LT_IF);
@@ -734,14 +811,14 @@
 		if (op->str == NULL)
 			break;
 		cp += strlen(op->str);
-		debug("eval%d %s", ops - eval_ops, op->str);
+		debug("eval%td %s", ops - eval_ops, op->str);
 		if (ops->inner(ops+1, &val, &cp) == LT_IF)
 			return (LT_IF);
 		*valp = op->fn(*valp, val);
 	}
 
 	*cpp = cp;
-	debug("eval%d = %d", ops - eval_ops, *valp);
+	debug("eval%td = %d", ops - eval_ops, *valp);
 	return (*valp ? LT_TRUE : LT_FALSE);
 }
 
@@ -764,10 +841,10 @@
 }
 
 /*
- * Skip over comments, strings, and character literals and stop at the
- * next character position that is not whitespace. Between calls we keep
- * the comment state in the global variable incomment, and we also adjust
- * the global variable linestate when we see a newline.
+ * Skip over comments and stop at the next character position that is
+ * not whitespace. Between calls we keep the comment state in the
+ * global variable incomment, and we also adjust the global variable
+ * linestate when we see a newline.
  * XXX: doesn't cope with the buffer splitting inside a state transition.
  */
 static const char *
@@ -794,14 +871,6 @@
 			} else if (strncmp(cp, "//", 2) == 0) {
 				incomment = CXX_COMMENT;
 				cp += 2;
-			} else if (strncmp(cp, "\'", 1) == 0) {
-				incomment = CHAR_LITERAL;
-				linestate = LS_DIRTY;
-				cp += 1;
-			} else if (strncmp(cp, "\"", 1) == 0) {
-				incomment = STRING_LITERAL;
-				linestate = LS_DIRTY;
-				cp += 1;
 			} else if (strncmp(cp, "\n", 1) == 0) {
 				linestate = LS_START;
 				cp += 1;
@@ -817,25 +886,6 @@
 			}
 			cp += 1;
 			continue;
-		case CHAR_LITERAL:
-		case STRING_LITERAL:
-			if ((incomment == CHAR_LITERAL && cp[0] == '\'') ||
-			    (incomment == STRING_LITERAL && cp[0] == '\"')) {
-				incomment = NO_COMMENT;
-				cp += 1;
-			} else if (cp[0] == '\\') {
-				if (cp[1] == '\0')
-					cp += 1;
-				else
-					cp += 2;
-			} else if (strncmp(cp, "\n", 1) == 0) {
-				if (incomment == CHAR_LITERAL)
-					error("unterminated char literal");
-				else
-					error("unterminated string literal");
-			} else
-				cp += 1;
-			continue;
 		case C_COMMENT:
 			if (strncmp(cp, "*\\\n", 3) == 0) {
 				incomment = FINISHING_COMMENT;
@@ -883,7 +933,7 @@
 }
 
 /*
- * Look for the symbol in the symbol table. If is is found, we return
+ * Look for the symbol in the symbol table. If it is found, we return
  * the symbol table index, else we return -1.
  */
 static int
@@ -895,11 +945,8 @@
 	cp = skipsym(str);
 	if (cp == str)
 		return (-1);
-	if (symlist) {
+	if (symlist)
 		printf("%.*s\n", (int)(cp-str), str);
-		/* we don't care about the value of the symbol */
-		return (0);
-	}
 	for (symind = 0; symind < nsyms; ++symind) {
 		if (strlcmp(symname[symind], str, cp-str) == 0) {
 			debug("findsym %s %s", symname[symind],
@@ -981,5 +1028,10 @@
 	else
 		warnx("%s: %d: %s (#if line %d depth %d)",
 		    filename, linenum, msg, stifline[depth], depth);
+	fclose(output);
+	if (overwriting) {
+		unlink(tmpname);
+		errx(2, "%s unchanged", ofilename);
+	}
 	errx(2, "output may be truncated");
 }

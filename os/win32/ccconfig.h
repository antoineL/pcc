/*	$Id$	*/

/*
 * Currently only supports console applications.
 */

#if defined(MSLINKER)
/* requires microsoft toolkit headers and llnker */
#define	CPPADD { "-DWIN32", "-D_WIN32", NULL }
#define DEFLIBS { "/subsystem:console", "msvcrt.lib", "libpcc.a", NULL }
#else
/* common cpp predefines */
#define	CPPADD { "-DWIN32", "-D_WIN32", "-D__MSVCRT__", "-D__MINGW32__", NULL }

/* host-dependent */
/* requires w32api-3.2.tar.gz and mingw-runtime-3.14.tar.gz */
#define CRT0		"crt2.o"
#define CRT0_S		"dllcrt2.o"	/* not foreseen so far */
#define GCRT0		"gcrt2.o"	/* in _addition_ to either crt2.o or dllcrt2.o */
#define	CRTBEGIN_S	"crtbegin.o"
#define	CRTEND_S	"crtend.o"
#define CRTI		0
#define CRTN		0
/* MingW cannot do -static linking */

#define WIN32_LIBC	"-lmsvcrt"
#define MINGW_RTLIBS	"-lmoldname", "-lmingwex", "-lmingw32"
#define MINGW_SYSLIBS	"-luser32", "-lkernel32" /* ,"-ladvapi32", "-lshell32" */
#define	DEFLIBS		{ MINGW_RTLIBS, "-lpcc", WIN32_LIBC, MINGW_SYSLIBS, MINGW_RTLIBS, WIN32_LIBC, 0 }
#define	DEFPROFLIBS	{ MINGW_RTLIBS, "-lpcc", WIN32_LIBC, "-lgmon", MINGW_SYSLIBS, MINGW_RTLIBS, WIN32_LIBC, 0 }
#define	DEFCXXLIBS	{ "-lp++", MINGW_RTLIBS, "-lpcc", WIN32_LIBC, MINGW_SYSLIBS, MINGW_RTLIBS, WIN32_LIBC, 0 }

#endif

#define CPPMDADD { "-D__i386__", NULL }

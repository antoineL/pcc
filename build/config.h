/* config.h.in.  Generated from configure.ac by autoheader.  */
/* Stripped from everything relevant to target: *ABI, TARGET_*_ENDIAN, WCHAR_*, STABS, TLS */

/*** C compiler toolchain ***/

#ifdef _MSC_VER
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif
#ifndef _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE
#endif
#ifndef _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE
#endif
#endif

#if 0 /* XXX check this */
#if !defined(vsnprintf)
#if defined(_MSC_VER) && _MSC_VER >= 1200 && _MSC_VER < 1500
#define vsnprintf _vsnprintf
#endif
#endif
/* windows defines (u)uintptr_t in stddef.h, not inttypes.h */
#include <stddef.h>
#if !defined(snprintf)
#define snprintf _snprintf
#endif

#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#define inline __inline
#endif

/* Define to 1 if you have the `basename' function. */
/* #undef HAVE_BASENAME */

/* Define to 1 if printf supports C99 size specifiers */
/* #undef HAVE_C99_FORMAT */

/* Define to 1 if you have the `ffs' function. */
/* #undef HAVE_FFS */

/* Define to 1 if you have the `getopt' function. */
/* #undef HAVE_GETOPT */

/* Define to 1 if you have the <inttypes.h> header file. */
#if defined(_MSC_VER) || (_MSC_VER >= 1600)
/* #define HAVE_INTTYPES_H 1 */
#else
#define HAVE_INTTYPES_H 1
#endif

/* Define to 1 if you have the <libgen.h> header file. */
/* #undef HAVE_LIBGEN_H */

/* Define to 1 if you have the <malloc.h> header file. */
/* #undef HAVE_MALLOC_H */

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkstemp' function. */
/* #define HAVE_MKSTEMP 1 */

/* Define to 1 if you have the `snprintf' function. */
/* XXX check this */
#define HAVE_SNPRINTF 1

/* Define to 1 if you have the <stdint.h> header file. */
#if defined(_MSC_VER) || (_MSC_VER >= 1600)
#undef HAVE_STDINT_H
#elif !defined(__MSC__)
#define HAVE_STDINT_H 1
#endif

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
/* #define HAVE_STRINGS_H 1 */

/* Define to 1 if you have the <string.h> header file. */
#undef HAVE_STRING_H

/* Define to 1 if you have the `strlcat' function. */
/* #define HAVE_STRLCAT 1 */

/* Define to 1 if you have the `strlcpy' function. */
/* #define HAVE_STRLCPY 1 */

/* Define to 1 if you have the `strtold' function. */
/* #undef HAVE_STRTOLD */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
/* #undef HAVE_SYS_WAIT_H */

/* Define to 1 if you have the <unistd.h> header file. */
/* #define HAVE_UNISTD_H 1 */

/* Define to 1 if you have the `vfork' function. */
/* #undef HAVE_VFORK */

/* Define to 1 if you have the `vsnprintf' function. */
/* XXX check this */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* host */

/* Define if building universal (internal helper macro) */
#undef AC_APPLE_UNIVERSAL_BUILD

/* Define if host is BIG endian */
#undef HOST_BIG_ENDIAN

/* Define if host is LITTLE endian */
#define HOST_LITTLE_ENDIAN

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  undef WORDS_BIGENDIAN
# endif
#endif

/*** paths for cc driver ***/

/* #define LIBEXECDIR "%PCCDIR%\\libexec\\" */

/* Define path to alternate assembler */
/* #undef ASSEMBLER */
/* #define ASSEMBLER "yasm.exe" */

/* Define path to alternate compiler */
#undef COMPILER

/* Define alternate standard lib directory */
#undef LIBDIR
/* #define LIBDIR "%PCCDIR%\\lib\\" */

/* Define path to alternate linker */
#undef LINKER
/* #define LINKER "link.exe /nologo" */

/* Define target Multi-Arch path */
/* #undef MULTIARCH_PATH */

/* Define path to alternate preprocessor */
#undef PREPROCESSOR

/* Define alternate standard include directory */
#undef STDINC
/* #define STDINC "%PCCDIR%\\include\\" */
/* #define INCLUDEDIR STDINC */

/*** packaging ***/

/* Define to the address where bug reports for this package should be sent. */
/* #define PACKAGE_BUGREPORT "pcc@lists.ludd.ltu.se" */

/* Define to the full name of this package. */
/* #define PACKAGE_NAME "pcc" */

/* Define to the full name and version of this package. */
/* #define PACKAGE_STRING "pcc " PACKAGE_VERSION */

/* Define to the one symbol short name of this package. */
/* #define PACKAGE_TARNAME "pcc" */

/* Define to the home page for this package. */
/* #define PACKAGE_URL "http://pcc.ludd.ltu.se" */

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.2.0.WORK"

/* Major version no */
#define PCC_MAJOR 1

/* Minor version no */
#define PCC_MINOR 2

/* Minor minor version no */
#define PCC_MINORMINOR 0

/* Version string */
#define VERSSTR "pcc " PACKAGE_VERSION " for " TARGET_STR ", http://pcc.ludd.ltu.se"
/* #define VERSSTR "pcc 1.2.0.DEVEL 20150122 for i586-pc-windows" */

/*** miscelaneous ***/

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
/* #undef YYTEXT_POINTER */

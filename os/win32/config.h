/*
 * Config file for visual studio build
 */
#ifndef LIBEXECDIR
#define LIBEXECDIR "%PCCDIR%\\libexec\\"
#endif
#define PREPROCESSOR "cpp.exe"
#define COMPILER "ccom.exe"
#define CXXCOMPILER "cxxcom.exe"

#define USE_YASM
#define PCC_MAJOR 1
#define PCC_MINOR 2
#define PCC_MINORMINOR 0
#define PCCVERSION	"1.2.0"

#ifdef USE_YASM
#define ASSEMBLER "yasm.exe"
#else
#define ASSEMBLER "gas.exe"
#endif

#ifdef USE_MSLINKER
#define LINKER "link.exe /nologo"
#define MSLINKER
#else
#define LINKER "ld.exe"
#endif


#define PECOFFABI

#define STDINC "%PCCDIR%\\include\\"
#define LIBDIR "%PCCDIR%\\lib\\"
#define INCLUDEDIR STDINC
#define PCCLIBDIR "%PCCDIR%\\lib\\i386-win32\\" PCCVERSION "\\lib\\"
#define PCCINCDIR "%PCCDIR%\\lib\\i386-win32\\" PCCVERSION "\\include\\"

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
#endif

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

/* #define HAVE_INTTYPES_H 1 */
#define HAVE_MEMORY_H 1
/* #define HAVE_MKSTEMP 1 */

#if defined(_MSC_VER) || (_MSC_VER >= 1600)
#undef HAVE_STDINT_H
#elif !defined(__MSC__)
#define HAVE_STDINT_H 1
#endif

#define HAVE_STDLIB_H 1
/* #define HAVE_STRINGS_H 1 */
#define HAVE_STRING_H 1
/* #define HAVE_STRLCAT 1 */
/* #define HAVE_STRLCPY 1 */
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SNPRINTF 1
#define HAVE_VSNPRINTF 1
/* #define HAVE_UNISTD_H 1 */
/* #define HOST_BIG_ENDIAN  */
#define HOST_LITTLE_ENDIAN

#define PACKAGE_NAME "pcc"
#define PACKAGE_STRING "pcc " PCCVERSION 
#define PACKAGE_TARNAME "pcc"
#define PACKAGE_VERSION PCCVERSION
#define STDC_HEADERS 1
#define TARGET_LITTLE_ENDIAN 1
#define VERSSTR "pcc " PCCVERSION " for win32, gmcgarry@pcc.ludd.ltu.se"
#define WCHAR_SIZE 2
#define WCHAR_TYPE USHORT
#define YYTEXT_POINTER 1

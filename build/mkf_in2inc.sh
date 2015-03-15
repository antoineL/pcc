#!/bin/sh
# Run against pcc (with possible libs/ subdir) tree 
# for i in */*/Makefile.in ; do sed -f deauto.sed $i > ${i}c ; done
# will create a Makefile.inc file along which each 2nd level Makefile.in,
# ready to included from a build directory.

[ -r DATESTAMP ] || ( echo execute from top directory of pcc && exit 1 )
echo Create Makefile.inc file without any @AUTOVAR@ along each 2nd level Makefile.in
for i in */*/Makefile.in
do
  echo creating ${i}c
  sed >${i}c -e '
/@SET_MAKE@/d

/VPATH *= *@srcdir@/d
/srcdir *= *@srcdir@/d
/top_srcdir *= *@top_srcdir@/d
/builddir *= *@builddir@/d
/top_builddir*= *@top_builddir@/d

/CC = @CC@/d
s/CFLAGS = @CFLAGS@ @ADD_CFLAGS@/CFLAGS += ${ADD_CFLAGS}/
/CFLAGS = @CFLAGS@/d
s/CPPFLAGS += @CPPFLAGS@ /CPPFLAGS += ${LIB_CPPFLAGS} /
s/CPPFLAGS = @CPPFLAGS@ /CPPFLAGS += /
s/@ADD_CPPFLAGS@/${ADD_CPPFLAGS}/
/LIBS = @LIBS@/d
/LDFLAGS = @LDFLAGS@/d
/EXEEXT = @EXEEXT@/d
/AR = @AR@/d
/RANLIB = @RANLIB@/d

/CC_FOR_BUILD = @CC_FOR_BUILD@/d
/LEX = @LEX@/d
/LEX_OUTPUT_ROOT = @LEX_OUTPUT_ROOT@/d
/YACC = @YACC@/d
/YFLAGS = @YFLAGS@/d

s/@PACKAGE_VERSION@/1.2.0.WORK/
s/@version@/1.2.0.WORK/
/TARGOS = @targos@/d
/TARGOSVER = @targosver@/d
/TARGMACH = @targmach@/d
/TARGET = @target@/d
s/@BINPREFIX@/${BINPREFIX}/
s/@ENDIAN_FLAG@/${ENDIAN_FLAG}/

s!prefix = @prefix@!prefix ?= /usr/local!
s!@exec_prefix@!${prefix}!
s!@bindir@!${exec_prefix}/bin!
s!@libdir@!${exec_prefix}/lib!
s!@libexecdir@!${exec_prefix}/libexec!
s!@includedir@!${prefix}/include!
s!@datarootdir@!${prefix}/share!
s!@mandir@!${datarootdir}/man!
s!INSTALL = @INSTALL@!INSTALL ?= ${top_srcdir}/install.sh!
s!@INSTALL_PROGRAM@!${INSTALL} ${STRIP_INSTALL}!
s!@INSTALL_DATA@!${INSTALL} -m 644!

#check no @ remains: /@[A-Za-egz]/p
' $i
done
ls -l */*/Makefile.inc
rem @echo off

set PCCDIR=
set PREFIX=
set usecl=
set dolibpcc=true
set doinstall=false
set dopackage=false

set TARGOSVER=1.2.0

rem Adjust to the versions and paths of yacc/bison and flex you have
rem Do not put them in the current directory (all *.exe are erased!)
rem -l/--no-lines suppress the #line directives, which CL does not handle
rem set YACC=yacc -l
set YACC=bison -y --no-lines
set FLEX=flex
rem No necessary:
rem set LIBFL="C:\Program Files\UnxUtils\usr\local\lib\libfl.lib"
rem Sometimes necessary with Flex or Berkeley Yacc
set TMPDIR=%TEMP%
rem If you want to build a package (installer), install Inno Setup
set ISCC="C:\Program Files\Inno Setup 5\ISCC.exe"

:parsecommandline
if '%1' == '/h' goto dispinfo
if '%1' == '/pcc' goto usepcc
if '%1' == '/cl' goto usecl
if '%1' == '/prefix' goto prefix
if '%1' == '/pccdir' goto pccdir
if '%1' == '/pccsrcdir' goto pccsrcdir
if '%1' == '/pcclibssrcdir' goto pcclibssrcdir
if '%1' == '/nolibpcc' set dolibpcc=false
if '%1' == '/install' set doinstall=true
if '%1' == '/package' set dopackage=true
if '%dopackage%' == 'true' set doinstall=true
goto build

:dispinfo
echo build.bat [/h] { /pcc or /cl } [/prefix -dir-] [/pccdir -dir-] [/pccsrcdir -dir-] [/pcclibssrcdir -dir-] [ /nolibpcc or /install or /package ]
goto end

:prefix
rem PREFIX is where the files are "installed" (usual configure use)
shift
set PREFIX=%1
shift
goto parsecommandline

:pccdir
rem %%PCCDIR%%\bin is where a working PCC.EXE (and AR.EXE) is found, if using /pcc not /cl
shift
set PCCDIR=%1
shift
goto parsecommandline

:pccsrcdir
shift
set PCCSRCDIR=%1
shift
goto parsecommandline

:pcclibssrcdir
shift
set PCCLIBSSRCDIR=%1
shift
goto parsecommandline

:usecl
set CC=cl.exe
set CC_OUT=/Fe
set CFLAGS=/nologo /Zi /MT /W2
set CFLAGS2=/nologo /Zi /MD /Za /Wall /GS- /D__MSC__ /wd4001 /wd4146
set OBJ=obj
set AR=lib.exe /nologo
set AR_OUT=/OUT:libpcc.a
set MASM=ml /nologo
set usecl=true
shift
goto parsecommandline

:usepcc
set CC=pcc.exe
set CC_OUT=-o
set CFLAGS=-g
set CFLAGS2=-fno-stack-protector-all
set OBJ=o
set AR=ar.exe
set AR_OUT=r libpcc.a
set usecl=false
shift
goto parsecommandline

:build

if '%usecl%' == '' goto dispinfo

set PREFIX=###%PREFIX%###
set PREFIX=%PREFIX:"###=%
set PREFIX=%PREFIX:###"=%
set PREFIX=%PREFIX:###=%

set PCCDIR=###%PCCDIR%###
set PCCDIR=%PCCDIR:"###=%
set PCCDIR=%PCCDIR:###"=%
set PCCDIR=%PCCDIR:###=%

set PCCSRCDIR=###%PCCSRCDIR%###
set PCCSRCDIR=%PCCSRCDIR:"###=%
set PCCSRCDIR=%PCCSRCDIR:###"=%
set PCCSRCDIR=%PCCSRCDIR:###=%

set PCCLIBSSRCDIR=###%PCCLIBSSRCDIR%###
set PCCLIBSSRCDIR=%PCCLIBSSRCDIR:"###=%
set PCCLIBSSRCDIR=%PCCLIBSSRCDIR:###"=%
set PCCLIBSSRCDIR=%PCCLIBSSRCDIR:###=%

if not '%PCCDIR%' == '' goto pccdirset
set PCCDIR=C:\Program Files\pcc
:pccdirset

if not '%PCCSRCDIR%' == '' goto pccsrcdirset
set PCCSRCDIR=..\..
:pccsrcdirset

if not '%PCCLIBSSRCDIR%' == '' goto pcclibssrcdirset
set PCCLIBSSRCDIR=..\..\..\pcc-libs
:pcclibssrcdirset

if '%usecl%' == 'true' goto ccprefixed
set CC="%PCCDIR%\bin\%CC%"
set AR="%PCCDIR%\bin\%AR%"
:ccprefixed

set TARGOS=win32
set MACH=i386
rem set DEF_LIBEXECDIR=-DLIBEXECDIR=\"\"
rem The default value is "%%PCCDIR%%\libexec\" (set in config.h)

set MIPDIR=%PCCSRCDIR%\mip
set CPPDIR=%PCCSRCDIR%\cc\cpp
set CCOMDIR=%PCCSRCDIR%\cc\ccom
set CXXCOMDIR=%PCCSRCDIR%\cc\cxxcom
set CCDIR=%PCCSRCDIR%\cc\cc
set DRIVERDIR=%PCCSRCDIR%\cc\driver
set OSDIR=%PCCSRCDIR%\os\%TARGOS%
set MACHDIR=%PCCSRCDIR%\arch\%MACH%

set CPPFLAGS=-DWIN32 -DGCC_COMPAT -DPCC_DEBUG -Dos_%TARGOS% -Dmach_%MACH%

del *.obj *.o *.exe

%CC% %CC_OUT%pcc.exe %CPPFLAGS% %DEF_LIBEXECDIR% %CFLAGS% -I%CCDIR% -I%DRIVERDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %CCDIR%\cc.c %DRIVERDIR%\strlist.c %DRIVERDIR%\xalloc.c %MIPDIR%\compat.c


rem obsolete in 1.2.0
rem %YACC% -t -d %CPPDIR%\cpy.y
rem move>NUL y.tab.c cpy.c
rem move>NUL y.tab.h cpy.h
rem obsolete in 1.0
rem flex %CPPDIR%\scanner.l
rem %CC% %CC_OUT%cpp.exe %CPPFLAGS% %CFLAGS% -I%CPPDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% -I. %CPPDIR%\cpp.c %MIPDIR%\compat.c cpy.c lex.yy.c
rem %CC% %CC_OUT%cpp.exe %CPPFLAGS% %CFLAGS% -I%CPPDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% -I. %CPPDIR%\cpp.c %CPPDIR%\token.c %MIPDIR%\compat.c cpy.c
%CC% %CC_OUT%cpp.exe %CPPFLAGS% %CFLAGS% -I%CPPDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% -I. %CPPDIR%\cpp.c %CPPDIR%\token.c %MIPDIR%\compat.c %CPPDIR%\cpc.c

%CC% %CC_OUT%mkext.exe -DMKEXT %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %MIPDIR%\mkext.c %MACHDIR%\table.c %MIPDIR%\common.c
mkext
%YACC% -t -d %CCOMDIR%\cgram.y
move>NUL y.tab.c cgram.c
move>NUL y.tab.h cgram.h
%FLEX% %CCOMDIR%\scan.l
move>NUL lex.yy.c scan.c

%CC% %CC_OUT%ccom.exe %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% -I. %CCOMDIR%\main.c %MIPDIR%\compat.c scan.c cgram.c external.c %CCOMDIR%\optim.c %CCOMDIR%\builtins.c %CCOMDIR%\pftn.c %CCOMDIR%\trees.c %CCOMDIR%\inline.c %CCOMDIR%\symtabs.c %CCOMDIR%\init.c %MACHDIR%\local.c %MACHDIR%\code.c %CCOMDIR%\stabs.c %CCOMDIR%\gcc_compat.c %MIPDIR%\match.c %MIPDIR%\reader.c %MIPDIR%\optim2.c %MIPDIR%\regs.c %MIPDIR%\unicode.c %MACHDIR%\local2.c %MACHDIR%\order.c %MACHDIR%\table.c %MIPDIR%\common.c

%YACC% -t -d %CXXCOMDIR%\cgram.y
move>NUL y.tab.c cxxgram.c
move>NUL y.tab.h cgram.h
%FLEX% %CXXCOMDIR%\scan.l
move>NUL lex.yy.c scanxx.c

%CC% %CC_OUT%cxxcom.exe %CPPFLAGS% %CFLAGS% -I%CXXCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% -I. %CXXCOMDIR%\main.c %MIPDIR%\compat.c scanxx.c cxxgram.c external.c %CXXCOMDIR%\cxxcode.c %CXXCOMDIR%\optim.c %CXXCOMDIR%\builtins.c %CXXCOMDIR%\pftn.c %CXXCOMDIR%\trees.c %CXXCOMDIR%\inline.c %CXXCOMDIR%\symtabs.c %CXXCOMDIR%\init.c %CXXCOMDIR%\cxxcode.c %MACHDIR%\local.c %MACHDIR%\code.c %CXXCOMDIR%\stabs.c %CXXCOMDIR%\gcc_compat.c %MIPDIR%\match.c %MIPDIR%\reader.c %MIPDIR%\optim2.c %MIPDIR%\regs.c %MIPDIR%\unicode.c %MACHDIR%\local2.c %MACHDIR%\order.c %MACHDIR%\table.c %MIPDIR%\common.c

if not '%dolibpcc%' == 'true' goto end

if not '%PREFIX%' == '' goto prefixset
set PREFIX=C:\Program Files\pcc
:prefixset

set PCCDESTDIR=%PREFIX%
set LIBPCCDESTDIR=%PREFIX%\lib\i386-win32\%TARGOSVER%

set LIBPCCDIR=%PCCLIBSSRCDIR%\libpcc
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\adddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\anddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ashldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ashrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\cmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\divdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixsfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixunsdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixunssfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatdisf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatunsdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\iordi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\lshldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\lshrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\moddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\muldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\negdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\notdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\qdivrem.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ssp.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\subdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ucmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\udivdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\umoddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\unwind.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\xordi3.c

if '%usecl%' == 'false' %CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\_alloca.c
if '%usecl%' == 'false' set alloca=_alloca.o
rem no equivalent for MASM syntax yet
if '%usecl%' == 'true' set alloca=
if '%usecl%' == 'false' %CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\cxmuldiv.c
if '%usecl%' == 'false' set cxmuldiv=cxmuldiv.o
rem require PCC to compile
if '%usecl%' == 'true' set cxmuldiv=
if '%usecl%' == 'false' %CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\_ftol.c
if '%usecl%' == 'true' %MASM% -c %LIBPCCDIR%\_ftol.asm

%AR% %AR_OUT% %alloc% _ftol.%OBJ% adddi3.%OBJ% anddi3.%OBJ% ashldi3.%OBJ% ashrdi3.%OBJ% cmpdi2.%OBJ% %cxmuldiv% divdi3.%OBJ% fixdfdi.%OBJ% fixsfdi.%OBJ% fixunsdfdi.%OBJ% fixunssfdi.%OBJ% floatdidf.%OBJ% floatdisf.%OBJ% floatunsdidf.%OBJ% iordi3.%OBJ% lshldi3.%OBJ% lshrdi3.%OBJ% moddi3.%OBJ% muldi3.%OBJ% negdi2.%OBJ% notdi2.%OBJ% qdivrem.%OBJ% ssp.%OBJ% subdi3.%OBJ% ucmpdi2.%OBJ% udivdi3.%OBJ% umoddi3.%OBJ% unwind.%OBJ% xordi3.%OBJ%

if not '%doinstall%' == 'true' goto end

md "%PCCDESTDIR%"
md "%PCCDESTDIR%\bin"
md "%PCCDESTDIR%\libexec"
md "%PCCDESTDIR%\man"
md "%PCCDESTDIR%\man\man1"
md "%LIBPCCDESTDIR%"
md "%LIBPCCDESTDIR%\lib"
md "%LIBPCCDESTDIR%\include"

copy pcc.exe "%PCCDESTDIR%\bin"
copy cpp.exe "%PCCDESTDIR%\libexec"
copy ccom.exe "%PCCDESTDIR%\libexec"
copy cxxcom.exe "%PCCDESTDIR%\libexec"

copy libpcc.a "%LIBPCCDESTDIR%\lib"
copy "%LIBPCCDIR%\include\*.h" "%LIBPCCDESTDIR%\include"

copy "%CCDIR%\cc.1" "%PCCDESTDIR%\man\man1"
copy "%CPPDIR%\cpp.1" "%PCCDESTDIR%\man\man1"
copy "%CCOMDIR%\ccom.1" "%PCCDESTDIR%\man\man1"

if not '%dopackage%' == 'true' goto end

%ISCC% /DAppName="PCC" /DAppVersion="%TARGOSVER%" /DAppNameLower="pcc" /DPrefix="%PREFIX%" %OSDIR%/pcc.iss
rem Resulting package is in %OSDIR%

:end

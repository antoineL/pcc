CC ?= gcc
#CFLAGS =
#CPPFLAGS =
#LDFLAGS =
#LIBS =
#AR =

ADD_CFLAGS = 
#ADD_CFLAGS += -Werror	# --enable-Werror
ADD_CFLAGS += -g -O2 -Wall -Wmissing-prototypes -Wshadow -Wsign-compare
# not supported: -Wstrict-prototypes -Wtruncate
ADD_CFLAGS += -W -Wno-unused-parameter # -Wextra

ADD_CPPFLAGS = 
CPPFLAGS = 
CPPFLAGS += -DGCC_COMPAT #--disable-gcc-compat
CPPFLAGS += -DPCC_DEBUG  #--disable-pcc-debug
STRIP_INSTALL = -s	 #--disable-stripping

EXEEXT = .exe
prefix = %PCCDIR%

HOST_CC = gcc
CC_FOR_BUILD ?= ${HOST_CC}
#CC_FOR_BUILD = gcc
LEX = ../../bin/flex
LEX_OUTPUT_ROOT=lex.yy
YACC = ../../bin/byacc
#YACC = bison -y
RANLIB ?= echo No ranlib for
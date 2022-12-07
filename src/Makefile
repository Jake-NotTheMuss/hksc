# makefile for building Lua
# see INSTALL for installation instructions
# see ../Makefile and luaconf.h for further customization

# == CHANGE THE SETTINGS BELOW TO SUIT YOUR ENVIRONMENT =======================

# CWARNS= -pedantic -Waggregate-return -Wcast-align \
#         -Wmissing-prototypes -Wpointer-arith -Wshadow \
#         -Wsign-compare -Wstrict-prototypes -Wundef -Wwrite-strings
CWARNS= -ansi -pedantic -Wall -Wextra
# -Wcast-qual

# -DEXTERNMEMCHECK -DHARDSTACKTESTS
# -g -DLUA_USER_H='"ltests.h"'
# -fomit-frame-pointer #-pg -malign-double
TESTS= -g -DLUA_USER_H='"ltests.h"'

LOCAL = $(TESTS) $(CWARNS)


CC= cc
CFLAGS= -Wall $(MYCFLAGS)
AR= ar rcu
RANLIB= ranlib
RM= rm -f

MYCFLAGS= $(LOCAL)
MYLDFLAGS=
MYLIBS=


# enable Linux goodies
# MYCFLAGS= $(LOCAL) -DLUA_USE_LINUX
# MYLDFLAGS= -Wl,-E
# MYLIBS=



# == END OF USER SETTINGS. NO NEED TO CHANGE ANYTHING BELOW THIS LINE =========


LIBS = -lm

CORE_T=	libhksc.a
CORE_O=	lcode.o lctype.o ldebug.o ldecomp.o ldo.o ldump.o lfunc.o lgc.o llex.o \
	lmem.o lobject.o lopcodes.o lparser.o lstate.o lstring.o ltable.o  \
	lundump.o lzio.o ltests.o
AUX_O=	hksclib.o

LUAC_T=	hksc
LUAC_O=	hksc.o hkscaux.o print.o

ALL_T= $(CORE_T) $(LUAC_T)
ALL_O= $(CORE_O) $(LUAC_O) $(AUX_O)
ALL_A= $(CORE_T)

default: all

all:	$(ALL_T)

o:	$(ALL_O)

a:	$(ALL_A)

$(CORE_T): $(CORE_O) $(AUX_O)
	$(AR) $@ $?
	$(RANLIB) $@

$(LUAC_T): $(LUAC_O) $(CORE_T)
	$(CC) -o $@ $(MYLDFLAGS) $(LUAC_O) $(CORE_T) $(LIBS) $(MYLIBS)

clean:
	$(RM) $(ALL_T) $(ALL_O)

depend:
	@$(CC) $(CFLAGS) -MM *.c

echo:
	@echo "CC = $(CC)"
	@echo "CFLAGS = $(CFLAGS)"
	@echo "AR = $(AR)"
	@echo "RANLIB = $(RANLIB)"
	@echo "RM = $(RM)"
	@echo "MYCFLAGS = $(MYCFLAGS)"
	@echo "MYLDFLAGS = $(MYLDFLAGS)"
	@echo "MYLIBS = $(MYLIBS)"
	@echo "DL = $(DL)"

# list targets that do not create files (but not all makes understand .PHONY)
.PHONY: all default o a clean depend echo

# DO NOT DELETE

hksc.o: hksc.c lua.h luaconf.h hkscconf.h ltests.h hksclib.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def lctype.h \
  hkscaux.h
hkscaux.o: hkscaux.c lua.h luaconf.h hkscconf.h ltests.h hksclib.h \
  lstate.h lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def \
  lctype.h ldo.h lstring.h lgc.h
hksclib.o: hksclib.c luaconf.h hkscconf.h lua.h ltests.h hksclib.h \
  lstate.h lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def \
  lctype.h ldo.h lgc.h lparser.h ltable.h lstring.h
lcode.o: lcode.c lua.h luaconf.h hkscconf.h ltests.h lcode.h llex.h \
  lobject.h llimits.h lzio.h lmem.h ltoken.def lopcodes.h lopcodes.def \
  lparser.h ltable.h ldebug.h lstate.h lundump.h ltype.def ldo.h lgc.h
lctype.o: lctype.c lctype.h lua.h luaconf.h hkscconf.h ltests.h llimits.h
ldebug.o: ldebug.c lua.h luaconf.h hkscconf.h ltests.h lcode.h llex.h \
  lobject.h llimits.h lzio.h lmem.h ltoken.def lopcodes.h lopcodes.def \
  lparser.h ltable.h ldebug.h lstate.h lundump.h ltype.def ldo.h lfunc.h \
  lstring.h lgc.h
ldecomp.o: ldecomp.c lua.h luaconf.h hkscconf.h ltests.h lobject.h \
  llimits.h lstate.h lundump.h lzio.h lmem.h ltype.def
ldo.o: ldo.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h lfunc.h \
  lopcodes.h lopcodes.def lparser.h ltable.h lstring.h lgc.h
ldump.o: ldump.c lua.h luaconf.h hkscconf.h ltests.h lobject.h llimits.h \
  lstate.h lundump.h lzio.h lmem.h ltype.def
lfunc.o: lfunc.c lua.h luaconf.h hkscconf.h ltests.h lfunc.h lobject.h \
  llimits.h lgc.h lmem.h lstate.h lundump.h lzio.h ltype.def lstring.h
lgc.o: lgc.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h lfunc.h \
  lgc.h lstring.h ltable.h
llex.o: llex.c lua.h luaconf.h hkscconf.h ltests.h lctype.h llimits.h \
  ldebug.h lstate.h lobject.h lundump.h lzio.h lmem.h ltype.def ldo.h \
  llex.h ltoken.def lparser.h ltable.h lstring.h lgc.h
lmem.o: lmem.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h
lobject.o: lobject.c lua.h luaconf.h hkscconf.h ltests.h lctype.h \
  llimits.h ldo.h lobject.h lstate.h lundump.h lzio.h lmem.h ltype.def \
  lstring.h lgc.h
lopcodes.o: lopcodes.c lopcodes.h llimits.h lua.h luaconf.h hkscconf.h \
  ltests.h lopcodes.def
lparser.o: lparser.c lua.h luaconf.h hkscconf.h ltests.h lcode.h llex.h \
  lobject.h llimits.h lzio.h lmem.h ltoken.def lopcodes.h lopcodes.def \
  lparser.h ltable.h ldebug.h lstate.h lundump.h ltype.def ldo.h lfunc.h \
  lstring.h lgc.h
lstate.o: lstate.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h lfunc.h \
  lgc.h llex.h ltoken.def lstring.h ltable.h
lstring.o: lstring.c lua.h luaconf.h hkscconf.h ltests.h lmem.h llimits.h \
  lobject.h lstate.h lundump.h lzio.h ltype.def lstring.h lgc.h
ltable.o: ltable.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h lstate.h \
  lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h lgc.h \
  ltable.h
ltests.o: ltests.c lua.h luaconf.h hkscconf.h ltests.h lcode.h llex.h \
  lobject.h llimits.h lzio.h lmem.h ltoken.def lopcodes.h lopcodes.def \
  lparser.h ltable.h lctype.h ldebug.h lstate.h lundump.h ltype.def \
  ldo.h lfunc.h lgc.h lstring.h
lundump.o: lundump.c lua.h luaconf.h hkscconf.h ltests.h ldebug.h \
  lstate.h lobject.h llimits.h lundump.h lzio.h lmem.h ltype.def ldo.h \
  lfunc.h lstring.h lgc.h
lzio.o: lzio.c lua.h luaconf.h hkscconf.h ltests.h llimits.h lmem.h \
  lstate.h lobject.h lundump.h lzio.h ltype.def
print.o: print.c lctype.h lua.h luaconf.h hkscconf.h ltests.h llimits.h \
  ldebug.h lstate.h lobject.h lundump.h lzio.h lmem.h ltype.def \
  lopcodes.h lopcodes.def

# (end of Makefile)

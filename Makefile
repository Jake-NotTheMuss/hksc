# makefile for building and testing Lua
# see src/Makefile and src/luaconf.h for further customization

# == CHANGE THE SETTINGS BELOW TO SUIT YOUR ENVIRONMENT =======================

# Your platform. See PLATS for possible values.
PLAT= none

# Where to install. The installation starts in the src directory, so take care
# if INSTALL_TOP is not an absolute path. (Man pages are installed from the
# doc directory.)
#
INSTALL_TOP= $(prefix)
INSTALL_BIN= $(INSTALL_TOP)/bin
INSTALL_INC= $(INSTALL_TOP)/include
INSTALL_LIB= $(INSTALL_TOP)/lib
INSTALL_MAN= $(INSTALL_TOP)/man/man1

# How to install. You may prefer "install" instead of "cp" if you have it.
# To remove debug information from binaries, use "install -s" in INSTALL_EXEC.
#
INSTALL_EXEC= $(CP)
INSTALL_DATA= $(CP)
#INSTALL_EXEC= $(INSTALL) -m 0755
#INSTALL_DATA= $(INSTALL) -m 0644

# == END OF USER SETTINGS. NO NEED TO CHANGE ANYTHING BELOW THIS LINE =========

CONFIG_H=src/hkscconf.h

TOP=.

include config.mak

# What to install.
TO_BIN= src/$(HKSC_T)
TO_INC= $(SRCDIR)/hksclua.h $(SRCDIR)/hkscluaconf.h $(SRCDIR)/hksclib.h  \
	$(SRCDIR)/hkscconf.h $(ETCDIR)/hksclua.hpp
TO_LIB= src/$(STATICLIB) src/$(SHAREDLIB)
# TO_MAN= lua.1 luac.1

# set prefix for the child Makefile to use with `rpath'
all: src

test: src
	@cd test && \
	$(MAKE) test HKSC_NAME=$(HKSC_T) && \
	$(MAKE) test HKSC_NAME=$(HKSC_SH)

src: $(CONFIG_H)
	@cd src && $(MAKE)

clean:
	@cd src && $(MAKE) $@
	@cd test && $(MAKE) $@

depend:
	-@cd $(SRCDIR) && { \
	TMP=tmp_$$; \
	$(CC) $(CFLAGS) -MM *.c > $$TMP; \
	$(CC) $(CFLAGS) -MM hksclib.c l*.c | \
	sed 's/^\([^. \t]*\)\.o:/\1.lo:/' >> $$TMP; \
	sed -i.bk -e "/^# DO NOT DELETE$$/,/^# (end of Makefile)$$/{ \
		/^# DO NOT DELETE$$/{G;p;r $$TMP" -e " \
	}; /^# (end of Makefile)$$/{H;g;p;}; d;" Makefile; \
	$(RM) $$TMP Makefile.bk >/dev/null 2>&1 }

distclean: clean
	$(RM) config.mak test/config.mak $(CONFIG_H) libhksc.pc

install:
	$(MKDIR) -p $(INSTALL_BIN) $(INSTALL_INC) $(INSTALL_LIB) $(INSTALL_MAN)
	$(INSTALL_EXEC) $(TO_BIN) $(INSTALL_BIN)
	$(INSTALL_DATA) $(TO_INC) $(INSTALL_INC)
	$(INSTALL_DATA) $(TO_LIB) $(INSTALL_LIB)
	$(INSTALL_DATA) $(TO_MAN) $(INSTALL_MAN)
#	$(RANLIB) $(INSTALL_LIB)/$(TO_LIB)

local:
	$(MAKE) install INSTALL_TOP=. INSTALL_EXEC="cp -p" INSTALL_DATA="cp -p"

# echo config parameters
echo:
	@echo ""
	@echo "These are the parameters currently set in src/Makefile to build Lua:"
	@echo ""
	@cd $(SRCDIR) && $(MAKE) -s echo
	@echo ""
	@echo "These are the parameters currently set in Makefile to install Lua:"
	@echo ""
	@echo "prefix = $(prefix)"
	@echo "exec_prefix = $(exec_prefix)"
	@echo "bindir = $(bindir)"
	@echo "libdir = $(libdir)"
	@echo "sharedlibdir = $(sharedlibdir)"
	@echo "includedir = $(includedir)"
	@echo "sharedir = $(sharedir)"
	@echo "docdir = $(docdir)"
	@echo "mandir = $(mandir)"
	@echo "pkgconfigdir = $(pkgconfigdir)"
	@echo "SRCDIR = $(SRCDIR)"
	@echo "SHAREDLIB = $(SHAREDLIB)"
	@echo "HKSC_VERSION = $(HKSC_VERSION)"
	@echo "PLAT = $(PLAT)"
	@echo "INSTALL = $(INSTALL)"
	@echo "FIND = $(FIND)"
	@echo "CP = $(CP)"
	@echo "MKDIR = $(MKDIR)"
	@echo "INSTALL_TOP = $(INSTALL_TOP)"
	@echo "INSTALL_BIN = $(INSTALL_BIN)"
	@echo "INSTALL_INC = $(INSTALL_INC)"
	@echo "INSTALL_LIB = $(INSTALL_LIB)"
	@echo "INSTALL_MAN = $(INSTALL_MAN)"
	@echo "INSTALL_LMOD = $(INSTALL_LMOD)"
	@echo "INSTALL_CMOD = $(INSTALL_CMOD)"
	@echo "INSTALL_EXEC = $(INSTALL_EXEC)"
	@echo "INSTALL_DATA = $(INSTALL_DATA)"
	@echo ""
	@echo "See also src/hkscluaconf.h ."
	@echo ""

# echo private config parameters
pecho:
	@echo "TO_BIN = $(TO_BIN)"
	@echo "TO_INC = $(TO_INC)"
	@echo "TO_LIB = $(TO_LIB)"
	@echo "TO_MAN = $(TO_MAN)"

# show what has changed since we unpacked
newer:
	@$(FIND) . -newer MANIFEST -type f

$(CONFIG_H): $(HKSCDIR)$(CONFIG_H).in
libhksc.pc: $(HKSCDIR)libhksc.pc.in

$(CONFIG_H) libhksc.pc:
	@echo "$@ is out-of-date. Please rerun ./configure"

config.mak:
	@test -e $@ || echo "Please run ./configure." && exit 1

# list targets that do not create files (but not all makes understand .PHONY)
.PHONY: all src test clean distclean install local echo pecho lecho newer

# (end of Makefile)

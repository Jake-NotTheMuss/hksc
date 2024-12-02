prefix=@prefix@
exec_prefix=@exec_prefix@
bindir=@bindir@
libdir=@libdir@
sharedlibdir=@sharedlibdir@
includedir=@includedir@
sharedir=@sharedir@
docdir=@docdir@
mandir=@mandir@
pkgconfigdir=@pkgconfigdir@
CP=@CP@
RM=@RM@
FIND=@FIND@
INSTALL=@INSTALL@
MKDIR=@MKDIR@
EXESUF=@EXESUF@

STATICLIB=@STATICLIB@@LIBSUF@
SHAREDLIB=@SHAREDLIB@@DLLSUF@
HKSC_T=@HKSC_T@
HKSC_SH=@HKSC_SH@
HKSC_INSTALL_NAME=@HKSC_INSTALL_NAME@

srcdir=@srcdir@
top_srcdir=@top_srcdir@
top_builddir=@top_builddir@

EXESUF=@EXESUF@

HKSC_TARGET_PLAT=@HKSC_TARGET_PLAT@
HKSC_PREFIX_MAP=@HKSC_PREFIX_MAP@
HKSC_GEN_PROFILE=@HKSC_GEN_PROFILE@
HKSC_GEN_DEBUG=@HKSC_GEN_DEBUG@

RM=@RM@

HKSC_VERSION=@HKSC_VERSION@

@VPATH_SET@

# should be set by the parent Makefile in the invokation
HKSC_NAME=hksc

HKSC=$(top_builddir)/src/$(HKSC_NAME)$(EXESUF)

TESTFLAGS=--testing --test-srcdir='$(srcdir)' @DECOMPILER_IGNORE_DEBUG@
HKSC_COMMON_FLAGS=$(HKSC_TARGET_PLAT)
# flags for loading source files
HKSCFLAGS=-L $(HKSC_COMMON_FLAGS)

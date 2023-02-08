# makefile for testing Lua

TOP=..

include ../config.mak
include config.mak

# should be set by the parent Makefile in the invokation
HKSC_NAME=

HKSC=$(TOPSRC)/$(HKSC_NAME)

HKSCFLAGS=-L $(HKSC_SUPPRESS_LOGGING)

# TESTS=version
TESTS= $(COMPILER_TESTS) $(DECOMPILER_TESTS)

TESTMAKEFLAGS=

# no need to list 2 games that have the same compatibility settings, i.e., if
# game X and game Y both have the same compatilbility settings, only use game X
# for tests; the one exception to this is Call of Duty, which has all features
# disabled for T6 and T7, but T7 generates different code than T6 because it has
# additional opcodes for bitwise operations
ALL_GAMEDIRS=codt6 codt7

# will be set to .dtest by configure if the decompiler is enabled
# *.dtest will depend on *.ctest, so compiler tests will always be run, and then
# if the decompiler is enabled, the decompiler tests will be run
TESTSUFFIX=.ctest

test: clean all

all: dummy version error $(ALL_TESTS)

# for when $(TESTS) is empty
# also checks that HKSC was set properly
dummy:
	@$(HKSC) --version >/dev/null 2>&1 || \
	echo You need to set HKSC_NAME in the parent Makefile && false

# check version of $(HKSC) against expected version
version:
	@echo checking \`$(HKSC) --version\` against $(HKSC_VERSION); \
	HKSC_VERSION_REGEX=`echo $(HKSC_VERSION) | sed 's/\./\\./g'`; \
	$(HKSC) --version | grep -iq ".*hksc[^0-9]*$$HKSC_VERSION_REGEX"

error:
	@cd $@ && $(MAKE) $(TESTMAKEFLAGS) HKSC_NAME=$(HKSC_NAME)

.SUFFIXES:
.SUFFIXES: .lua .cexpect .profileexpect .debugexpect .ctest .dtest

# .lua.luac:
# 	$(HKSC) --compile -s $< -o $@

.lua.cexpect:
	@echo You need to regenerate $@!; false
.lua.profileexpect:
	@echo You need to regenerate $@!; false
.lua.debugexpect:
	@echo You need to regenerate $@!; false

.profileexpect.ctest:
	@:
.debugexpect.ctest:
	@:

# compiler test
.cexpect.ctest:
	@echo ------------ $* ------------
# 	generate stripped bytecode
	$(HKSC) $(HKSCFLAGS) -s $*.lua
	$(HKSC) $(HKSCFLAGS) $(GEN_PROFILE) $*.luaprofile $*.lua
	$(HKSC) $(HKSCFLAGS) $(GEN_DEBUG) $*.luadebug $*.lua
	@ if $(CMP_CODE) $*.cexpect $*.luac; then echo "$*.luac OK"; fi
	@ if $(CMP_DEBUG) $*.debugexpect $*.luadebug; then echo "$*.luadebug OK"; fi
	@ if $(CMP_PROFILE) -u $*.profileexpect $*.luaprofile; then \
	echo "$*.luacallstackdb OK"; fi


# decompiler test
.ctest.dtest:
	@: echo hello $@

clean:
	$(RM) *.luac *.luadebug *.luacallstackdb *.ctest *.dtest
	@cd error && $(MAKE) $@

.PHONY: test all dummy clean version error

05_bigtable.lua: gen-bigtable.sh
	$(SHELL) $< $@

# (end of Makefile)

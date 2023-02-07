# makefile for checking hksc error messages

TOP=../..
include $(TOP)/config.mak

HKSC=$(SRCDIR)/$(HKSC_NAME)

HKSCFLAGS=-p --compile $(HKSC_SUPPRESS_LOGGING)

# files are generated for each BOM that is NOT accepted by hksc
BOM_TESTS=utf16be_bom.test utf16le_bom.test utf32be_bom.test utf32le_bom.test  \
utf16or32le_bom.test unknown_bom.test
OTHER_TESTS=badbreak.test earlybreak.test earlyreturn.test  \
literaloverflow.test lud.test ui64.test #utf8_identifier.test
# error checks for when structures are enabled/disabled
NOHSTRUCTURE_TESTS=hmake.test hstructure.test typedparam.test typedvar.test
YESHSTRUCTURE_TESTS=

ALL_ERROR_TESTS= $(OTHER_TESTS) $(BOM_TESTS)

all: prepare $(ALL_ERROR_TESTS) $(HSTRUCTURE_ERROR_TESTS)
	@printf 'All Lua error checks passed\n\n'

prepare: clean
	@printf '\nStarting Lua error checks...\n'

nohstructure: $(NOHSTRUCTURE_TESTS)

yeshstructure: $(YESHSTRUCTURE_TESTS)

.SUFFIXES: .test .expect .lua

.lua.expect:
	@echo You need to regenerate $@!; false

.expect.test:
	@echo ------------ $* ------------
	@EXTRAFLAGS= ; test "x$*" = "xliteraloverflow" && EXTRAFLAGS=-L; \
	$(HKSC) $(HKSCFLAGS) $$EXTRAFLAGS $*.lua >/dev/null 2>$@
	@if diff -Nbu $^ $@ ; then echo "$* test OK"; fi

$(BOM_TESTS:.test=.lua): gen-bom.sh
	@$(SHELL) $^

clean:
	-@$(RM) *.test >/dev/null 2>&1

.PHONY: all prepare clean nohstructure yeshstructure

# (end of Makefile)

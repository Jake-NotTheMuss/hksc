# makefile for checking hksc error messages

TOP=../..
include $(TOP)/config.mak

BOM_TESTS= utf16be_bom utf16le_bom utf32be_bom utf32le_bom
# OTHER_TESTS=badbreak earlybreak earlyreturn literaloverflow lud ui64 utf8_identifier
OTHER_TESTS= literaloverflow lud ui64
# error checks for when structures are disabled
NOHSTRUCTURE_TESTS=hmake hstructure typedparam typedvar

ALL_ERROR_TESTS= $(OTHER_TESTS) $(BOM_TESTS)

all: $(ALL_ERROR_TESTS)
	@$(RM) *.output

# use -s to avoid generating extra debug files in case the command succeeds
HKSCFLAGS=--compile -s $(HKSC_SUPPRESS_LOGGING)

EXPECTFILE=

# the error should be that a UI64 overflowed which made its low 4 bits nonzero
# literals need to be enabled for this test
literaloverflow: HKSCFLAGS+=-L
$(OTHER_TESTS): EXPECTFILE=$@.expect
# the error is the same for all BOMs
$(BOM_TESTS): EXPECTFILE=utfxx_bom.expect

$(ALL_ERROR_TESTS):
	@HKSC_BASENAME=`basename $(HKSC) | sed 's/\./\\./g'`; \
	$(HKSC) $(HKSCFLAGS) $@.lua -o hksc.out 2>&1 1>/dev/null | \
	sed "s/^.*$$HKSC_BASENAME:[ \t]*//" > $@.output
	@$(RM) hksc.out >/dev/null 2>&1
	@if diff -Nbu $(EXPECTFILE) $@.output ; then echo "$@ test OK"; fi

# (end of Makefile)

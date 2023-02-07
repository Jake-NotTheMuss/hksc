#!/bin/sh

gen()
{
	luafile=$1_bom.lua
	expectfile=$1_bom.expect
	bomtype=$2
	shift 2
	bytes=
	for b; do
		bytes="$bytes\\x$b"
	done
	printf '%b' $bytes > $luafile
	printf "$luafile:1: Invalid or unsupported file encoding. Only ASCII and \
UTF-8 are supported near '<%s BOM>'\n" $bomtype > $expectfile
}

# gen utf8         ef bb bf
gen utf16be      UTF16BE  fe ff
gen utf16le      UTF16LE  ff fe
gen utf32be      UTF32BE  00 00 fe ff
gen utf32le      UTF16LE  ff fe 00 00
gen utf16or32le  UTF16LE  ff fe 00
gen unknown      Invalid  fe 00 ff ef

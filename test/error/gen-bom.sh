#!/bin/sh

gen()
{
	file=$1_bom.lua
	shift
	bytes=
	for b; do
		bytes="$bytes\\x$b"
	done
	printf '%b' $bytes > $file
}

# gen utf8         ef bb bf
gen utf16be      fe ff
gen utf16le      ff fe
gen utf32be      00 00 fe ff
gen utf32le      ff fe 00 00
gen utf16or32le  ff fe 00

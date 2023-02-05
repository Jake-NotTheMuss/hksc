#!/bin/sh

printf '%b' '\xef\xbb\xbf' > utf8_bom.lua
printf '%b' '\xfe\xff' > utf16be_bom.lua
printf '%b' '\xff\xfe' > utf16le_bom.lua
printf '%b' '\x00\x00\xfe\xff' > utf32be_bom.lua
printf '%b' '\xff\xfe\x00\x00' > utf32le_bom.lua

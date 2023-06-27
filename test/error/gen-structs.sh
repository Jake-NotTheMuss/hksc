#!/bin/sh

# generate 1024 structures to reach the VM limit

limit=1024
i=0

while test $i != $limit; do
	echo "hstructure S$i o:object end"
	i=$(($i+1))
done > s_structlimit.lua

# generate 224 slots in a structure to reach the VM limit
limit=224
i=0

{
	echo "hstructure S"
	while test $i != $limit; do
		echo "s$i:object"
		i=$(($i+1))
	done
	echo "end"
} > s_slotlimit.lua

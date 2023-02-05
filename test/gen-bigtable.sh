#!/bin/sh

file=$1
size=${2-200}

test "x$file" = x && exit 1

printf "local t = {\n" > $file


count=0
while test $count -le $size; do
	printf "\t100,\n"
	count=$((count+1))
done >> $file

printf "};\nt = {\n" >> $file

count=0
while test $count -le $size; do
	printf "\ta$count = 100,\n"
	count=$((count+1))
done >> $file

printf "};\nt = {\n" >> $file

count=0
while test $count -le $size; do
	printf "\t100,\n\ta$count = 100,\n"
	count=$((count+1))
done >> $file

printf "\t100,\n};\nt = {\n" >> $file

count=0
while test $count -le $size; do
	printf "\ta$count = 100,\n\t100,\n"
	count=$((count+1))
done >> $file

printf "\ta$count = 100,\n};\n" >> $file

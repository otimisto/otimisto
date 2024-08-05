#!/bin/bash

if [[ $# -ne 1 ]]; then
	echo "Usage: $0 <file>"
	exit 1
fi

rm -f a.out
g++ $1 -I../halide/include -L../halide/lib -lHalide -lpthread -ldl -lz
./a.out

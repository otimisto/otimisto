#!/bin/bash

if [[ $# -ne 2 ]]; then
	echo "Usage: $0 <halide C file> <output file schedsynth> <Line number to insert scheduling calls into>"
	exit 1
fi

sed "$3r $2" $1 $1.new

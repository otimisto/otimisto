#!/bin/bash

set -x
set -eu

if [[ $# -ne 1 ]]; then
	echo "Usage: $0 <testname>"
	exit 1
fi

file=$1
orig=${file}_orig
targ=${file}_targ
reshapes=${file}_reshapes

dir=$PWD
cd ../..
cargo run $dir/$orig $dir/$targ $dir/$reshapes $dir/${file}_sched_output --halide-program $dir/${file}_generated.c --halide-dir $PWD/scripts/halide/ --debug-opentuner

# Now, put this into the generated file:
sed -e "/SCHED_CONTENT/ {
r $dir/${file}_sched_output
d
}" $dir/${file}_generated.c > $dir/${file}_generated_target.c

# Build the new file and run it:
cd $dir
echo "Original Output: "
#./build.sh ${file}.c
echo "New Output: "
#./build.sh ${file}_generated_target.c

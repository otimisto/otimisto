#!/bin/echo source this file

if [[ ! -d halide ]]; then 
	wget https://github.com/halide/Halide/releases/download/v16.0.0/Halide-16.0.0-x86-64-linux-1e963ff817ef0968cc25d811a25a7350c8953ee6.tar.gz -O Halide.tar.gz
	tar -xvf Halide.tar.gz
	mv Halide-16.0.0-x86-64-linux/ halide
fi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/halide/lib

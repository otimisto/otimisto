#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
	Var x("x"), y("y");
	Var a("a"), b("b"), c("c"), d("d");

	Func producer("producer"), consumer("consumer");

	f1(x, y, z, c) = sin(x * y);
	f2(x, y, z, c) = (x * y) / 4;

	// Oroginal
	f1.print_loop_nest();
	printf("===============\n");
	// schedule
	Var yo, yi;

	f1.compute_root()
		.reorder(c, x, y, z)
		.reorder_storage(c, x, y, z)
		.vectorize(c)
		.unroll(y, 2, TailStrategy::RoundUp)
		.tile(x, y, z, xi, yi, zi, 32, 8, 1, TailStrategy::RoundUp);

	// target
	producer.print_loop_nest();
}


#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
	Var x("x"), y("y");

	Func producer("producer"), consumer("consumer");

	producer(x, y) = sin(x * y);
	consumer(x, y) = (producer(x, y) + producer(x, y + 1) + producer(x + 1, y) + producer(x + 1, y + 1)) / 4;

	// Oroginal
	producer.print_loop_nest();
	printf("===============\n");
	// schedule
	Var yo, yi;

	producer.compute_root();
	producer.parallel(y, 32);
	producer.vectorize(x, 8);

	// target
	producer.print_loop_nest();
}

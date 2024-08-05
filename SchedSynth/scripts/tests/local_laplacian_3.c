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

	consumer.split(y, yo, yi, 64);
	producer.store_at(consumer, yo)
	.compute_at(consumer, yi)
	.fold_storage(y, 4)
	.vectorize(x, 8);

	// target
	producer.print_loop_nest();
}

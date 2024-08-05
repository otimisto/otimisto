#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
	Var x("x"), y("y"), c("c");

	Func producer("producer"), consumer("consumer");

	producer(x, y) = sin(x * y);
	consumer(x, y) = (producer(x, y) + producer(x, y + 1) + producer(x + 1, y) + producer(x + 1, y + 1)) / 4;

	// Oroginal
	consumer.print_loop_nest();
	printf("===============\n");
	// schedule
	Var yo, yi;

	consumer.reorder(c, x, y).split(y, yo, yi, 64).parallel(yo).vectorize(x, 8);
	producer.store_at(consumer, yi);
	producer.compute_at(consumer, yo);

	// target
	consumer.print_loop_nest();
}

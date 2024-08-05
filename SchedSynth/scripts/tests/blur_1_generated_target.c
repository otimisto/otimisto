#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
	Var x("x"), y("y");

	Func producer("producer"), consumer("consumer");

	producer(x, y) = sin(x * y);
	consumer(x, y) = (producer(x, y) + producer(x, y + 1) + producer(x + 1, y) + producer(x + 1, y + 1)) / 4;

	// Oroginal
	consumer.print_loop_nest();
	printf("===============\n");
	// schedule
	Var yo, yi, xo, xi;
	/* consumer.split(y, yo, yi, 16); */
	/* // Compute the strips using a thread pool and a task queue. */
	/* consumer.parallel(yo); */
	/* // Vectorize across x by a factor of four. */
	/* consumer.vectorize(x, 4); */

	/* // Now store the producer per-strip. This will be 17 scanlines */
	/* // of the producer (16+1), but hopefully it will fold down */
	/* // into a circular buffer of two scanlines: */
	/* producer.store_at(consumer, yo); */
	/* // Within each strip, compute the producer per scanline of the */
	/* // consumer, skipping work done on previous scanlines. */
	/* producer.compute_at(consumer, yi); */
	/* // Also vectorize the producer (because sin is vectorizable on x86 using SSE). */
	/* producer.vectorize(x, 4); */
consumer.split(y, yo, yi, 19);
producer.split(x, xo, xi, 4);
producer.store_at(consumer, yo);
producer.vectorize(xi);

	// target
	consumer.print_loop_nest();
}

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
	Var yo, yi;

	blur_x(x, y) = (input(x, y) + input(x + 1, y) + input(x + 2, y)) / 3;                         
	blur_y(x, y) = (blur_x(x, y) + blur_x(x, y + 1) + blur_x(x, y + 2)) / 3;                      

	blur_y
                  .split(y, y, yi, 32)
                  .parallel(y)
                  .vectorize(x, 16);
	blur_x
                  .store_at(blur_y, y)
                  .compute_at(blur_y, x)
                  .vectorize(x, 16);

	// target
	blur_x.print_loop_nest();
}

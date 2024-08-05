#include "Halide.h"

	using namespace Halide;

int main() {
	        const int N = 5, CI = 128, CO = 128, W = 100, H = 80;
			const int vec = 4;
			int tile_w = 4;
			int tile_h = 4;

	Var x("x"), y("y"), c("c"), n("n"), z("z");
	Var co("co"), ci("ci"), xo("xo"), xi("xi"), yo("yo"), yi("yi"), t("t");

	Func conv("conv");
	Func input("input");
	Func filter("filter");
	Func bias("bias");
	Func relu("relu");

	input(x, y, c, z) = x;
	filter(x, y, c, z) = x;
	bias(c) = c;

	RDom r(0, CI, 0, 3, 0, 3);

	conv(c, x, y, n) = bias(c);
	conv(c, x, y, n) += filter(c, r.y, r.z, r.x) * input(r.x, x + r.y, y + r.z, n);

	relu(c, x, y, n) = max(0, conv(c, x, y, n));


	relu.print_loop_nest();
	relu.split(c, co, ci, vec * tile_w)
		.split(x, xo, xi, tile_h)
		.reorder(ci, xi, xo, y, n, co)
		.vectorize(ci, vec)
		.unroll(ci)
		.unroll(xi)
		.parallel(y)
		.parallel(n)
		.parallel(co);

	relu.print_loop_nest();
}

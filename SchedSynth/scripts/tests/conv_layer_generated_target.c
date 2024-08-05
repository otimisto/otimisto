
#include "Halide.h"

	using namespace Halide;

int main() {
	        const int N = 5, CI = 128, CO = 128, W = 100, H = 80;
			const int vec = 4;
			int tile_w = 4;
			int tile_h = 4;

	Var x("x"), y("y"), c("c"), n("n"), z("z");
	Var co("co"), ci("ci"), ci2("ci2"), ci3("ci3"), xo("xo"), xi("xi"), yo("yo"), yi("yi"), t("t");

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


relu.split(c, co, ci, 16);
relu.split(x, xo, xi, 4);
relu.split(ci, ci2, ci3, 4);
relu.reorder(xi, co);
relu.reorder(xo, co);
relu.reorder(y, co);
relu.reorder(n, co);
conv.compute_at(relu, ci3);
relu.compute_root();
relu.vectorize(ci3);
relu.parallel(y);
relu.parallel(n);
relu.parallel(co);
	/* filter.in() */
	/* 	.compute_at(conv, r.x) */
	/* 	.vectorize(_0, vec) */
	/* 	.unroll(_0) */
	/* 	.unroll(_3); */
	/* input.in() */
	/* 	.compute_at(conv, x) */
	/* 	.unroll(_0); */

	relu.print_loop_nest();
}

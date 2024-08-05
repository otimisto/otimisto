#include "Halide.h"
// #include "halide_trace_config.h"

using namespace Halide;

    void generate() {
//     Output<Buffer<float, 2>> bilateral_grid{"bilateral_grid"};
        Var x("x"), y("y"), z("z"), c("c");

        // Add a boundary condition
		Func clamped;
		Func bilateral_grid;
        clamped(x, y) = 0.0f;

        // Construct the bilateral grid
        Expr val = clamped(x * 2, y / 2);
        val = clamp(val, 0.0f, 1.0f);

        Expr zi = cast<int>(val * (1.0f) + 0.5f);

        Func histogram("histogram");
        histogram(x, y, z, c) = 0.0f;
        histogram(x, y, zi, c) += mux(c, {val, 1.0f});

        // Blur the grid using a five-tap filter
        Func blurx("blurx"), blury("blury"), blurz("blurz");
        blurz(x, y, z, c) = (histogram(x, y, z - 2, c) +
                             histogram(x, y, z - 1, c) * 4 +
                             histogram(x, y, z, c) * 6 +
                             histogram(x, y, z + 1, c) * 4 +
                             histogram(x, y, z + 2, c));
        blurx(x, y, z, c) = (blurz(x - 2, y, z, c) +
                             blurz(x - 1, y, z, c) * 4 +
                             blurz(x, y, z, c) * 6 +
                             blurz(x + 1, y, z, c) * 4 +
                             blurz(x + 2, y, z, c));
        blury(x, y, z, c) = (blurx(x, y - 2, z, c) +
                             blurx(x, y - 1, z, c) * 4 +
                             blurx(x, y, z, c) * 6 +
                             blurx(x, y + 1, z, c) * 4 +
                             blurx(x, y + 2, z, c));

        // Take trilinear samples to compute the output
        val = clamp(1.0f, 0.0f, 1.0f);
        Expr zv = val * (1.0f);
        zi = cast<int>(zv);
        Expr zf = zv - zi;
        Expr xf = cast<float>(x);
        Expr yf = cast<float>(y);
        Expr xi = x;
        Expr yi = y;
        Func interpolated("interpolated");
        interpolated(x, y, c) =
            (blury(xi, yi, zi, c), blury(xi + 1, yi, zi, c), xf),
                      blury(xi, yi + 1, zi, c), blury(xi + 1, yi + 1, zi, c), xf, yf,
					  blury(xi, yi, zi + 1, c), blury(xi + 1, yi, zi + 1, c), xf,
					  blury(xi, yi + 1, zi + 1, c), blury(xi + 1, yi + 1, zi + 1, c), xf, yf,
                 zf;

        // Normalize
        bilateral_grid(x, y) = interpolated(x, y, 0) / interpolated(x, y, 1);

        /* ESTIMATES */
        // (This can be useful in conjunction with RunGen and benchmarks as well
        // as auto-schedule, so we do it in all cases.)
        // Provide estimates on the input image
        /* input.set_estimates({{0, 1536}, {0, 2560}}); */
        // Provide estimates on the parameters
        /* r_sigma.set_estimate(0.1f); */
        // TODO: Compute estimates from the parameter values
        /* histogram.set_estimate(z, -2, 16); */
        /* blurz.set_estimate(z, 0, 12); */
        /* blurx.set_estimate(z, 0, 12); */
        /* blury.set_estimate(z, 0, 12); */
        /* bilateral_grid.set_estimates({{0, 1536}, {0, 2560}}); */

            // CPU schedule.

            // 2.04ms on an Intel i9-9960X using 32 threads at 3.5 GHz using
            // target "host". This is a little less SIMD-friendly than some of the
            // other apps, so we benefit from hyperthreading, and don't benefit
            // from 512-bit vectors.


			blurx.print_loop_nest();
			printf("======\n");
            blurz.compute_at(blurx, y)
                .reorder(c, z, x, y)
                .vectorize(x, 8)
                .unroll(c);
            histogram.compute_at(blurz, y);
            /* histogram.update() */
            /*     .reorder(c, r.x, r.y, x, y) */
            /*     .unroll(c); */
            blurx.compute_root()
                .reorder(c, x, z, y)
                .parallel(y)
                .vectorize(x, 8)
                .unroll(c);
            blury.compute_at(bilateral_grid, y)
                .store_in(MemoryType::Stack)
                .reorder(c, x, y, z)
                .reorder_storage(c, z, x, y)
                .vectorize(x, 8)
                .unroll(c);
            bilateral_grid.compute_root()
                .parallel(y, 8)
                .vectorize(x, 8);

			blurx.print_loop_nest();
	}
int main() {
	generate();
}

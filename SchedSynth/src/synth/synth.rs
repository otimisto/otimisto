use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::options::options::Options;
use crate::gen::target::Backend;
use crate::gen::target::TargetLower;
use crate::gen::target::TargetGenerate;
use crate::holes::holes::fill_structural_holes;

fn synthesize_candidates(opts: &Options, backend_type: Backend, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> String {
    // Infer over any structural holes.
    let filled_target = fill_structural_holes(opts, source, target, reshapes);
    // assert that all structural holes are filled
    crate::holes::holes::assert_no_structural_holes(&filled_target);

    // Go through the various halide exprs and get the calls for them.
    let infered_reshapes = &crate::ast::ast::insert_reorders(opts, reshapes, source, &filled_target);
    let infered_reorders = &crate::reshape::reshape::infer_reorders_between(opts, source, &filled_target, infered_reshapes);

    let mut backend = crate::gen::target::newBackend(backend_type);

    backend.to_reshape(infered_reshapes);
    backend.to_reshape(infered_reorders);
    backend.to_compute_at(crate::ast::ast::get_compute_at(opts, &filled_target));
    backend.to_compute_with(crate::ast::ast::infer_compute_with(opts, &filled_target, &infered_reshapes));
    backend.to_store_at(crate::ast::ast::get_store_at(opts, &filled_target));
    backend.to_vectorize(crate::ast::ast::get_vectorized(opts, &filled_target));
    backend.to_parallel(crate::ast::ast::get_parallel(opts, &filled_target));
    backend.to_unroll(crate::ast::ast::get_unroll(opts, &filled_target));
    backend.to_prefetch(crate::ast::ast::get_prefetches(opts, &filled_target));
    backend.to_func_property(crate::ast::ast::get_func_properties(opts, &filled_target));

    let candidates =  vec![
        backend
    ];

    let best_candidate =
        if opts.no_opentuner {
            candidates[0].clone()
        } else {
            crate::runner::runner::best_schedule(opts, candidates)
        };
    best_candidate.generate()
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> String {
    let best_candidate = synthesize_candidates(opts, opts.backend, source, target, reshapes);
    return best_candidate
}

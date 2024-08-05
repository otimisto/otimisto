use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::ForRange;
use crate::ast::ast::AST;
use crate::ast::ast::ASTUtils;
use crate::ast::ast::NumberOrHole;
use crate::ast::ast::VarOrHole;
use crate::ast::ast::HoleOptionTrait;

use crate::options::options::Options;
use std::collections::HashMap;

use std::fmt;

#[derive(Clone)]
pub enum Reshape {
    Split(Func, Var, (Var, Var), NumberOrHole),
    Reorder(Func, (Var, Var)),
    Fuse(Func, (Var, Var), Var)
}

pub trait ReshapeFunctions {
    fn is_split(&self) -> bool;
    fn is_reorder(&self) -> bool;
    fn is_fuse(&self) -> bool;
}

impl ReshapeFunctions for Reshape {
    fn is_split(&self) -> bool { match self { Reshape::Split(_, _, _, _) => true, _ => false } }
    fn is_reorder(&self) -> bool { match self { Reshape::Reorder(_, _) => true, _ => false } }
    fn is_fuse(&self) -> bool { match self { Reshape::Fuse(_, _, _) => true, _ => false } }
}

#[derive(Clone)]
pub enum OrderingConstraint {
    Nested(Var, Var), // Require that var, var are nested within each other
}

impl fmt::Display for OrderingConstraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OrderingConstraint::Nested(var1, var2) => write!(f, "{} > {}", var1, var2),
        }
    }
}

impl fmt::Display for Reshape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reshape::Split(func, var, (outer, inner), factor) => write!(f, "Split({}, {}, ({}, {}), {})", func, var, outer, inner, factor),
            Reshape::Reorder(func, (outer, inner)) => write!(f, "Reorder({}, ({}, {}))", func, outer, inner),
            Reshape::Fuse(func, (outer, inner), fused) => write!(f, "Fuse({}, ({}, {}), {})", func, outer, inner, fused)
        }
    }
}

impl fmt::Debug for Reshape {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Reshape::Split(func, var, (dim1, dim2), factor) => {
                write!(f, "Split({}, {}, ({}, {}), {})", func, var, dim1, dim2, factor)
            },
            Reshape::Reorder(func, (dim1, dim2)) => {
                write!(f, "Reorder({}, ({}, {}))", func, dim1, dim2)
            },
            Reshape::Fuse(func, (dim1, dim2), var) => {
                write!(f, "Fuse({}, ({}, {}), {})", func, dim1, dim2, var)
            }
        }
    }
}

// take a reshape and generate the reverse -- needed for
// the explicit cross-function fuses.
// Note that information is not completely
// preserved --- loose the split factors.
pub fn reverse_reshape(reshape: &Reshape) -> Reshape {
    match reshape {
        Reshape::Split(func, fromv, (tov1, tov2), _factor) =>
            Reshape::Fuse(func.clone(), (tov1.clone(), tov2.clone()), fromv.clone()),
        Reshape::Reorder(func, (v1, v2)) =>
            Reshape::Reorder(func.clone(), (v2.clone(), v1.clone())),
        Reshape::Fuse(func, (fromv1, fromv2), tov) =>
            // don't know the number, so make ti a number of -1 to make that very
            // clear an explicit.
            Reshape::Split(func.clone(), tov.clone(), (fromv1.clone(), fromv2.clone()), NumberOrHole::Number(-1)),
    }
}

pub fn reverse_reshapes(reshapes: &Vec<Reshape>) -> Vec<Reshape> {
    reshapes.iter().map(|r| reverse_reshape(r)).collect()
}

// if can apply, return the ast that this has been applied to.
fn can_apply(ast: &AST, reshape: &Reshape) -> Option<AST> {
    // does the reshape reshape match the ast?
    match reshape {
        Reshape::Split(_f, v, (v1, v2), factor) => {
            if ast.is_loop_type() {
                let variable = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                let properties = ast.get_properties();
                if variable == *v {
                    let inner_loop = ast.get_substruct().unwrap();
                    Some(
                        // TODO -- preserve vect/parallel?
                        AST::For(
                            VarOrHole::Var(v1.clone()), Box::new(AST::For(
                                VarOrHole::Var(v2.clone()), Box::new(inner_loop), ForRange::Between(NumberOrHole::Number(0), factor.clone()), properties.clone()
                            )), ForRange::All(), properties.clone()
                        )
                    )
                } else {
                    None
                }
            } else {
                None
            }
        }
        Reshape::Reorder(_f, (v1, v2)) => {
            if ast.is_loop_type() {
                let variable_1 = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                if variable_1 == *v1 {
                    let inner_loop = ast.get_substruct().unwrap();
                    let v1_range = ast.get_iteration_range().unwrap();
                    let v1_properties = ast.get_properties();

                    if inner_loop.is_loop_type() {
                        let variable_2 = inner_loop.get_iteration_variable().unwrap();
                        let v2_range = inner_loop.get_iteration_range().unwrap();
                        let inner_inner_loop = inner_loop.get_substruct().unwrap();
                        let v2_properties = inner_loop.get_properties();

                        if variable_2 == *v2 {
                            Some(
                                AST::For(
                                    VarOrHole::Var(v2.clone()), Box::new(AST::For(
                                            VarOrHole::Var(v1.clone()), Box::new(inner_inner_loop),
                                            v1_range.clone(), v1_properties
                                    )), v2_range.clone(), v2_properties
                                )
                            )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Reshape::Fuse(_f, (v1, v2), v) => {
            if ast.is_loop_type() {
                let outer_variable = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                let inner_loop = ast.get_substruct().unwrap();

                if inner_loop.is_loop_type() {
                    let inner_variable = inner_loop.get_iteration_variable().unwrap();
                    let inner_inner_loop = inner_loop.get_substruct().unwrap();

                    // We just throw away the properties here -- should we preseve?
                    if outer_variable == *v1 && inner_variable == *v2 {
                        Some(
                            AST::For(
                                VarOrHole::Var(v.clone()), Box::new(inner_inner_loop), ForRange::All(), vec![]
                            )
                        )
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

fn apply_reshape(ast: &AST, reshape: &Reshape) -> (AST, bool) {
    match can_apply(ast, reshape) {
        Some(new_ast) => (new_ast, true),
        None => {
            // recurve through the ast and see if we can find somehwere
            // to apply the reshape.
            match ast {
                AST::Produce(func, ast, props) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::Produce(func.clone(), Box::new(new_ast), props.clone()), applied)
                }
                AST::Consume(func) => {
                    (AST::Consume(func.clone()), false)
                }
                AST::For(var, ast, range, properties) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::For(var.clone(), Box::new(new_ast), range.clone(), properties.clone()), applied)
                }
                AST::Assign(func) => {
                    (AST::Assign(func.clone()), false)
                }
                AST::StoreAt(func) => {
                    (AST::StoreAt(func.clone()), false)
                }
				AST::Prefetch(func, var, stride) => {
					(AST::Prefetch(func.clone(), var.clone(), stride.clone()), false)
				}
                AST::StructuralHole(ast) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::StructuralHole(Box::new(new_ast)), applied)
                }
                AST::Sequence(asts) => {
                    let mut new_asts = Vec::new();
                    let mut applied = false;
                    for ast in asts {
                        let (new_ast, applied_in_ast) = apply_reshape(&*ast, reshape);
                        applied = applied | applied_in_ast;
                        new_asts.push(new_ast);
                    }
                    (AST::Sequence(new_asts), applied)
                }
            }
        }
    }
}

pub fn apply_reshapes(ast: &AST, reshapes: &Vec<Reshape>) -> AST {
    let mut new_ast = ast.clone();
    for reshape in reshapes {
        let (new_ast_internal, applied) = apply_reshape(&new_ast, reshape);
        new_ast = new_ast_internal;

        if !applied {
            println!("Warning: did not apply rule {}", reshape);
        }
    }
    new_ast
}

fn infer_constraints_from(reshape: &Reshape) -> Vec<OrderingConstraint> {
 match reshape {
        Reshape::Split(_func, _var, (_outer, _inner), _factor) => {
            vec![]
        },
        Reshape::Reorder(_func, (outer, inner)) => {
            vec![OrderingConstraint::Nested(inner.clone(), outer.clone())]
        },
        Reshape::Fuse(_func, (outer, inner), _fused) => {
            vec![OrderingConstraint::Nested(outer.clone(), inner.clone())]
        }
    }
}

fn infer_reorder_over(opts: &Options, ast: &AST, constraint: OrderingConstraint) -> Vec<Reshape> {
    if opts.debug_reshape {
        println!("Infering reorders for constraints {}", constraint);
    }
    let func_lookup = crate::ast::analysis::func_table_for(opts, ast);
    let res = match constraint {
        // infer the list of reorder constraints required
        // to support this.
        OrderingConstraint::Nested(outer, inner) => enforce_nested(opts, ast, outer, inner, &func_lookup, false, false)
    };
    if opts.debug_reshape {
        println!("Inferred reorders {:?}", res);
    };
    res
}

fn enforce_nested(opts: &Options, ast: &AST, outer: Var, inner: Var, func_lookup: &HashMap<Var, Func>, found_outer: bool, found_inner: bool) -> Vec<Reshape> {
    // Recursively walk through the AST --- find either the outer, or the inner.
    // Then reorder all the other dimensions to make sure that the outer is outermost,
    // and the inner is innermost.
    if opts.debug_reshape {
        println!("Enforcing nesting {} > {} on ast {}", outer, inner, ast);
    };
    let res = match ast {
        AST::Produce(_func, subast, _props) => {
            enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner)
        },
        AST::Consume(_func) => {
            vec![]
        },
        AST::For(var_hole, subast, _range, _properties) => {
            let var = var_hole.get().unwrap(); // must have no holes to reorder.
                                               // I guess we could rewrite this if required
            let this_is_inner = var == inner;
            let this_is_outer = var == outer;
            if opts.debug_reshape {
                println!("Found inner var: {}, found outer var: {}", this_is_inner | found_inner,
                    this_is_outer | found_outer)
            }
            // breakdown by cases --- if we have seen the inner and outer,
            // we are done -- possibly needing to reorder those if they are in
            // the wrong order.
            if found_outer {
                if this_is_inner {
                    vec![]
                } else {
                    // This is not the inner --- insert a reordering that swaps the outer
                    // and this loop --- and recurse
                    // TODO -- check that both vars have the same parent.
                    let new_reshape = Reshape::Reorder(func_lookup.get(&var).unwrap().clone(), (var.clone(),
                    outer.clone()));
                    let mut reorders = enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner);
                    reorders.push(new_reshape);
                    reorders
                }
            } else if found_inner {
                if this_is_outer {
                    // we are done -- but still need to swap inner and outer.
                    vec![Reshape::Reorder(func_lookup.get(&var).unwrap().clone(), (outer.clone(),
                    inner.clone()))]
                } else {
                    // we are still looking for the outer --- recurse
                    // and insert a swap for this loop and the inner.
                    let new_reshape = Reshape::Reorder(func_lookup.get(&var).unwrap().clone(), (var.clone(),
                    inner.clone()));
                    let mut reorders = enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner);
                    reorders.push(new_reshape);
                    reorders
                }
            } else {
                // have thus far found neither --- recurse (note that this_is_inner and
                // this_is_outer can be passed as args here)
                enforce_nested(opts, &*subast, outer, inner, func_lookup, this_is_outer, this_is_inner)
            }
            // If we have seen just one, need to insert a reorder.  We aim towards
            // moving the outer loop in rather than the inner loop out.

        },
        AST::Assign(_func) => vec![],
        AST::StoreAt(_func) => vec![],
		AST::Prefetch(_func, _, _) => vec![],
        AST::StructuralHole(subast) => {
            enforce_nested(opts, &subast, outer.clone(), inner.clone(), func_lookup, found_outer, found_inner)
        },
        AST::Sequence(seq) => {
            let mut reorders = vec![];
            for subast in seq {
                let mut new_reorders = enforce_nested(opts, &subast, outer.clone(), inner.clone(), func_lookup, found_outer, found_inner);
                reorders.append(&mut new_reorders);
            }
            reorders
        }
    };
    if opts.debug_reshape {
        println!("Inferred the following reorders: {:?}", res);
    }
    res
}

fn infer_reorders_over(opts: &Options, ast: &AST, order_constraints: &Vec<OrderingConstraint>) -> (AST, Vec<Reshape>) {
    // Go through the ast, and determine every set of reorders required
    match order_constraints.len() {
        // If there are no order constraints, then no reorders are required
        0 => (ast.clone(), vec![]),
        _n => {
            let (head, tail) = order_constraints.split_at(1);

            let mut reshapes = infer_reorder_over(opts, ast, head[0].clone());

            // need to apply the reshapes --- otherwise other constraints may not
            // make sense.
            let new_ast = apply_reshapes(ast, &reshapes);
            let (result_ast, mut rest_of_reshapes) = infer_reorders_over(opts, &new_ast, &tail.to_vec());
            reshapes.append(&mut rest_of_reshapes);
            (result_ast, reshapes)
        }
    }
}

// Aim of this is to go through each reshape, and infer the constraints
// that it imposes on variable ordering.  From that, we can infer
// which reorders we have to use.
pub fn inject_reorders(opts: &Options, ast: &AST, reshapes: &Vec<Reshape>) -> Vec<Reshape> {
    let mut result_reshapes = Vec::new();
    let mut new_ast = ast.clone();
    if opts.debug_reshape {
        println!("Looking for constraints required for reshapes {:?}", reshapes);
    }
    for reshape in reshapes {
        let order_constraints = infer_constraints_from(reshape);
        let (new_ast_intermediate, mut reorders) = infer_reorders_over(opts, &new_ast, &order_constraints);

        result_reshapes.append(&mut reorders); // put the infered reorders
                                               // into a list.
        result_reshapes.push(reshape.clone());
        // apply all the reorders
        let (new_ast_intermediate, _applied) = apply_reshape(&new_ast_intermediate, reshape);
        new_ast = new_ast_intermediate;

        if opts.debug_reshape {
            println!("After reshape {}, have ast {}", reshape, new_ast);
        }
    }

    result_reshapes
}

pub fn infer_reorders_between(opts: &Options, orig_ast: &AST, targ_ast: &AST, transformations: &Vec<Reshape>) -> Vec<Reshape> {
    // Assuming that orig_ast and targ_ast both have the same
    // set of variables, infer the rewrites required
    // to convert one to the other
    
    let transformed_orig = apply_reshapes(orig_ast, transformations);
    
    let reorders = crate::ast::ast::find_reorders(opts, &transformed_orig, targ_ast);

    reorders.iter().map(|(f, v, v2)| Reshape::Reorder(f.clone(), (v.clone(), v2.clone()))).collect()
}

pub fn get_reshape_lhs(reshape: Reshape) -> Vec<Var> {
    match reshape {
        Reshape::Split(_, l, _, _) => vec![l],
        Reshape::Reorder(_, (v1, v2)) => vec![v1, v2],
        Reshape::Fuse(_, (v1, v2), _) => vec![v1, v2],
    }
}

pub fn get_reshape_rhs(reshape: Reshape) -> Vec<Var> {
    match reshape {
        Reshape::Split(_, _, (v1, v2), _) => vec![v1, v2],
        Reshape::Reorder(_, (v1, v2)) => vec![v1, v2],
        Reshape::Fuse(_, _, v) => vec![v],
    }
}

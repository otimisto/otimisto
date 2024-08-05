use std::collections::HashMap;
use crate::options::options::Options;
use crate::ast::ast::AST;
use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::ForRange;
use crate::ast::ast::VarOrHole;
use crate::ast::ast::HoleOptionTrait;

// recursively walk through the ast --- for every loop node (vect or for)
// add to the lookup table so we can access the range that that variable
// takes.
fn range_table_for_internal(opts: &Options, ast: &AST, table: &mut HashMap<Var, ForRange>) {
    match ast {
        AST::Produce(_var, ast, _props) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::Consume(_var) => {
            ()
        },
        AST::For(var, ast, range, _properties) => {
            match var {
                VarOrHole::Var(v) => {
                    table.insert(v.clone(), range.clone());
                },
                VarOrHole::Hole() => (),
            };
            range_table_for_internal(opts, ast, table)
        },
        AST::Assign(_var) => (),
        AST::StoreAt(_var) => (),
        AST::Prefetch(_buf, _var, _stride) => (),
        AST::StructuralHole(ast) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::Sequence(asts) => {
            for ast in asts {
                range_table_for_internal(opts, ast, table);
            }
        }
    }
}

pub fn range_table_for(opts: &Options, ast: &AST) -> HashMap<Var, ForRange> {
    let mut map = HashMap::new();
    range_table_for_internal(opts, ast, &mut map);
    map
}

// Each variable should have a parent func it refers to. Using compute_at
// it's possible to have a single varibale with multiple funcs, and this won't
// handle that --- it takes the outermost instance of a variable/func pair.
// keep track of the name of the func producer, and for each for/vectorize
// var that we encounter, add it to the map so we can look up the producer

// if current_producer is none then we throw (?what should we do?)
fn func_table_internal(opts: &Options, ast: &AST, current_producer: &Option<Func>, table: &mut HashMap<Var, Func>) {
    match ast {
        AST::Produce(var, body, _props) => {
            func_table_internal(opts, body, &Some(var.clone()), table);
        },
        AST::Consume(_var) => {
			// nothing
        },
        AST::For(var_hole, body, _, _properties) => {
            let var = var_hole.get().unwrap(); // need to have non-hole at this point.
            // Note that this probably isn't the binding order we'd really
            // like to have --- but it should be good enough --- really
            // need to make sure we make variables
            if !table.contains_key(&var) {
                if let Some(producer) = current_producer {
                    table.insert(var.clone(), producer.clone());
                    if opts.debug_func_table { 
                        println!("Adding {}, {} to table", var.clone(), producer.clone());
                    }
                } else {
                    panic!("No producer for variable {:?}", var.to_string());
                }
            } else {
                // TODO -- want to deal with this warning eventually
                println!("Warning: encountered variable {} more than once", var)
            }
            func_table_internal(opts, body, current_producer, table);
        },
        AST::StructuralHole(body) => {
            func_table_internal(opts, body, current_producer, table);
        }
        AST::Assign(_var) => {
            // Should we assert that var is the same as producer?
        },
        AST::Prefetch(_, _var, _) => {
			// should we assert? (as above?)
		},
        AST::StoreAt(_func) => { },
        AST::Sequence(seq) => {
            for ast in seq {
                func_table_internal(opts, ast, current_producer, table);
            }
        }
    }
}

pub fn func_table_for(opts: &Options, ast: &AST) -> HashMap<Var, Func> {
    let mut table = HashMap::new();
    func_table_internal(opts, ast, &None, &mut table);
    table
}

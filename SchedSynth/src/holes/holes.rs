use crate::ast::ast::AST;
use crate::ast::ast::ASTUtils;
use crate::ast::ast::Var;
use crate::ast::ast::get_all_funcnames;
use crate::ast::ast::split_ast_into_funcs;
use crate::ast::ast::ForRange;
use crate::ast::ast::HoleOptionTrait;
use crate::ast::ast::VarOrHole;
use crate::ast::ast::FuncProperty;
use std::collections::HashMap;
use crate::options::options::Options;
use crate::reshape::reshape::Reshape;
use crate::reshape::reshape::apply_reshapes;
use std::process::Command;

pub enum HoleStructure {
    StructuralHole(),
    VariableHole(),
    Loop(i32)
}

impl ToString for HoleStructure {
 fn to_string(&self) -> String {
        match self {
            HoleStructure::StructuralHole() => "hole".to_string(),
            HoleStructure::VariableHole() => "single_hole".to_string(),
            HoleStructure::Loop(i) => i.to_string(),
        }
    }
}

pub fn to_hole_string(structure: Vec<HoleStructure>) -> String {
    // generate the structure required
    // join the holes into a single array comma separated
    let mut hole_string = String::new();
    for hole in structure {
        hole_string.push_str(&format!("{},", hole.to_string()));
    }
    // remove the last comma
    hole_string.pop();
    hole_string
}

pub fn from_hole_string(s: String) -> Vec<i32> {
    let vectors = s.split(",");
    // map each elt to an int
    let mut int_vectors: Vec<i32> = Vec::new();
    for v in vectors {
        let v_int = v.parse::<i32>().unwrap();
        int_vectors.push(v_int);
    }
    int_vectors
}

fn print_varmap(map: &HashMap<Var, i32>) {
    for (v, val) in map {
        println!("{} = {}", v, val);
    }
}

// Parse the AST into the abstract hole structure and apply ilp.
// Note that original_ast /must/ have all the splits/fuses
// applied --- it must have all the variables we need.
// return the vars in order.
pub fn run_ilp_solver(opts: &Options, original_ast: &AST, holey_ast: &AST) -> Vec<Var> {
    // So it is possible to have multiple loop nests rather
    // than the linear model that the python script assumes,
    // but this isn't really common in the halide model
    // so we'll only support the linear case here.

    // First, number the variables using the map:
    let mut varmap = HashMap::<Var, i32>::new();
    let mut varcount = 1; // ILP solver starts with 1
    map_builder(original_ast, &mut varmap, &mut varcount);
    let hole_structure = hole_structure_builder(holey_ast, &varmap);

    // Get this as a string
    let hole_structure_list = to_hole_string(hole_structure);
    if opts.debug_ilp_solver {
        println!("Holey ast is {}", holey_ast);
        println!("Hole list {}", hole_structure_list);
        println!("Max is {}", varcount);
        print_varmap(&varmap);
    }
    // get the max we care about

    // Run the ILP solver and get the right order back:
    let output = Command::new("python3")
        .arg("scripts/hole_infer.py")
        .arg(hole_structure_list.clone())
        .arg((varcount - 1).to_string())
        .output()
        .expect("Failed to execute hole inference");
    let output_string = String::from_utf8_lossy(&output.stdout);
    let output_stderr = String::from_utf8_lossy(&output.stderr);

    if opts.debug_ilp_solver {
        println!("stderr of ilp solver is {}", output_stderr);
        println!("Result of ilp solver is {}", output_string);
    }

    // Get the line in output_string that starts with Result: --- the list 
    // is after that.  Parse it into a vector of integers.
    let mut result_vec: Vec<i32> = Vec::new();
    for line in output_string.lines() {
        if line.starts_with("Result:") {
            // line is of the form Result:[X, Y, ,....]
            let preprocessed_line = &line[9..line.len()-1];
            let result_string = preprocessed_line.split(",").collect::<Vec<&str>>();
            for result in result_string {
                result_vec.push(result.trim().parse::<i32>().unwrap());
            }
            break;
        }
    }

    // Now, we have the result vector.  We need to map it back to the 
    // original hole structure list using the map

    let mut result_vec_vars: Vec<Var> = Vec::new();
    let mut pushed = false;
    for result in result_vec {
        for (var, num) in &varmap {
            if *num == result {
                result_vec_vars.push(var.clone());
                pushed = true;
            }
        }

        assert!(pushed); // need to have found the map for each variable!
    };

    result_vec_vars
}

fn hole_structure_builder(ast: &AST, map: &HashMap<Var, i32>) -> Vec<HoleStructure> {
    // ATM only support linear ASTs.
    match ast {
        AST::Produce(_f, subast, _prop) =>
            hole_structure_builder(&subast, map),
        AST::Consume(_f) =>
            vec![],
        AST::For(v, subast, _range, _props) => {
            let mut result = hole_structure_builder(&subast, map);
            match v {
                VarOrHole::Var(var) => {
                    // println!("Var {}", var.to_string());
                    result.insert(0, HoleStructure::Loop(map.get(&var).unwrap().clone()))
                },
                VarOrHole::Hole() => result.insert(0, HoleStructure::VariableHole()),
            }
            return result;
        },
        AST::Assign(_f) =>
            vec![],
        AST::StoreAt(_f) =>
            vec![],
		AST::Prefetch(_buf, _var, _stride) => vec![],
        AST::StructuralHole(subast) => {
            let mut result = hole_structure_builder(&subast, map);
            result.insert(0, HoleStructure::StructuralHole());
            return result;
        }
        AST::Sequence(subasts) => {
            // OK, so we currently don't support truly nested
            // fused loops --- the ILP model really should
            // be expanded to support these, but it doens't ATM.
            // Those aren't really in the hlaide model anyway.
            // So, find the nested imte and proceed down that
            // in the recursion
            let mut found = 0;
            let mut res = None;
            for ast in subasts {
                if ast.is_main_nest() {
                    res = Some(hole_structure_builder(ast, map));
                    found += 1;
                }
            };
            assert!(found <= 1);
            if found == 0 {
                // only empty subtrees
                vec![]
            } else {
                res.unwrap()
            }
        }
    }
}

// Build a map from var to name of that var in the ILP formulation (an int)
fn map_builder(target_ast: &AST, current_map: &mut HashMap<Var, i32>, current_max: &mut i32) {
    match target_ast {
        AST::Produce(_f, subast, _prop) =>
            map_builder(subast, current_map, current_max),
        AST::Consume(_f) =>
            (),
        AST::For(v, subast, _f, _p) => {
            current_map.insert(v.get().unwrap(), current_max.clone());
            *current_max += 1;
            map_builder(subast, current_map, current_max)
        },
        AST::Assign(_f) => (),
        AST::StoreAt(_f) => (),
        AST::Prefetch(_, _, _) => (),
        AST::StructuralHole(subast) =>
            map_builder(subast, current_map, current_max),
        AST::Sequence(fs) => {
            for ast in fs {
                map_builder(ast, current_map, current_max);
            }
        },
    }
}

// Fill the holes in ast_with_holes
pub fn fill_structural_holes(opts: &Options, original_ast: &AST, ast_with_holes: &AST, reshapes: &Vec<Reshape>) -> AST {
    // apply the reshapes to the original to get the ast
    // we actually want to compute min distance to:
    let modified_original = apply_reshapes(original_ast, reshapes);

    // We need to break these down per-func in the definition
    // one ilp instance per func.
    let original_funcs = split_ast_into_funcs(&modified_original);
    let hole_funcs = split_ast_into_funcs(ast_with_holes);
    let funcnames = get_all_funcnames(&modified_original);

    // If there is a top-level hole, then we need to insert
    // any missing func-defs into that top-level hole.
    // TODO --- synthesize the halide-specific stuff rather
    // than just the loop orders

    let mut funcs_vec = vec![];
    for func in funcnames {
        if opts.debug_ilp_solver {
            println!("Checking func {}", func.to_string());
        }
        // Now,run ilp solver on this if there are any structural holes.
        let (hole_func, hole_props) = hole_funcs.get(&func).unwrap();
        if hole_func.has_structural_holes() {
            if opts.debug_ilp_solver {
                println!("Func {} has structural holes, running ILP", func.to_string());
            }
            let (orig_func, orig_props) = original_funcs.get(&func).unwrap();

            let result_variable_order = run_ilp_solver(opts, &orig_func, hole_func);
            let filled_hole_body = fill_holes(hole_func, &result_variable_order);
            // rewrap the func in a product:
            let filled_hole_func =
                AST::Produce(func.clone(), Box::new(filled_hole_body), orig_props.clone());

            if opts.debug_ilp_solver {
                println!("Func with resolved holes are {}", filled_hole_func.to_string());
            }
            // fill the holes with the variables in the result_variable_order.
            funcs_vec.push(filled_hole_func)
        } else {
            if opts.debug_ilp_solver {
                println!("Func {} has no structural holes, skipping", func.to_string());
            }

            // push the plain func into it
            let filled_hole_func =
                AST::Produce(func.clone(), Box::new(hole_func.clone()), hole_props.clone());
            funcs_vec.push(filled_hole_func)
        }
    }

    // Now, we need to take these results and merge them back together
    // into a single high-level seq.
    if funcs_vec.len() == 0 {
        panic!("No funcs found!")
    } else if funcs_vec.len() == 1 {
        return funcs_vec[0].clone();
    } else {
        return AST::Sequence(funcs_vec);
    }
}

fn fill_holes(ast: &AST, var_order: &Vec<Var>) -> AST {
    match ast {
        AST::Produce(f, subast, props) =>
            AST::Produce(f.clone(), Box::new(fill_holes(subast, var_order)), props.clone()),
        AST::Consume(f) =>
            AST::Consume(f.clone()),
        AST::For(v, subast, range, prop) => {
            // need this variable to be the top-- otherwise the order is wrong!
            let new_variable = match v {
                VarOrHole::Var(v) => {
                    // check that we have a valid order
                    assert!(v.clone() == var_order[0].clone());
                    v.clone()
                },
                VarOrHole::Hole() => {
                    // fill the hole and recurse.
                    var_order[0].clone()
                },
            };

            // slice var_order
            let new_subast = fill_holes(subast, &var_order[1..].to_vec());
            AST::For(VarOrHole::Var(new_variable), Box::new(new_subast), range.clone(), prop.clone())
        },
        AST::Assign(f) =>
            AST::Assign(f.clone()),
        AST::StoreAt(f) =>
            AST::StoreAt(f.clone()),
        AST::Prefetch(buf, dim, stride) =>
            AST::Prefetch(buf.clone(), dim.clone(), stride.clone()),
        AST::Sequence(fs) => {
            // only fill on mainline
            let mut new_fs = Vec::new();
            for f in fs {
                if f.is_main_nest() {
                    new_fs.push(fill_holes(f, var_order))
                } else {
                    new_fs.push(f.clone());
                }
            }

            AST::Sequence(new_fs)
        }
        AST::StructuralHole(subast) => {
            // Get the first var from subast -- then fill until
            // we get to that var.  Note that when we support
            // fixed holes we'll need to deal with this differently.
            // Note that we'll have to do this differently once
            // variable holes are properly supported.
            let next_var = subast.get_next_main_var();
            let number_of_fixed_holes = subast.get_number_of_fixed_holes();
            let number_till_next_var = match next_var {
                None => var_order.len(),
                // get index of v in var_order
                Some(v) => {
                    // println!("{}", v.to_string());
                    // println!("{}", var_order.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "));
                    var_order.iter().position(|x| *x == v).unwrap()
                }
            };
            // convert i32 to usize
            let number_to_fill = number_till_next_var - (number_of_fixed_holes as usize);

            // Build the subast with the rest of the var_order
            // (sliced at number_to_fill)
            let mut new_subast = fill_holes(subast, &var_order[number_to_fill..].to_vec());

            // iterate from 0 to number_to_fill
            let mut new_order = (var_order[..number_to_fill]).to_vec();
            new_order.reverse();
            for var in new_order {
                new_subast = AST::For(VarOrHole::Var(var), Box::new(new_subast), ForRange::All(), vec![]);
            }

            new_subast
        }
    }
}

// walk through the ast and assert that there are no
// StructuralHole, or that all VarOrHole are Var not Hole.
pub fn assert_no_structural_holes(ast: &AST) {
    let has_holes = ast_has_holes(ast);
    if has_holes {
        println!("Tree {} has holes ", ast);
        panic!("")
    }
}

fn var_is_hole(v: &VarOrHole) -> bool  {
    match v {
        VarOrHole::Var(_) => false,
        VarOrHole::Hole() => true
    }
}

fn func_properties_has_holes(props: FuncProperty) -> bool {
    match props {
        FuncProperty::StoreOrder(vs) => {
            for v in vs {
                if var_is_hole(&v) {
                    return true;
                }
            }

            return false;
        }
        FuncProperty::Memoize() => false,
        FuncProperty::AllowRaceConditions() => false,
        FuncProperty::Async() => false,
    }
}

fn ast_has_holes(ast: &AST) -> bool {
    match ast {
        AST::Produce(_, ast, _props) => ast_has_holes(ast),
        AST::Consume(_) => false,
        AST::For(var_or_hole, ast, _, _) => {
            match var_or_hole {
                VarOrHole::Var(_) => ast_has_holes(ast),
                VarOrHole::Hole() => true
            }
        },
        AST::Assign(_) => false,
        AST::StoreAt(_) => false,
        AST::Prefetch(_buf, dim, _prefetch) => { 
			match dim {
				VarOrHole::Var(_) => false,
				// Holes in prefetch should really be
				// filled by the iterative synthesizer
				// rather than the structural synthesizer.
				// we should really return false here eventually.
				VarOrHole::Hole() => panic!("Unexpected hole in prefetch")
			}
		},
        AST::StructuralHole(_) => true,
        AST::Sequence(asts) => {
            for ast in asts {
                if ast_has_holes(ast) {
                    return true;
                }
            }

            return false;
        }
    }
}

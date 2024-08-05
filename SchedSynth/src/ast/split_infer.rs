/*
use crate::options::options::Options;
use std::collections::HashMap;
use std::collections::HashSet;
use crate::ast::ast::AST;
use crate::ast::ast::Range;
use crate::ast::ast::Var;

use crate::ast::analysis::range_table_for;
use crate::ast::analysis::func_table_for;
// use crate::ast::analysis::variable_tree;

use crate::utils::format::hashset_to_string;

pub fn find_splits_internal(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<Reshape> {
    // First, go through and find variables that are differnet between these two ASTs.
    let original_variables = variable_tree(original_ast);
    let target_variables = variable_tree(target_ast);

    // let func_table = function_table_for(original_ast);
    let range_table = range_table_for(opts, target_ast); // Get a lookup table that maps variables to ranges.
    let func_table = func_table_for(opts, target_ast); // Get a lookup table that maps variables to the names of their parent funcs.

    // We want to look through the variables for 'compound variables', which
    // are some variable of the form x.y.  x.y /either/ came from a fusing of
    // x and y, or it came from a splittig of x (in which case it has a partner, x.z)
    // We want to iterate until there are no compound variables.
    //
    // Assume that the original set has no compound variables.
    assert!(!has_compound_variables(&original_variables));
    
    // we work backwards towards the original_variables set.
    // Because we are working backwards, terminology is a bit
    // confusing here, but we try to maintain the forward
    // terminology (i.e. splitting means we want to end up
    // with a Reshape::Split)
    let mut remaining_variables = target_variables.clone();
    let mut reshapes = Vec::new();

    let mut changed = true;

    while has_compound_variables(&target_variables) {
        if !changed {
            panic!("Not finding solution to the fuse/split inference")
        }
        changed = false;

        // Check for any splittable variables.  We know that loops are splittable
        // if they are of they form X.y, X.z where 'X' may contain '.', but y and
        // z do not.  These came from the same loop, so split them.
        // We also need that the variables are: directly nested in each other,
        // and are not 
        let split = get_splittable(opts, compound_variables);
        match split {
            Some((from, to1, to2)) => {
                let reshape = split_into_reshape((from, to1, to2), range_table, func_table);
                reshapes.push(reshape);

                // apply_reshape_backwards(

                changed = true;
            }
            None => {
                // if we found no splits, also try to find merges.
            }
    };

    reshapes
}

// convert Vec<(Var, Var, Var)> into Split --- filling producer
// and factor fields with the range_table and func_table.
// using call to and map: split_into_reshape(split: (Var, Var, Var), range_table: &HashMap<Var, Range>, func_table: HashMap<Var, Var>) -> Reshape {
fn splits_into_reshapes(splits: Vec<(Var, Var, Var)>, range_table: &HashMap<Var, Range>, func_table:
    &HashMap<Var, Var>) -> Vec<Reshape> {
    splits.iter().map(|split| split_into_reshape(split, range_table, func_table)).collect()
}


fn split_into_reshape(split: (Var, Var, Var), range_table: &HashMap<Var, Range>, func_table: HashMap<Var, Var>) -> Reshape {
    let (split_from, split_into_1, split_into_2) = split;
    let factor = match range_table.get(&split_into_2) {
        Some(Range::Between(start, end)) => {
            end - start
        },
        _ => panic!("should have between range for split (Looked for variable {} and didn't
                find it)", split_into_2)
    };
    let func_var = func_table.get(&split_into_2).unwrap();
    Reshape::Split(func_var.clone(), split_from, (split_into_1, split_into_2), factor)
}

// convert Vec<(Var, Var, Var)> into Fuse --- filling producer
// and factor fields with the range_table and func_table.
fn fuses_into_reshapes(fuses: Vec<(Var, Var, Var)>, range_table: &HashMap<Var, Range>, func_table:
    &HashMap<Var, Var>) -> Vec<Reshape> {
    let mut reshapes = Vec::new();
    for (fuse_from_1, fuse_from_2, fuse_into) in fuses {
        let func = func_table.get(&fuse_into).unwrap(); // todo --- this will probably
        // crash if fuse_into is a partial var (like the result of a split)
        reshapes.push(Reshape::Fuse(func.clone(), (fuse_from_1, fuse_from_2), fuse_into));
    }
    reshapes
}

// Check for any splittable variables.  We know that loops are splittable
// if they are of they form X.y, X.z where 'X' may contain '.', but y and
// z do not.  These came from the same loop, so split them.
//
// We also require that loops are appropriately nested, so that
// we only unsplit loops that are actually next to each other and
// part of the same consumer.
fn get_splittable(opts: &Options, vars: VariableTree) -> Vec<(Var, Var, Var)> {
    let mut inserted: HashSet<Var> = HashSet::new(); // do not double-enter Vars

    get_splittable_internal(opts, vars, &inserted);
}

fn get_splittable_internal(opts: &Options, vars: VariableTree, merged: &HashSet<Var>) -> Option<(Var, Var, Var)> {
    match vars {
        Node(v, Node(v2, subtree)) => {
            // check if v and v2 are possible results of splits.
            // Get the longest prefixes:
            let x_prefix = get_longest_prefix(x.clone());
            let y_prefix = get_longest_prefix(y.clone());

            // If the prefixes are equal, then add these
            // to the vars set.
            if x_prefix == y_prefix && !inserted.contains(&x) && !inserted.contains(&y) {
                // split result

                if opts.debug_split {
                    println!("Foudn the following splits {} => {}, {}", x_prefix.into().clone(), x.clone(), y.clone());
                }
                Some((x_prefix.into().clone(), x.clone(), y.clone()))
            } else {
                // recurse and see if there is anything else in here.
                get_splittable_internal(opts, vars, merged)
            }
        },
        Leaf() => None
    }
}

fn print_varset(varse: &Vec<(Var, Var, Var)>) -> String {
 let mut s = String::new();
    for (x, y, z) in varse {
        s.push_str(&format!("({}, {}, {})\n", x, y, z));
    }
    s
}

// get all the characters before the last '.'.  If there is no
// last '.' panic.
fn get_longest_prefix(var: Var) -> String {
    match var.name.rfind('.') {
        Some(i) => var.name[..i].into(),
        None => panic!("No last '.' found"),
    }
}


pub enum VariableTree {
    Node(Var, Vec<VariableTree>),
    Leaf()
}
// check if any of the elements (var.name) of vars
// have '.' in them
fn has_compound_variables(vars: &VaraibleTree) -> bool {
    match vars {
        VariableTree::Node(var, children) => {
            if var.name.contains('.') {
                true
            } else {
                children.iter().any(has_compound_variables)
            }
        }
        VariableTree::Leaf() => false
    }
}

// get the set of variables that contain '.'
fn get_compound_variables(vars: &HashSet<Var>) -> HashSet<Var> {
    let mut compound_vars = HashSet::new();

    for var in vars {
        if var.name.contains('.') {
            compound_vars.insert(var.clone());
        }
    }

    compound_vars
}

// Apply each of the reshapes into the variables set --- splitting or merging
// variables as dictated
fn apply_reshapes(reshapes: Vec<Reshape>, variables: HashSet<Var>) -> HashSet<Var> {
    let mut new_variables = variables.clone();
    for reshape in reshapes {
        match reshape {
            Reshape::Split(producer, split_var, (var1, var2), factor) => {
                new_variables.remove(&split_var);
                new_variables.insert(var1);
                new_variables.insert(var2);
            },
            Reshape::Fuse(producer, (var1, var2), fused_var) => {
                new_variables.remove(&var1);
                new_variables.remove(&var2);
                new_variables.insert(fused_var);
            }
        }
    }
    new_variables
}

// Apply each of the reshapes into the variables set backwards
// --- unsplitting or unfusing variables as dictated
// (i.e. if we have a fuse, we un-fuse the variables in the set)
fn apply_reshapes_backwards(reshapes: Vec<Reshape>, variables: HashSet<Var>) -> HashSet<Var> {
 let mut new_variables = variables.clone();
    for reshape in reshapes {
        match reshape {
            Reshape::Split(producer, split_var, (var1, var2), factor) => {
                new_variables.remove(&var1);
                new_variables.remove(&var2);
                new_variables.insert(split_var);
            },
            Reshape::Fuse(producer, (var1, var2), fused_var) => {
                new_variables.remove(&fused_var);
                new_variables.insert(var1);
                new_variables.insert(var2);
            }
        }
    }
    new_variables
}

// Figure out which variables should be split to create variables_to_create
fn induce_splits(opts: &Options, range_table: HashMap<Var, Range>, func_table: HashMap<Var, Var>, variables_from: HashSet<Var>, variables_to_create: HashSet<Var>) -> Vec<Reshape> { 
    induce_splits_internal(opts, variables_from, variables_to_create)
        .iter()
        // TODO -- need to get the correct func.
        .map(|spl| {
            // We get the split factor by the range of the inner loop --- that's
            // what Halide will tile into.
            let split_factor = split_factor_from_range(range_table.get(&spl.1.1));
            if opts.debug_split {
                println!("Building split: {} -> ({}, {}) with factor {}", spl.0.clone(), spl.1.0.clone(), spl.1.1.clone(), split_factor);
            }
            // Load the func if we can --- this func table is derived
            // from the target structure, so we use the target variables.
            // If the variable spl.1.1 was synthesized then this will fail---
            // will need to find some temp function name to use I guess in that case?
            // depends what call we  want to output --- we have to split under
            // some func.
            //
            // One alternative here would be to use the func_table to get a list
            // of parenst --- and split under each parent.  Of course, in that
            // case, this whole function needs to updated to track splitting in
            // compute-ats.  --- Perhaps it should actualy be that we call this
            // on all the funcs that share these variables?
            let func = match func_table.get(&spl.1.1) {
                None => panic!("Error: found no parent func for variable {}", &spl.1.1.clone()),
                Some(v) => v
            };
            Reshape::Split(func.clone(), spl.0.clone(), spl.1.clone(), split_factor)
        })
        .collect()
}


fn induce_splits_internal(opts: &Options, variables_from: HashSet<Var>, variables_to_create: HashSet<Var>) -> HashMap<Var, (Var, Var)> {
    if opts.debug_split {
        println!("Building variables {} from variables {}", hashset_to_string(&variables_to_create), hashset_to_string(&variables_from));
    }

    // iterate over variables_to_create --- using the odds_variable_is_split_from(v1, v2) to 
    // determine to put v2 into a vec of candidates for v1 (sorted by the odds)
    let mut candidates: HashMap<Var, Vec<(Var, f32)>> = HashMap::new();
    for v1 in &variables_from {
        for v2 in &variables_to_create {
            let odds = odds_variable_is_split_from(v2, v1);
            if opts.debug_split {
                println!("Odds {} is split from {} is {}", v1, v2, odds);
            }
            if odds > 0.0 {
                let entry = candidates.entry(v1.clone()).or_insert(Vec::new());
                entry.push((v2.clone(), odds));
            }
        }
    }

    // insert two temp variables --- with odds 0.5 --- these will later
    // have to be fused.

    // sort the candidates by odds
    for (_, v) in candidates.iter_mut() {
        v.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    }
    
    // Now we need to go through candidates, finding the most
    // likely overall pairings: need each vec to be exactly two elements
    // long --- and no variable to appear in any vec more than once.
    // We use a greedy approach to this here, but should really
    // do a more co-optimized approach.
    let mut splits: HashMap<Var, (Var, Var)> = HashMap::new();
    let mut used_variables: HashSet<Var> = HashSet::new();
    for (split, intos) in candidates.iter() {
        // We need each into variable to appear exactly once.
        let mut into_vars = Vec::new();

        for (invar, _likelyhood) in intos {
            if used_variables.contains(invar) {
                // skip this variable --- already used.
            } else if into_vars.len() < 2 {
                // TODO -- order probably matters here and we should
                // probably be infering the order of variables
                // somwhere.
                into_vars.push(invar);
                used_variables.insert(invar.clone());
            }
        };

        if into_vars.len() == 2 {
            if opts.debug_split {
                println!("Splitting {} into {} and {}", split, into_vars[0], into_vars[1]);
            }
            splits.insert(split.clone(), (into_vars[0].clone(), into_vars[1].clone()));
        } else {
            // Not every variable has to be split /from/.
            if opts.debug_split {
                println!("Variable {} is not being split because only {} candidates were found to split into (2 required)", split, into_vars.len());
            }
        }
    }

    // compute that variables that we were unable to find
    // assignments for:
    let unassigned_variables: HashSet<Var> = variables_to_create.difference(&used_variables).cloned().collect();

    // There are no more variables to be split into 
    if unassigned_variables.len() == 0 {
        splits
    } else {
        // There are still variables that we have to split into. --- recurse and do this.
        if unassigned_variables.len() == variables_to_create.len() {
            // in this call, we didn't manage to split into any of the
            // variables that we wanted to split into --- we are making
            // no progress, so panic.
            // 
            // This panic means that we are guaranteed to terminate as long
            // as unassigned_variables is of finite size.
            panic!("Making no progress creating variables {}", hashset_to_string(&variables_to_create));
        } else {
            // we made progress so recurse and see.
            let new_split_variables = variables_to_create.union(&used_variables).cloned().collect();

            induce_splits_internal(opts, new_split_variables, unassigned_variables)
        }
    }
}


// This is a heuristic function that gives a boolean
// guessing where a variable was split from --- it should probably
// eventually return a float 0-1 to give a rangking
fn odds_variable_is_split_from(variable: &Var, from: &Var) -> f32 {
    // check if from.name is a substring in variable.name
    let from_name: &String = &from.name;
    let variable_name = &variable.name;

    // TODO --- do a better job at this. --- need to properly support the 'dot' notation
    // used in hlaide
    if variable_name.contains(from_name) {
        return 1.0;
    } else {
        return 0.0;
    }
}

// Get all the Vars in the AST by recursively
// walkthing through it.
fn variables(ast: &AST) -> HashSet<Var> {
 let mut vars = HashSet::new();
    match ast {
        AST::Produce(_var, ast, _props) => {
            // dont include func vars
            vars.extend(variables(&*ast));
        },
        AST::Consume(_var) => {
            // dont include func vars
        },
        AST::For(var, ast, _range, _properties) => {
            vars.insert(var.clone());
            vars.extend(variables(&*ast));
        },
        AST::Assign(_var) => {
            // this is a func var
        },
        AST::Sequence(asts) => {
            for ast in asts {
                vars.extend(variables(&ast));
            }
        }
    }
    vars
}

// get a factor to use in split from the Range --- if unknown we
// return 0 --- otherwise it should be the size of the stride
// in between (i.e. top - bottom)
fn split_factor_from_range(range: Option<&Range>) -> i32 {
    match range {
        Some(Range::Between(bottom, top)) => top - bottom,
        _ => 0 // TODO -- I'm not really sure what split does with
               // zeroes --- should we return something like -1 to
               // make it more clear thta this si unexepcted? (Is it?)
    }
}
*/

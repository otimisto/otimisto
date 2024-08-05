use crate::options::options::Options;
use std::collections::HashMap;
use std::collections::HashSet;
use crate::ast::ast::AST;
use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::HoleOptionTrait;
use crate::reshape::reshape::Reshape;
use crate::reshape::reshape::inject_reorders;

// We want to walk through each AST, and record
// the order that Vars are mentioned in (see get_orders below) --- this is
//  be stored in a vec for the original and then a different
//  vec for the target. 
//  
//  once we have the two lists, we look for places
//  where the vars are in different orders, and return
//  each pair of those
pub fn get_reorders_internal(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<(Func, Var, Var)> {
    let mut original_order = HashMap::<Func, Vec<Var>>::new();
    let mut target_order = HashMap::<Func, Vec<Var>>::new();
    get_orders(opts, &original_ast, &None, &mut original_order);
    get_orders(opts, &target_ast, &None, &mut target_order);

    // For each key in the hash maps (should be the same keys)
    // (each is a producer), we have a vec that is the order in whic
    // variables are used in that producer.  Inherently, this gives
    // a relation X <- Y (X comes before Y, meaning that X appears
    // before Y).  We want to go through the original_orders and
    // target_orders and find all the X, Y such that X <- Y in original_orders
    // and Y <- X in target_orders.
    let mut reversed_orders = Vec::new();
    for (producer, original_order) in &original_order {
        if opts.debug_reorder {
            println!("Looking for reorders in producer {}", producer);
        }

        let target_order = &target_order[producer];
        let empty_set = HashSet::new(); // declare this out here so it outlives
                                        // the closure below, which was cuasing
                                        // problems.

        let original_comes_before: HashMap<Var, HashSet<Var>> = build_comes_before_map(original_order);
        let target_comes_before: HashMap<Var, HashSet<Var>> = build_comes_before_map(target_order);

        if opts.debug_reorder {
            // print the hashmaps in a readable way
            println!("original_comes_before:");
            for (var, set) in &original_comes_before {
                print!("{}: ", var);
                for var2 in set {
                    print!("{} ", var2);
                }
                println!();
            }
            println!("target_comes_before:");
            for (var, set) in &target_comes_before {
                print!("{}: ", var);
                for var2 in set {
                    print!("{} ", var2);
                }
                println!();
            }
        }

        // So each of these now represent the <- relation.  Go through and find
        // any differences between the sets.
        // Note that absence is not a difference --- e.g. x in original[v]
        // and x not in target[v].  We are looking for all the x such
        // that x in original[v] and v in target[x].
        let mut differences: Vec<(Func, Var, Var)> = original_comes_before
            .iter()
            .flat_map(|(v, original_v)| {
                original_v
                    .iter()
					// v -> x is a relation.
					// Check if x -> v is also a relation
                    .filter(|x| {
						// get the x map
						let xmap = target_comes_before.get(x).unwrap_or(&empty_set);
						let res = xmap.contains(v); // if this is true, then we have that this was in here
							// both forwards and backwards.
						if opts.debug_reorder {
							println!("{} -> {} exists, checking if it js violated: {}", v.clone(), x, res);
						};
						res
					})
                    .map(move |x| (producer.clone(), v.clone(), x.clone()))
            })
            .collect::<Vec<_>>().to_vec();
		if opts.debug_reorder {
			println!("Result map is of size {}", differences.len());
		};

        reversed_orders.append(&mut differences)
    }

	if opts.debug_reorder {
		println!("In total, have {} reorders", reversed_orders.len());
	};

    // Now, sort these re-orders so they are in the correct
    // order to be applied -- Halide is pretty flexible
    // in this regard, but it struggles if we give it
    // the reorders in /any/ order.
    topo_sort(opts, original_ast, reversed_orders)
}

fn topo_sort(opts: &Options, ast: &AST, orders: Vec<(Func, Var, Var)>) -> Vec<(Func, Var, Var)> {
    let mut original_order = HashMap::<Func, Vec<Var>>::new();
    get_orders(opts, &ast, &None, &mut original_order);

    // Go by func:
    let mut by_func = HashMap::<Func, HashSet::<(Var, Var)>>::new();
    build_table_by_func(opts, &mut by_func, &orders);

    let mut result_vec = Vec::new();

    // iterate over the by_func table
    for (func, func_reorders) in by_func.iter() {
        let mut reorders = func_reorders.clone();
        let mut order = original_order[func].clone();
        let mut removed_last_time = true;

        while reorders.len() > 0 {
            if !removed_last_time {
                panic!("Not making progress");
            }
            removed_last_time = false;

            // iterate over reorders by index
            for (_i, (v1, v2)) in reorders.clone().iter().enumerate() {
                let mut last_matched = false;
                let mut removed_reorder = false; // when this gets set to true, have to remove it.

                if opts.debug_reorder_topo {
                    println!("Trying to schedule reorder {}, {} (func {})", v1.clone(), v2.clone(), func.clone());
                    println!("Order is {:?}", order.clone());
                }

                let mut v1_index = 0;
                for (j, v) in order.clone().iter().enumerate() {
                    if last_matched && v == v2 {
                        let v2_index = j;
                        // We got v1, v2 in order --- this is the swap
                        // that we can do next.
                        result_vec.push((func.clone(), v1.clone(), v2.clone()));
                        // remove i from the reorders list
                        reorders.remove(&(v1.clone(), v2.clone()));
                        removed_reorder = true;

                        // also need to apply this reorder :)
                        let temp = order[v1_index].clone();
                        order[v1_index] = order[v2_index].clone();
                        order[v2_index] = temp;
                        break;
                    }

                    if v == v1 {
                        last_matched = true;
                        v1_index = j;
                    } else {
                        last_matched = false;
                    }
                }

                if removed_reorder {
                    if opts.debug_reorder_topo {
                        println!("Scheduled Successfully");
                    }
                    // indexes changed --- need to reset iterator.
                    removed_last_time = true;
                    break;
                }
            }
        }
    }

    result_vec
}

fn build_table_by_func(_opts: &Options, table: &mut HashMap<Func, HashSet::<(Var, Var)>>, orders: &Vec<(Func, Var, Var)>) {
    for (func, var1, var2) in orders {
        let order = table.entry(func.clone()).or_insert(HashSet::new());
        order.insert((var1.clone(), var2.clone()));
    }
}

// Reutrn a map from each var in the input to all of the vars
// that follow it in the list.
fn build_comes_before_map(input: &Vec<Var>) -> HashMap<Var, HashSet<Var>> {
    let mut comes_before_map = HashMap::new();
    for i in 1..input.len() {
        let (before, after) = input.split_at(i);
        // get last element -- then we set the hashmap to the after
        let var = before.last().unwrap_or_else(|| panic!("Must have a list element")).clone();

        // convert the after vector to a set
        let after_set: HashSet<Var> = after.iter().map(|x| x.clone()).into_iter().collect();

        comes_before_map.insert(var, after_set);
    }
    comes_before_map
}

// Walk through the AST and record the order of vars
// put in a table entry for each producer.
fn get_orders(opts: &Options, ast: &AST, current_producer: &Option<Func>,
        orders: &mut HashMap<Func, Vec<Var>>) {
    match ast {
        AST::Produce(var, ast, _props) => {
            // into a new producer --- this gets a new var ordering.
            // note that if this is a compute-at producer it probably is
            // going to get the ordering of the original producer.
            // (i.e. this will probably be buggy).
            get_orders(opts, ast, &Some(var.clone()), orders);
        },
        AST::Consume(_var) => {
            // I don't think that a consume changes what the
            // current producer should be but mayb there are
            // cases where it should?
        },
        AST::For(var, ast, _range, _properties) => {
            // insert var.clone() into the vec in the orders map at the current_producer.
            // if current_producer is None use "_top_level".  If orders hashmap doens't
            // have current_producer, then create a new singleton vec for it.
            let producer_name = current_producer.clone().unwrap_or(Func{ name: "_top_level".to_string(), update: None });
            let order_vec = orders.entry(producer_name).or_insert(vec![]);
            order_vec.push(var.get().unwrap());
            
            // recurse
            get_orders(opts, ast, current_producer, orders);
        },
        AST::Assign(_var) | AST::StoreAt(_var) => {
            // I think we don't need to do anything here.
        },
        AST::Prefetch(_, _var, _)  => {
            // Also don't think anything needs to happen
        }
        AST::StructuralHole(ast) => {
            get_orders(opts, ast, current_producer, orders);
        }
        AST::Sequence(asts) => {
            for ast in asts {
                get_orders(opts, ast, current_producer, orders);
            }
        }
    }
}


pub fn insert_reorders_internal(opts: &Options, reshapes: &Vec<Reshape>, original_ast: &AST, _target_ast: &AST) -> Vec<Reshape> {
    inject_reorders(opts, original_ast, reshapes)
}

use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::fs;

use crate::options::options::Options;
use crate::shared::range_set::IntegerRangeSet;
use crate::shared::range_set::AnyIntegerSet;
use crate::shared::range_set::set_from_name;

#[derive(Parser)]
#[grammar = "sketch_parse/grammar.pest"]
struct LoopParser;

#[derive(Clone)]
pub struct Variable {
    pub name: String,
    pub hole: bool
}


#[derive(Clone)]
pub enum ForRangeAST {
    Between(ASTNumberOrHole, ASTNumberOrHole), // start, end
    All()
}

#[derive(Clone)]
pub enum SketchAST { // nodes have nesting, <other stuff>
    Produce(i32, Variable, Box<SketchAST>, Vec<ASTFuncProperty>), // name, contents, properties
    Consume(i32, Variable), // name
    For(i32, Variable, Box<SketchAST>, ForRangeAST, Vec<ASTLoopProperty>), // variable name, sub-contents, optional range
    Assign(i32, Variable), // variable name
    StoreAt(i32, Variable), // variable name
    Sequence(i32, Vec<SketchAST>), // list of sub-asts
	Prefetch(i32, Variable, Variable, ASTNumberOrHole),
    StructuralHole(i32, Box<SketchAST>), // optional sub-asts.
    Property(i32, ASTFuncProperty),
}

#[derive(Clone)]
pub enum ASTLoopProperty {
    Vectorize(),
    Parallel(),
    Unroll(ASTNumberOrHole),
    Fuse(Variable)
}

#[derive(Clone)]
pub enum ASTFuncProperty {
    StoreOrder(Vec<Variable>), // Store order
    Memoize(), // memoize
    AllowRaceConditions(),
    Async(),
}

#[derive(Clone)]
pub enum ASTNumberOrHole {
    Number(i32),
    Hole(IntegerRangeSet)
}

// Trait for SketchAST.
trait AST {
    fn children(&self) -> Vec<SketchAST>;
    fn node_type(&self) -> String;
    fn size(&self) -> i32;
}

impl std::fmt::Display for ASTNumberOrHole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNumberOrHole::Number(n) => write!(f, "{}", n),
            ASTNumberOrHole::Hole(r) => write!(f, "{}", r)
        }
    }
}
impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for ASTFuncProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ASTFuncProperty::StoreOrder(order) => write!(f, "Store order: {:}", order.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
            ASTFuncProperty::Memoize() => write!(f, "Memoize"),
            ASTFuncProperty::AllowRaceConditions() => write!(f, "AllowRaceConditions"),
            ASTFuncProperty::Async() => write!(f, "Async"),
        }
    }
}

impl ToString for ASTLoopProperty {
 fn to_string(&self) -> String {
        match self {
            ASTLoopProperty::Vectorize() => "vectorize".to_string(),
            ASTLoopProperty::Parallel() => "parallel".to_string(),
            ASTLoopProperty::Unroll(n) => format!("unroll({})", n),
            ASTLoopProperty::Fuse(v) => format!("fuse({})", v),
        }
    }
}

impl ToString for ForRangeAST {
    fn to_string(&self) -> String {
        match self {
            ForRangeAST::Between(start, end) => format!("{}..{}", start, end),
            ForRangeAST::All() => String::from("all"),
        }
    }
}

impl ToString for SketchAST {
    fn to_string(&self) -> String {
        match self {
            SketchAST::Produce(_, n, subelts, properties) => format!("Produce {} ({}): {}", n.to_string().clone(),
            subelts.to_string(), properties.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ").clone()),
            SketchAST::Consume(_, n) => format!("Consume {}", n.to_string().clone()),
            SketchAST::For(_, n, subelts, range, properties) => {
                let properties_string = 
                    properties.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
                format!("For({}) {} in {}: ({})", properties_string,
                n.to_string().clone(), range.to_string(), subelts.to_string())
            },
            SketchAST::Assign(_, n) => format!("compute {}", n.to_string().clone()),
            SketchAST::StoreAt(_, n) => format!("store {} here", n.to_string().clone()),
            SketchAST::Prefetch(_, buf, dim, stride) => format!("prefetch {} at {} (stride {})",
			buf.to_string(), dim.to_string(), stride.to_string()),
            SketchAST::Sequence(_, subvars) => format!("Sequence({})", subvars.iter().map(|x|
                    x.to_string()).collect::<Vec<String>>().join(",")),
            SketchAST::StructuralHole(_, subvar) => format!("StructuralHole({})", subvar.to_string()),
            SketchAST::Property(_, property) => format!("Func Property:({})", property.to_string()),
        }
    }
}

impl AST for SketchAST {
    fn children(&self) -> Vec<SketchAST> {
        match self {
            SketchAST::Produce(_nest, _n, children, _properties) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::Consume(_nest, _n) => {
                vec![]
            },
            SketchAST::For(_nest, _n, children, _range, _properties) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::Assign(_nest, _n) => {
                let res = Vec::new();
                res
            },
            SketchAST::StoreAt(_, _n) => {
                vec![]
            },
            SketchAST::Prefetch(_, _buf, _dim, _stride) => {
                vec![]
            }
            SketchAST::StructuralHole(_nest, child) => vec![child.as_ref().clone()],
            SketchAST::Sequence(_nest, children) => children.to_vec(),
            SketchAST::Property(_nest, _prop) => vec![]
        }
    }

    fn node_type(&self) -> String {
        match self {
            SketchAST::Produce(_, _, _, _) => "Produce".into(),
            SketchAST::Consume(_, _) => "Consume".into(),
            SketchAST::For(_, _, _, _, _) => "For".into(),
            SketchAST::Assign(_, _) => "Assign".into(),
            SketchAST::StoreAt(_, _) => "StoreAt".into(),
            SketchAST::Prefetch(_, _, _, _) => "Prefetch".into(),
            SketchAST::StructuralHole(_, _) => "StructuralHole".into(),
            SketchAST::Sequence(_, _) => "Sequence".into(),
            SketchAST::Property(_, _) => "Property".into(),
        }
    }

    fn size(&self) -> i32 {
        match self {
            SketchAST::Produce(_, _, child, _) => child.size() + 1,
            SketchAST::Consume(_, _) => 1,
            SketchAST::For(_, _, child, _, _) => child.size() + 1,
            SketchAST::Assign(_, _) => 1,
            SketchAST::StructuralHole(_, child) => child.size() + 1,
            SketchAST::Sequence(_, children) => children.iter().map(|child| child.size()).sum(),
            SketchAST::StoreAt(_, _) => 1,
            SketchAST::Prefetch(_, _, _, _) => 1,
            SketchAST::Property(_, _) => 1
        }
    }
}

fn process_ident_list(opts: &Options, sequence: Pair<Rule>) -> Vec<Variable> {
    if opts.debug_parser {
        println!("Parsing ident list|");
    }
    match sequence.as_rule() {
        Rule::ident_list => {
            let mut inner = sequence.into_inner();

            if inner.len() == 1 {
                // just an ident
                process_ident_list(opts, inner.next().unwrap())
            } else {
                // an ident list
                let mut list_head = process_ident_list(opts, inner.next().unwrap());
                let _ = inner.next(); // whitespace
                let _ = inner.next(); // whitespace
                let tail_list = process_ident_list(opts, inner.next().unwrap());

                // prepend list_head to the tail_list vec
                list_head.extend(tail_list);
                list_head
            }
        },
        Rule::ident_list_spaces => {
            let mut inner = sequence.into_inner();

            if inner.len() == 1 {
                // just an ident
                process_ident_list(opts, inner.next().unwrap())
            } else {
                let mut list_head = process_ident_list(opts, inner.next().unwrap());
                let _ = inner.next(); // whitespace
                let tail_list = process_ident_list(opts, inner.next().unwrap());

                list_head.extend(tail_list);
                list_head
            }
        }
        Rule::ident => {
            vec![process_ident(opts, sequence)]
        }
        _ => panic!("Not an ident list")
    }
}

fn process_ident(opts: &Options, sequence: Pair<Rule>) -> Variable {
    match sequence.as_rule() {
        Rule::ident => {
            if opts.debug_parser {
                println!("Got an ident");
            }
            let name = sequence.as_str();
            Variable{name: name.into(), hole: false}
        },
        Rule::ident_or_hole => {
            if opts.debug_parser {
                println!("Got an ident or hole");
            }
            
            let name = sequence.as_str();
            if name == "??" {
                Variable{name: name.into(), hole: true} // TODO -- need to return a hole here.
            } else {
                Variable{name: name.into(), hole: false}
            }

        }
        _ => panic!("Unable to process non-ident sequence '{}' into variable", sequence.as_str())
    }
}

fn process_number(opts: &Options, num: Pair<Rule>) -> ASTNumberOrHole {
    match num.as_rule() {
        Rule::number => {
            let num_str = num.as_str();
            let num_val: i32 = num_str.parse().unwrap();
            ASTNumberOrHole::Number(num_val)
        },
		Rule::number_or_hole => {
			let mut inner = num.clone().into_inner();

			if inner.len() > 1 {
				// This is a number_set_hole
                // TODO --- we want to support manually-specified sets
                // eventually. we need to support that.
                let _ = inner.next();
                let set_name = process_ident(opts, inner.next().unwrap());
                let _ = inner.next();

                ASTNumberOrHole::Hole(set_from_name(set_name.name))
			} else {
				let num_str = num.as_str();
				if num_str == "??" {
					ASTNumberOrHole::Hole(AnyIntegerSet())
				} else {
					// Recurse on the inner --- it is a number
					// type
					process_number(opts, inner.next().unwrap())
				}
			}
		}
        _ => panic!("not a number {}", num.as_str())
    }
}

fn process_stride(opts: &Options, sequence: Pair<Rule>) -> ASTNumberOrHole {
	match sequence.as_rule() {
		Rule::optional_stride => {
			let mut inner = sequence.into_inner();
			
			if inner.len() > 0 {
				let mut stride = inner.next().unwrap().into_inner();

				let _ = stride.next(); // whitespace
				let _ = stride.next(); // whitespace
				let stride_value = process_number(opts, stride.next().unwrap());

                stride_value
			} else {
				// don't have a stride
				// default stride to 1
				ASTNumberOrHole::Number(1)
			}
		},
		_ => { panic!("Unexpected non-stride!"); }
	}
}

fn process_range(opts: &Options, sequence: Pair<Rule>) -> ForRangeAST {
    match sequence.as_rule() {
        Rule::optional_range => {
            // whitespace
            let mut inner = sequence.into_inner();

            // we have a rnage
            if inner.len() > 0 {
                // range
                let mut range = inner.next().unwrap().into_inner();

                // println!("Range is {}", range);
                let _ = range.next(); // whitespace
                //let _ = range.next(); // in
                let _ = range.next(); // whitespace
                //let _ = range.next(); // [
                let start = process_number(opts, range.next().unwrap());
                let _ = range.next(); // whitespace
                // let _ = range.next(); // ,
                let _ = range.next(); // whitespace
                let end = process_number(opts, range.next().unwrap());
                let res = ForRangeAST::Between(start, end);

                if opts.debug_parser {
                    println!("Parsed a range of {}", res.to_string());
                }

                res
            } else {
                if opts.debug_parser {
                    println!("Parsed a range of all") 
                }
                // no range
                ForRangeAST::All()
            }
        },
        _ => panic!("Must pass optinal_range to process_range")
    }
}

fn process_fuses(opts: &Options, sequence: Pair<Rule>) -> Vec<ASTLoopProperty> {
    if opts.debug_parser {
        println!("Processing a fuse property, {}", sequence.as_str());
    }

    match sequence.as_rule() {
        Rule::optional_fuse => {
            let mut inner = sequence.into_inner();

            if inner.len() == 0 {
                // nothing
                vec![]
            } else {
                // have an existing fuse inner.
                let mut fuse_inner = inner.next().unwrap().into_inner();
                let _ = fuse_inner.next(); // whitespace
                let _ = fuse_inner.next(); // whitespace
                let _ = fuse_inner.next(); // whitespace
                let ident_list = fuse_inner.next().unwrap();

                let idents = process_ident_list(opts, ident_list);
                // map idents into ASTLoopProperty::Fuse(
                let idents = idents.into_iter().map(|ident| {
                    ASTLoopProperty::Fuse(ident)
                }).collect();
                idents
            }
        },
        _ => panic!("Unexpected non-fuse rule to process_fuses: {:?}", sequence.as_rule()),
    }
}

fn process(opts: &Options, nesting_depth: i32, sequence: Pair<Rule>) -> SketchAST {
    if opts.debug_parser {
        println!("Processing '{}'", sequence.as_str());
    }
    match sequence.as_rule() {
        Rule::sequence_list => {
            if opts.debug_parser {
                println!("Got a sequcnce list");
            }
            let mut inner = sequence.into_inner();
            // For a sequence, get the head, then go through
            // and get the tail.
            // Note that this produces a very flat list --- it doesn't consider
            // how indented things are --- that is handled in a secondary pass.
            let seq = process(opts, nesting_depth, inner.next().unwrap());
            let _ = inner.next().unwrap(); // This is whitespace in both rules.
            let tail = inner.next().unwrap();
            let has_tail = match tail.as_rule() {
                Rule::sequence_list => // This is the first case for sequence_list
                    true,
                Rule::EOI => // This is the econd case of sequence_list
                    false,
                _ => panic!("Unexepced last token '{}'", tail)
            };
            if has_tail {
                // no increase in nesting depth because seq is just a sequence --it's explicit from
                // the indentation anyway.
                let rest = process(opts, nesting_depth, tail); // next seq list
                let mut existing_children = rest.children();
                existing_children.insert(0, seq);
                SketchAST::Sequence(nesting_depth, existing_children)
            } else {
                // just sequnce, EOI
                let mut res_vec = Vec::new();
                res_vec.push(seq);
                SketchAST::Sequence(nesting_depth, res_vec)
            }
        },
        Rule::sequence => {
            if opts.debug_parser {
                println!("Got a sequence");
            }
            let mut inner = sequence.into_inner();
            if inner.len() == 1 {
                // empty line
                SketchAST::Sequence(nesting_depth, vec![])
            } else {
                assert!(inner.len() == 2);

                let nesting = inner.next().unwrap();
                let new_nesting_depth: i32 = nesting.into_inner().len() as i32; // Get the depth of the nest

                // nesting despth is explitict from the indentation.
                let stmt = process(opts, new_nesting_depth, inner.next().unwrap());
                stmt
            }
        },
        Rule::produce => {
            if opts.debug_parser {
                println!("Got a produce");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // produce
                                       // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::Produce(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                Vec::new())
        },
		Rule::consume => {
			if opts.debug_parser {
                println!("Got a consume");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // consume
		    // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::Consume(nesting_depth, ident)
        },
        Rule::pfor => {
            if opts.debug_parser {
                println!("Got a for ({})", sequence.as_str());
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // for
                                       // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());
            let fuses = process_fuses(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range, fuses)
        },
        Rule::structure_hole => {
            if opts.debug_parser {
                println!("Got a structural hole");
            }

            SketchAST::StructuralHole(nesting_depth,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        }
        Rule::assignment => {
            if opts.debug_parser {
                println!("Got an assignment");
            }

            let name = sequence.as_str();
            if name == "??" {
                // Structure hole --- note that doesn't just
                // have to be a compute.
                // Implicitly, this is the same as
                // a compute nested within a ??.  So it's just syntactic
                // sugar for:
                // ??:
                //   compute
                SketchAST::StructuralHole(nesting_depth,
                    Box::new(SketchAST::Assign(nesting_depth, Variable { name: "Inferred".to_string(), hole: false} )))
            } else {
                // I think the name of the assign isn't actually
                // used anywhere relevant.
                SketchAST::Assign(nesting_depth, Variable{ name: "Inferred".to_string(), hole: false })
            }
        }
        Rule::store_at => {
            if opts.debug_parser {
                println!("Got a store_at");
            }
            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            SketchAST::StoreAt(nesting_depth, ident)
        }
        Rule::vectorize => {
            if opts.debug_parser {
                println!("Got a vectorize");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace token
            // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());
            let mut fuses = process_fuses(opts, inner.next().unwrap());
            fuses.extend(vec![ASTLoopProperty::Vectorize()]);

            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range, vec![ASTLoopProperty::Vectorize()])
        }
        Rule::parallel => {
            if opts.debug_parser {
                println!("Got a parallel");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace token
            // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());
            let mut fuses = process_fuses(opts, inner.next().unwrap());
            fuses.extend(vec![ASTLoopProperty::Parallel()]);

            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range, fuses)
        }
        Rule::unroll => {
            if opts.debug_parser {
                println!("Got an unroll");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace token
            // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace token
            let amount = process_number(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace token
            let mut fuses = process_fuses(opts, inner.next().unwrap());
            fuses.extend(vec![ASTLoopProperty::Unroll(amount)]);

            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range, fuses)
        }
		Rule::prefetch => {
			if opts.debug_parser {
				println!("Got prefetch");
			}

			let mut inner = sequence.into_inner();
			let _ = inner.next(); // whitespace
			let buffer_id = process_ident(opts, inner.next().unwrap()); // this one can't be a hole
			// -- dont have the info to fill it.
			let _ = inner.next(); // whitespace
			let _ = inner.next(); // whitespace
			let dimension_id = process_ident(opts, inner.next().unwrap());
			let stride = process_stride(opts, inner.next().unwrap());

			SketchAST::Prefetch(nesting_depth, buffer_id, dimension_id, stride)
		},
        Rule::ordering => {
            if opts.debug_parser {
                println!("Got ordering");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace
            let orders = process_ident_list(opts, inner.next().unwrap());

            SketchAST::Property(nesting_depth, ASTFuncProperty::StoreOrder(orders))
        },
        Rule::simple_property => {
            if opts.debug_parser {
                println!("Got a simple property");
            }

            let name = sequence.as_str();
            match name {
                "memoize" =>
                    SketchAST::Property(nesting_depth, ASTFuncProperty::Memoize()),
                "allow race conditions" =>
                    SketchAST::Property(nesting_depth, ASTFuncProperty::AllowRaceConditions()),
                "async" =>
                    SketchAST::Property(nesting_depth, ASTFuncProperty::Async()),
                _ => panic!("Unknown simple property {}", name)
            }

        },
        Rule::EOI => {
            if opts.debug_parser {
                println!("Got an EOI");
            }
            SketchAST::Sequence(nesting_depth, Vec::new())
        }
        Rule::indent_nest => {
            panic!("Unreachable?")
        },
        Rule::newline => panic!("Got newline"),
        Rule::whitespace_plus => panic!("whitespace_plus"),
        Rule::whitespace => panic!("whitespace"),
        _ => {
            panic!("Unexpected rule {}", sequence.as_str())
        }
    }
}

// Get the nesting depth
fn get_nest_depth(v: &SketchAST) -> &i32 {
    match v {
        SketchAST::Produce(n, _, _, _) => n,
        SketchAST::Consume(n, _) => n,
        SketchAST::For(n, _, _, _, _) => n,
        SketchAST::Assign(n, _) => n,
        SketchAST::Property(n, _) => n,
        SketchAST::Sequence(n, _) => n,
        SketchAST::StructuralHole(n, _) => n,
        SketchAST::StoreAt(n, _) => n,
        SketchAST::Prefetch(n, _, _, _) => n,
    }
}

// set the loop nest contents
fn set_nest(v: &SketchAST, nest: SketchAST) -> SketchAST {
    if nest.size() == 0 {
        v.clone()
    } else {
        match v {
            SketchAST::Produce(n, var, current_nest, properties) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::Produce(n.clone(), var.clone(), Box::new(nest), properties.clone())
            },
            SketchAST::Consume(n, var) => {
                SketchAST::Consume(n.clone(), var.clone())
            },
            SketchAST::For(n, var, current_nest, range, properties) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::For(n.clone(), var.clone(), Box::new(nest), range.clone(), properties.clone())
            },
            SketchAST::StructuralHole(n, current_nest) => {
                assert!(current_nest.size() == 0);
                SketchAST::StructuralHole(n.clone(), Box::new(nest))
            },
            SketchAST::StoreAt(_n, _var) => panic!("Can't set nest to a store"),
            SketchAST::Prefetch(_n, _var, _, _) => panic!("Can't set nest to a prefetch"),
            SketchAST::Property(_n, _property) => panic!("Can't set nest to a store order"),
            SketchAST::Sequence(_n, _nest) => panic!("Can't set nest to a sequence"),
            SketchAST::Assign(_n, _var) => panic!("Can't set nest to an assign"),
        }
    }
}

fn nest_rules(rules: Vec<SketchAST>) -> Vec<SketchAST> {
    match rules.len() {
        0 => vec![],
        1 => rules,
        _n => {
            let (head_array, rest) = rules.split_at(1);
            let head = head_array[0].clone();// will always have one element.
            let head_nesting = get_nest_depth(&head);

            // Get the things that belong to head and the things that are at
            // the same level as head.
            // head index is the index where we return to the nesting of this
            // loop --- everything that follows belongs at the same
            // nesting level.
            let head_index = rest.iter().position(|x| get_nest_depth(x) <= head_nesting);
            let (before, after) = match head_index {
                Some(head_index) => rest.split_at(head_index),
                // if no place where we returned to a lesser nesting,
                // there is nothing that comes back above the previous statement.
                None => rest.split_at(rest.len())
            };

            // Recursively build structures in the nest:
            let structured_subnest = nest_rules(before.to_vec());
            // recurviesvley build structures in the tail:
            let mut structured_rest = nest_rules(after.to_vec());

            // Set the sub-slements of this item appropriately:
            let nested = set_nest(&head, SketchAST::Sequence(head_nesting + 1, structured_subnest));
            // And put this at the start of the nest for this level.
            structured_rest.insert(0, nested);
            structured_rest
        }
    }
}

fn nest(parsed: SketchAST) -> SketchAST {
    match parsed {
        SketchAST::Sequence(0, rules) => {
            SketchAST::Sequence(0, nest_rules(rules))
        }
        _ => panic!("Unexepcted top-level statement {}", parsed.to_string())
    }
}

fn find_func_properties_in(ast: &SketchAST) -> (Vec<ASTFuncProperty>, Option<SketchAST>) {
    match ast {
        SketchAST::Produce(_, _, _, _) => panic!("Unexpected func when finding properties"),
        SketchAST::Consume(n, v) => (vec![], Some(SketchAST::Consume(n.clone(), v.clone()))),
        SketchAST::For(n, v, subast, range, props) => {
            let (func_props, option_subast) = find_func_properties_in(subast);
            let new_subast = match option_subast {
                Some(s) => s,
                None => panic!("Bad structure: the only child of the for was a function property"),
            };
            let res_ast = SketchAST::For(n.clone(), v.clone(), Box::new(new_subast), range.clone(), props.clone());
            (func_props, Some(res_ast))
        },
        SketchAST::Assign(_, _) => (vec![], Some(ast.clone())),
        SketchAST::StoreAt(_, _) => (vec![], Some(ast.clone())),
        SketchAST::Sequence(n, ss) => {
            let mut result = Vec::new();
            let mut new_ss = Vec::new();
            for ast in ss {
                let (intermediate_result, intermediate_ast) = find_func_properties_in(ast);
                result.extend(intermediate_result);
                match intermediate_ast {
                    Some(new_ast) => new_ss.push(new_ast),
                    None => {}
                }
            };
            (result, Some(SketchAST::Sequence(n.clone(), new_ss)))
        },
        SketchAST::Prefetch(_, _, _, _) => (vec![], Some(ast.clone())),
        SketchAST::StructuralHole(_, _) => (vec![], Some(ast.clone())),
        SketchAST::Property(_, prop) => (vec![prop.clone()], None),
    }
}

// This takes the func properties and puts them in the func definitions.
// It removes those properties from the AST
fn lift_func_properties(nested: &SketchAST) -> SketchAST {
    match nested {
        SketchAST::Sequence(n, rules) => {
            SketchAST::Sequence(n.clone(), rules.iter().map(lift_func_properties).collect())
        },
        SketchAST::Produce(n, name, body, properties) => {
            let (mut new_properties, new_body)  = find_func_properties_in(body);
            let new_body_unwrapped = match new_body {
                Some(body) => body,
                None => panic!("Error: only child of a produce was a func property")
            };
            // properties will be empty here --- but extend for future
            // extensibility.
            new_properties.extend(properties.clone());
            SketchAST::Produce(n.clone(), name.clone(), Box::new(new_body_unwrapped), new_properties)
        },
        // We don't support nested func defs, 
        // so other recursion cases are not required.
        _ => panic!("Got a top-level non-func")
    }
}

pub fn parse(opts: &Options, filename: &String) -> SketchAST {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let mut sequence = LoopParser::parse(Rule::sequence_list, &input[..]).unwrap();
    // Parse into a flat structure.
    let parsed = process(opts, 0, sequence.next().unwrap());
    if opts.debug_parser {
        println!("Parsed {}", parsed.to_string());
    }

    // Go through and properly nest everything.
    let nested = nest(parsed);
    if opts.debug_parser {
        println!("Nested {}", nested.to_string());
    }

    // Remove the property annotations from within the funcs and put them
    // on the parent func for each command.
    let func_properties_lifted = lift_func_properties(&nested);

    if opts.debug_parser {
        println!("Func Properties Lifted {}", func_properties_lifted.to_string());
    }
    func_properties_lifted

}

use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::fs;
use std::collections::HashMap;

use crate::options::options::Options;
use crate::sketch_parse::parser::Variable;
use crate::reshape::reshape::Reshape;

use crate::ast::ast::Func;
use crate::ast::ast::Var;
use crate::ast::convert::variable_to_var;
use crate::ast::ast::AST;
use crate::ast::ast::HoleOptionTrait;
use crate::sketch_parse::parser::ASTNumberOrHole;
use crate::shared::range_set::AnyIntegerSet;
use crate::shared::range_set::set_from_name;
use crate::ast::convert::hole_from_ast_hole;

#[derive(Parser)]
#[grammar = "sketch_parse/splits.pest"]
struct SplitsPest;

#[derive(Clone)]
pub enum SplitsAST {
    Split(Variable, (Variable, Variable), ASTNumberOrHole),
    Fuse((Variable, Variable), Variable),
}

fn splits_ast_to_reshape(ast: SplitsAST, func_lookup: &mut HashMap<Var, Func>) -> Reshape {
    match ast {
        SplitsAST::Split(var, (var1, var2), dim) => {
            let new_var = variable_to_var(&var).get().unwrap(); // not allowed to ahve holes in reshape variables
            let new_var1 = variable_to_var(&var1).get().unwrap();
            let new_var2 = variable_to_var(&var2).get().unwrap();
            let new_func = func_lookup.get(&new_var).unwrap().clone();

            let new_dim = hole_from_ast_hole(dim);
            
            // keep the func_lookup table updated.
            func_lookup.insert(new_var1.clone(), new_func.clone());
            func_lookup.insert(new_var2.clone(), new_func.clone());

            Reshape::Split(new_func, new_var, (new_var1, new_var2), new_dim)
        },
        SplitsAST::Fuse((var1, var2), var) => {
            // note vars are in a different order from above.
            let new_var1 = variable_to_var(&var1).get().unwrap();
            let new_var2 = variable_to_var(&var2).get().unwrap();
            let new_var = variable_to_var(&var).get().unwrap();
            let new_func = func_lookup.get(&new_var1).unwrap().clone();
            
            // keep the func_lookup table updated.
            func_lookup.insert(new_var.clone(), new_func.clone());

            Reshape::Fuse(new_func, (new_var1, new_var2), new_var)
        },
    }
}

fn splits_asts_to_reshapes(asts: Vec<SplitsAST>, func_lookup: &mut HashMap<Var, Func>) -> Vec<Reshape> {
    asts.into_iter().map(|x| splits_ast_to_reshape(x, func_lookup)).collect()
}

impl ToString for SplitsAST
 {
    fn to_string(&self) -> String {
        match self {
            SplitsAST::Split(var, (var1, var2), cost) => {
                format!("Split({}, ({}, {}), {})", var.to_string(), var1.to_string(), var2.to_string(), cost.to_string())
            },
            SplitsAST::Fuse((var1, var2), var) => {
                format!("Fuse(({}, {}), {})", var1.to_string(), var2.to_string(), var.to_string())
            }
        }
    }
}

fn process_factor(_opts: &Options, rule: Pair<Rule>) -> ASTNumberOrHole {
    match rule.as_rule() {
        Rule::factor_or_hole => {
            // convert rule string into i32
			let _inner = rule.clone().into_inner();
            let factor_str = rule.as_span().as_str();
			if !factor_str.starts_with("<") {
				if factor_str == "??" {
                    ASTNumberOrHole::Hole(AnyIntegerSet())
				} else {
                    // println!("Factorint {}", factor_str);
					let factor_int = factor_str.parse::<i32>().unwrap();
					ASTNumberOrHole::Number(factor_int)
				}
			} else { // starts with <
                // This is a set --- tOdo -- need to process for arbitrary
                // sets not just the named ones.
                // This should be co-ordinated with the full grammar.
                ASTNumberOrHole::Hole(set_from_name(factor_str[1..factor_str.len() - 1].to_string()))
			}
        },
        Rule::factor => {
            let factor_int = rule.as_str().parse::<i32>().unwrap();
            ASTNumberOrHole::Number(factor_int)
        }
        _ => panic!("Expected factor, got {:?}", rule.as_str()),
    }
}

fn process_ident(opts: &Options, rule: Pair<Rule>) -> Variable {
    match rule.as_rule() {
        Rule::ident => {
            if opts.debug_parser {
                println!("Got an ident");
            }
            let name = rule.as_str();
            Variable{name: name.into(), hole: false}
        },
        _ => panic!("Unable to process non-ident sequence into variable")
    }
}

fn process(opts: &Options, rule: Pair<Rule>) -> Vec<SplitsAST> {
    match rule.as_rule() {
        Rule::sequence_list => {
            let mut inner = rule.into_inner();
            if inner.len() == 2 {
                // hit the rule for an empty file.
                return vec![];
            }

            let mut seq = process(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let tail = inner.next().unwrap();
            let has_tail = match tail.as_rule() {
                Rule::sequence_list => true,
                Rule::EOI => false,
                _ => panic!("Unexpected last token '{}'", tail)
            };
            if has_tail {
                let mut rest = process(opts, tail);
                seq.append(&mut rest);
                seq
            } else {
                seq
            }
        },
        Rule::sequence => {
            let mut inner = rule.into_inner();
            assert!(inner.len() == 1);
            process(opts, inner.next().unwrap())
        },
        Rule::split => {
            let mut inner = rule.into_inner();

            let split_from = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let split_to_1 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let split_to_2 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let split_factor = process_factor(opts, inner.next().unwrap());

            // TODO -- load the split factor
            vec![SplitsAST::Split(split_from, (split_to_1, split_to_2), split_factor)]
        },
        Rule::fuse => {
            let mut inner = rule.into_inner();

            let fuse_from_1 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let fuse_from_2 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let fuse_to = process_ident(opts, inner.next().unwrap());

            // TODO -- load the split factor
            vec![SplitsAST::Fuse((fuse_from_1, fuse_from_2), fuse_to)]
        },
        _ => panic!("unexpected rule {}", rule.as_str())
    }
}

pub fn parse(opts: &Options, ast: &AST, filename: &String) -> Vec<Reshape> {
    let input = fs::read_to_string(filename).expect("unable to read file");
    let mut sequence = SplitsPest::parse(Rule::sequence_list, &input[..]).unwrap();

    let parsed = process(opts, sequence.next().unwrap());

    // gets modified as we go through the rewrites to preserve
    // which func each variable belongs to.
    let mut func_lookup = crate::ast::analysis::func_table_for(opts, ast);
    splits_asts_to_reshapes(parsed, &mut func_lookup)
}

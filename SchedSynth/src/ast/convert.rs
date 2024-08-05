use crate::ast::ast::AST;
use crate::ast::ast::ForRange;
use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::Buf;
use crate::ast::ast::Property;
use crate::ast::ast::FuncProperty;
use crate::ast::ast::NumberOrHole;
use crate::ast::ast::VarOrHole;
use crate::sketch_parse::parser::SketchAST;
use crate::sketch_parse::parser::ForRangeAST;
use crate::sketch_parse::parser::ASTLoopProperty;
use crate::sketch_parse::parser::ASTFuncProperty;
use crate::sketch_parse::parser::ASTNumberOrHole;

use std::fmt;

// implement display for Var
impl fmt::Display for Var {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}


impl fmt::Display for ForRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForRange::Between(start, end) => write!(f, "({}, {})", start, end),
            ForRange::All() => write!(f, "all")
        }
    }
}

impl fmt::Display for Property {
 fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Property::Vectorize() => write!(f, "vectorize"),
            Property::Parallel() => write!(f, "parallel"),
            Property::Unroll(ref n) => write!(f, "unroll({})", n),
            Property::Fuse(ref n) => write!(f, "fuse({})", n)
        }
    }
}

impl fmt::Display for FuncProperty {
  fn fmt (&self, f: &mut fmt::Formatter) -> fmt::Result {
      match self {
          FuncProperty::StoreOrder(vs) => {
              let varlist = vs.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
              write!(f, "StoreOrder({})", varlist)
          },
          FuncProperty::Memoize() =>
              write!(f, "Memoize()"),
          FuncProperty::AllowRaceConditions() =>
              write!(f, "AllowRaceConditions()"),
          FuncProperty::Async() =>
              write!(f, "Async"),
      }
  }
}

impl PartialEq for Property {
    fn eq(&self, other: &Property) -> bool {
        match (self, other) {
            (&Property::Vectorize(), &Property::Vectorize()) => true,
            (&Property::Parallel(), &Property::Parallel()) => true,
            (&Property::Unroll(ref n1), &Property::Unroll(ref n2)) => n1 == n2,
            (&Property::Fuse(ref n1), &Property::Fuse(ref n2)) => n1 == n2,
            _ => false
        }
    }
}


// implement ToString for AST
impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AST::Produce(ref var, ref ast, ref props) => {
                let props_string = props.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
                write!(f, "produce {} in ({}) with {}", var, ast, props_string)
            },
            AST::Consume(ref var) => write!(f, "consume {}", var),
            AST::For(ref var, ref ast, ref range, ref properties) => {
                let property_string = 
                    properties.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
                write!(f, "for {} [{}] in {}: ({})", var, property_string, range, ast)
            },
            AST::Assign(ref var) => write!(f, "assign {}", var),
            AST::StoreAt(ref var) => write!(f, "store {} here", var),
            AST::Prefetch(ref buf, ref var, ref stride) => write!(f, "prefetch {} along dimension {} (stride {}) here", buf, var, stride),
            AST::StructuralHole(ref ast) => write!(f, "structural hole ({})", ast),
            AST::Sequence(ref ast) => {
                let mut s = String::new();
                for a in ast {
                    s.push_str(&format!("{}", a));
                    s.push_str("; ");
                }
                write!(f, "{}", s)
            }
        }
    }
}

pub fn variable_to_var(variable: &crate::sketch_parse::parser::Variable) -> VarOrHole {
    if variable.hole {
        VarOrHole::Hole()
    } else {
        VarOrHole::Var(Var { name: variable.name.clone() })
    }
}

pub fn variable_to_func(variable: crate::sketch_parse::parser::Variable) -> Func {
    // split varaible.name into a part before the '.' and an optional part after
    // the '.'
    // println!("Converting {}", variable.name);
    let (name_before_dot, name_after_dot) = match variable.name.find('.') {
        Some(index) => {
            // convert &after[1..] into an i32
            let (before, after) = variable.name.split_at(index);
            let after_str = &after[1..];
            let after_num = after_str.parse::<i32>();
            // convert to Option
            let after_num = match after_num {
                Ok(num) => Some(num),
                Err(_) => None,
            };
            (before, after_num)
        }
        None => (variable.name.as_str(), None),
    };

    Func { name: name_before_dot.into(), update: name_after_dot}
}

pub fn variable_to_buf (variable: crate::sketch_parse::parser::Variable) -> Buf {
	assert!(!variable.hole, "Not allowed to have buf with holes");

	Buf { name: variable.name }
}

fn var_to_variable(var: Var) -> crate::sketch_parse::parser::Variable {
    crate::sketch_parse::parser::Variable { name: var.name, hole: false }
}

fn ast_from_range(input: ForRangeAST) -> ForRange {
    match input {
        ForRangeAST::Between(start, end) => ForRange::Between(hole_from_ast_hole(start), hole_from_ast_hole(end)),
        ForRangeAST::All() => ForRange::All()
    }
}

pub fn ast_from_sketch_ast(input: SketchAST) -> AST {
    match input {
        SketchAST::Produce(_nesting, var, ast, properties) => {
            AST::Produce(variable_to_func(var), Box::new(ast_from_sketch_ast(*ast)), func_properties_from_properties(properties))
        },
        SketchAST::Consume(_nesting, var) => {
            AST::Consume(variable_to_func(var))
        },
        SketchAST::For(_nesting, var, ast, range, properties) => {
            AST::For(variable_to_var(&var), Box::new(ast_from_sketch_ast(*ast)), ast_from_range(range), properties_from_loop_properties(properties))
        },
        SketchAST::Assign(_nesting, var) => {
            AST::Assign(variable_to_func(var))
        },
        SketchAST::StoreAt(_nesting, var) => {
            AST::StoreAt(variable_to_func(var))
        },
        SketchAST::Prefetch(_nesting, buf, var, stride) => {
            AST::Prefetch(variable_to_buf(buf), variable_to_var(&var), hole_from_ast_hole(stride))
        },
        SketchAST::StructuralHole(_nesting, nest) => {
            AST::StructuralHole(Box::new(ast_from_sketch_ast(*nest)))
        }
        SketchAST::Sequence(_nesting, asts) => {
            let mut ast_vec = Vec::new();
            for ast in asts {
                ast_vec.push(ast_from_sketch_ast(ast));
            }
            AST::Sequence(ast_vec)
        },
        SketchAST::Property(_nesting, _prop) => {
            panic!("Expected propertie attributes to be removed.  ")
        }
    }
}

pub fn property_from_loop_property(input: ASTLoopProperty) -> Property {
    match input {
        ASTLoopProperty::Vectorize() => Property::Vectorize(),
        ASTLoopProperty::Parallel() => Property::Parallel(),
        ASTLoopProperty::Unroll(i) => Property::Unroll(hole_from_ast_hole(i)),
        ASTLoopProperty::Fuse(i) => Property::Fuse(variable_to_var(&i))
    }
}

pub fn func_property_from_property(input: &ASTFuncProperty) -> FuncProperty {
    match input {
        ASTFuncProperty::StoreOrder(vs) => FuncProperty::StoreOrder(vs.clone().iter().map(variable_to_var).collect()),
        ASTFuncProperty::Memoize() => FuncProperty::Memoize(),
        ASTFuncProperty::Async() => FuncProperty::Async(),
        ASTFuncProperty::AllowRaceConditions() => FuncProperty::AllowRaceConditions(),
    }
}

pub fn func_properties_from_properties(input: Vec<ASTFuncProperty>) -> Vec<FuncProperty> {
    input.iter().map(func_property_from_property).collect()
}

pub fn hole_from_ast_hole(input: ASTNumberOrHole) -> NumberOrHole  {
	match input {
		ASTNumberOrHole::Hole(set) => NumberOrHole::Hole(set),
		ASTNumberOrHole::Number(n) => NumberOrHole::Number(n)
	}
}

pub fn properties_from_loop_properties(input: Vec<ASTLoopProperty>) -> Vec<Property> {
    input.into_iter().map(property_from_loop_property).collect()
}


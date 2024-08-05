use crate::gen::target::TargetGenerate;
use crate::gen::target::TargetLower;
use crate::gen::target::Target;
use crate::gen::target::HoleBindingMap;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::ast::ast::Property;
use crate::gen::target::TargetHoles;
use crate::gen::target::Hole;
use crate::options::options::Options;

#[derive(Clone)]
pub struct ExoFunc {
    pub name: String,
}

#[derive(Clone)]
pub struct ExoVar {
    pub name: String
}

#[derive(Clone)]
pub enum ExoCommand {
    Reorder(ExoFunc, ExoVar, ExoVar),
    Split(ExoFunc, ExoVar, ExoVar, ExoVar, i32), // split the var loop into var, var
    Fuse(ExoFunc, ExoVar, ExoVar, ExoVar)
}

#[derive(Clone)]
pub struct ExoProgram {
    pub commands: Vec<ExoCommand>,
    pub funcs: Vec<ExoFunc>,
}

pub struct ExoHole {

}

impl Target for ExoProgram {}
impl TargetGenerate for ExoProgram {
    fn generate(&self) -> String {
        return "".to_string()
    }

    fn get_required_build_flags(&self, _opts: &Options) -> Vec<String> {
        vec![]
    }
}

impl TargetLower for ExoProgram {
    fn to_vectorize(&mut self, _commands: Vec<(Func, Var, Property)>) { }
    fn to_parallel(&mut self, _commands: Vec<(Func, Var, Property)>) { }
    fn to_store_at(&mut self, _commands: Vec<(Func, Func, Var)>) { }
    fn to_unroll(&mut self, _commands: Vec<(Func, Var, Property)>) { }
    fn to_compute_at(&mut self, _commands: Vec<(Func, Option<Func>, Option<Var>)>) { }
    fn to_compute_with(&mut self, _commands: Vec<(Func, Var, Var)>) { }
    fn to_reorder(&mut self, _commands: Vec<(Func, Var, Var)>) { }
    fn to_reshape(&mut self, _commands: &Vec<Reshape>) { }
    fn to_prefetch(&mut self, _commands: Vec<(Buf, VarOrHole, NumberOrHole)>) { }
    fn to_func_property(&mut self, _commands: Vec<(Func, FuncProperty)>) { }
}

impl TargetHoles for ExoProgram {
    fn get_holes(&self) -> Vec<Box<dyn Hole>> { vec![] }
    fn can_resolve_holes(&self, _opts: &Options) -> bool { true }
    fn fill_holes(&mut self, _map: &HoleBindingMap) { }
}

impl ToString for ExoProgram {
    fn to_string(&self) -> String {
        let mut result = String::new();

        for command in &self.commands {
            let exo_directive = match command {
                ExoCommand::Reorder(func, fvar, tvar) =>
                    format!("{} = reorder({}, \"{} {}\")", func.name, func.name, fvar.name, tvar.name),
                ExoCommand::Split(func, fvar, tvar1, tvar2, factor) =>
                    format!("{} = divide_loop({}, {}, \"{}\", [\"{}\", \"{}\"], perfect=True)", func.name, func.name, factor, fvar.name, tvar1.name, tvar2.name),
                ExoCommand::Fuse(func, fvar1, fvar2, _tvar) =>
                    format!("{} = fuse({}, {}, {})\n#TODO --rename tvar", func.name, func.name, fvar1.name, fvar2.name),
            };
            result.push_str(&exo_directive);
        }
        result
    }
}

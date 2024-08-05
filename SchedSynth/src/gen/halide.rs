use crate::options::options::Options;
use crate::gen::target::TargetGenerate;
use crate::gen::target::TargetLower;
use crate::gen::target::TargetHoles;
use crate::gen::target::Target;
use crate::gen::target::Hole;
use crate::gen::target::HoleOption;
use crate::gen::target::is_hole;
use crate::gen::target::HoleBindingMap;
use crate::gen::target::hole_value_to_option;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;

use std::sync::Mutex;
use std::path::Path;

#[derive(Clone)]
pub struct HFunc {
    pub name: String,
    pub update: Option<i32>,
}

#[derive(Clone)]
pub struct HBuf {
	pub name: String
}

#[derive(Clone)]
pub struct HVar {
    pub name: String
}

impl std::fmt::Display for HFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.update {
            Some(upnum) => write!(f, "{}.update({})", self.name, upnum),
            None => write!(f, "{}", self.name)
        }
    }
}

impl std::fmt::Display for HVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for HBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub enum HalideCommand {
    Vectorize(HoleOption<HFunc>, HoleOption<HVar>), // HFunc to vectorize
    Parallel(HoleOption<HFunc>, HoleOption<HVar>), // HFunc to vectorize
    Unroll(HoleOption<HFunc>, HoleOption<HVar>, HoleOption<i32>), // HFunc to unroll, unroll factor.
    Tile(), // HFunc to tile, 
    ComputeAt(HoleOption<HFunc>, HoleOption<HFunc>, HoleOption<HVar>), // Compute func at func at varaiable
    ComputeWith(HoleOption<HFunc>, HoleOption<HVar>, HoleOption<HVar>),
    StoreAt(HoleOption<HFunc>, HoleOption<HFunc>, HoleOption<HVar>), // store func at variable
    Prefetch(HoleOption<HBuf>, HoleOption<HVar>, HoleOption<i32>), // store func at variable
    ComputeRoot(HoleOption<HFunc>), // Compute func at func at varaiable
    Reorder(HoleOption<HFunc>, (HoleOption<HVar>, HoleOption<HVar>)), // Reoder <to> hvar, hvar
    Split(HoleOption<HFunc>, HoleOption<HVar>, (HoleOption<HVar>, HoleOption<HVar>), HoleOption<i32>), // split var into (var, var) with tiling factor i32
    Fuse(HoleOption<HFunc>, (HoleOption<HVar>, HoleOption<HVar>), HoleOption<HVar>), // fuse (var, var) into (var)

    // This is the function properties.
    Memoize(HoleOption<HFunc>),
    StoreOrder(HoleOption<HFunc>, Vec<HoleOption<HVar>>),
    Async(HoleOption<HFunc>),
    AllowRaceConditions(HoleOption<HFunc>),
}

impl TargetHoles for HalideProgram {
    fn get_holes(&self) -> Vec<Box<dyn Hole>> {
        let mut holes: Vec<Box<dyn Hole>> = vec![];
        for command in &self.commands {
            match command {
                HalideCommand::Vectorize(ref hf, ref hv) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                },
                HalideCommand::Parallel(ref hf, ref hv) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                },
                HalideCommand::Unroll(ref hf, ref hv, ref hi) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                    if is_hole(hi) {
                        holes.push(Box::new(hi.clone()))
                    }
                },
                HalideCommand::Tile() => {
                    // TODO
                },
                HalideCommand::ComputeAt(ref hf1, ref hf2, ref hv) => {
                    if is_hole(hf1) {
                        holes.push(Box::new(hf1.clone()))
                    }
                    if is_hole(hf2) {
                        holes.push(Box::new(hf2.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                },
                HalideCommand::ComputeWith(ref hf, ref hv1, ref hv2) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(hv1) {
                        holes.push(Box::new(hv1.clone()))
                    }
                    if is_hole(hv2) {
                        holes.push(Box::new(hv2.clone()))
                    }
                },
                HalideCommand::StoreAt(ref hf1, ref hf2, ref hv) => {
                    if is_hole(hf1) {
                        holes.push(Box::new(hf1.clone()))
                    }
                    if is_hole(hf2) {
                        holes.push(Box::new(hf2.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                },
				HalideCommand::Prefetch(ref buf, ref dim, ref stride) => {
					if is_hole(buf) {
						holes.push(Box::new(buf.clone()))
					}
					if is_hole(dim) {
						holes.push(Box::new(dim.clone()))
					}
					if is_hole(stride) {
						holes.push(Box::new(stride.clone()))
					}
				}
                HalideCommand::ComputeRoot(ref hf) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                },
                HalideCommand::Reorder(ref hf, ref hv) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(&hv.0) {
                        holes.push(Box::new(hv.0.clone()))
                    }
                    if is_hole(&hv.1) {
                        holes.push(Box::new(hv.1.clone()))
                    }
                },
                HalideCommand::Split(ref hf, ref hv, ref hvs, ref hi) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                    if is_hole(&hvs.0) {
                        holes.push(Box::new(hvs.0.clone()))
                    }
                    if is_hole(&hvs.1) {
                        holes.push(Box::new(hvs.1.clone()))
                    }
                    if is_hole(hi) {
                        holes.push(Box::new(hi.clone()))
                    }
                },
                HalideCommand::Fuse(ref hf, ref hvs, ref hv) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                    if is_hole(&hvs.0) {
                        holes.push(Box::new(hvs.0.clone()))
                    }
                    if is_hole(&hvs.1) {
                        holes.push(Box::new(hvs.1.clone()))
                    }
                    if is_hole(hv) {
                        holes.push(Box::new(hv.clone()))
                    }
                },
                HalideCommand::Memoize(ref hf) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                },
                HalideCommand::Async(ref hf) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                },
                HalideCommand::AllowRaceConditions(ref hf) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }
                },
                HalideCommand::StoreOrder(ref hf, ref vs) => {
                    if is_hole(hf) {
                        holes.push(Box::new(hf.clone()))
                    }

                    for var in vs {
                        if is_hole(var) {
                            holes.push(Box::new(var.clone()))
                        }
                    }
                },
            }
        }
        holes
    }

    fn can_resolve_holes(&self, opts: &Options) -> bool {
        // check that we have all the information actually
        // required to build the program.
        // 
        // Need to check that we have:
        //  (a) halide_program set
        //  (b) halide_dir set

        // This doesn't do a full check --- so things could fail
        // elsewhere --- would be better to do a truly complete check
        // here (i.e. check that halid-dir has all the required
        // files and that halide_program points to a currectly formatted
        // file.

        // check that opts.halide_dir (a string) is an existing folder
        let halide_dir = Path::new(&opts.halide_dir);
        if !halide_dir.exists() {
            return false;
        }

        // check that opts.halide_program (a string) is an existing file
        let halide_program = Path::new(&opts.halide_program);
        if !halide_program.exists() {
            return false;
        }

        return true;
    }

    fn fill_holes(&mut self, map: &HoleBindingMap) {
        for command in &mut self.commands {
            match command {
                HalideCommand::Vectorize(ref mut hf, ref mut hv) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::Parallel(ref mut hf, ref mut hv) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::Unroll(ref mut hf, ref mut hv, ref mut hi) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hi) {
                        *hi = hole_value_to_option(map.map.get(&hi.to_string()).unwrap().clone());
                    }
                },
                HalideCommand::Tile() => {
                    // TODO
                },
                HalideCommand::ComputeAt(ref mut hf1, ref mut hf2, ref mut hv) => {
                    if is_hole(hf1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hf2) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::ComputeWith(ref mut hf1, ref mut hv1, ref mut hv2) => {
                    if is_hole(hf1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv2) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::StoreAt(ref mut hf1, ref mut hf2, ref mut hv) => {
                    if is_hole(hf1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hf2) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                },
				HalideCommand::Prefetch(ref mut buf, ref mut dim, ref mut stride) => {
					if is_hole(buf) {
						assert!(false)
					}

					if is_hole(dim) {
						assert!(false)// should be able to handle this type of hole tbh.
					}

					if is_hole(stride) {
                        *stride = hole_value_to_option(map.map.get(&stride.to_string()).unwrap().clone());
					}
				},
                HalideCommand::ComputeRoot(ref mut hf) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::Reorder(ref mut hf, ref mut hv) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hv.0) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hv.1) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::Split(ref mut hf, ref mut hv, ref mut hvs, ref mut hi) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hvs.0) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hvs.1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hi) {
                        *hi = hole_value_to_option(map.map.get(&hi.to_string()).unwrap().clone());
                    }
                },
                HalideCommand::Fuse(ref mut hf, ref mut hvs, ref mut hv) => {
                    if is_hole(hf) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hvs.0) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(&hvs.1) {
                        assert!(false) // unsupported hole type
                    }
                    if is_hole(hv) {
                        assert!(false) // unsupported hole type
                    }
                },
                HalideCommand::Memoize(ref hf) => {
                    if is_hole(hf) {
                        assert!(false)
                    }
                },
                HalideCommand::AllowRaceConditions(ref hf) => {
                    if is_hole(hf) {
                        assert!(false)
                    }
                },
                HalideCommand::Async(ref hf) => {
                    if is_hole(hf) {
                        assert!(false)
                    }
                },
                HalideCommand::StoreOrder(ref hf, ref vars) => {
                    if is_hole(hf) {
                        assert!(false)
                    }

                    for v in vars {
                        if is_hole(v) {
                            assert!(false)
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct HalideProgram {
    pub commands: Vec<HalideCommand>
}

// Holes are a top-level primitive in the HalideCommand language,
// but we also use this enum to keep track of the
// properties with which each hole should be filled.
#[derive(Clone)]
pub enum HalideHole {
    NumberHole(),
    // Pattern holes best specified at the loop level
    // rather than the optimization level.
    PatternHole()
}

impl Hole for HalideHole {
    fn to_opentuner(&self, manipulator_name: &String) -> String {
        format!("{}.add_parameter(IntegerParameter('x', 0, 10))", manipulator_name)
    }

    fn get_name(&self) -> String {
        "x".to_string()
    }
}

impl TargetGenerate for HalideProgram {
    fn generate(&self) -> String {
        self.to_string()
    }

    fn get_required_build_flags(&self, opts: &Options) -> Vec<String> {
        vec![
            "-I".to_owned() + &opts.halide_dir + "/include",
            "-L".to_owned() + &opts.halide_dir + "/lib",
            "-lHalide".to_owned()
        ]
    }
}

//temp shortcut for this wrapper --- I expect
// that eventually the middle end will support
// holes and so re-wrapping everything here will
// be unnessecary.
fn v<T>(i: T) -> HoleOption<T> {
    HoleOption::Value(i)
}

static hole_number_so_far: Mutex<i32> = Mutex::new(0);
fn get_hole_name() -> String {
    let mut num = hole_number_so_far.lock().unwrap();
    *num = *num + 1;

    return "hole_".to_owned() + &(*num).to_string();
}

fn number_or_hole_to_hole_option(n: NumberOrHole) -> HoleOption<i32> {
    match n {
        NumberOrHole::Number(n) => HoleOption::Value(n),
        NumberOrHole::Hole(range) =>
            HoleOption::IntHole(get_hole_name(), range)
            // TODO --- convert to something for the halid backeng
    }
}

fn var_or_hole_to_hole_option(n: VarOrHole) -> HoleOption<HVar> {
    match n {
        VarOrHole::Var(v) => HoleOption::Value(HVar {name: v.name}),
        VarOrHole::Hole() => panic!("Variable holes are currently unsupported") // TODO
    }
}

fn buf_or_hole_to_hole_option(n: BufOrHole) -> HoleOption<HBuf> {
    match n {
        BufOrHole::Buf(b) => HoleOption::Value(HBuf {name: b.name}),
        BufOrHole::Hole() => panic!("Buffer holes are currently unsupported") // TODO
    }
}

impl TargetLower for HalideProgram {
    // Conert the the pairs of func and variable into
    // vectorize halide commands.
    fn to_vectorize(&mut self, commands: Vec<(Func, Var, Property)>) {
        // the first far is the func, and the second var is the variable
        // to vectorize (hvar).
        let mut halide_commands = Vec::new();
        for (func, hvar, _) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Vectorize(v(hfunc), v(hhvar)));
        }

        self.commands.append(&mut halide_commands);
    }

    fn to_parallel(&mut self, commands: Vec<(Func, Var, Property)>) {
        let mut halide_commands = Vec::new();
        for (func, hvar, _) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Parallel(v(hfunc), v(hhvar)));
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_unroll(&mut self, commands: Vec<(Func, Var, Property)>) {
        let mut halide_commands = Vec::new();
        for (func, hvar, unroll_property) in commands {
			let factor = match unroll_property {
				Property::Unroll(size) => size,
				_ => panic!("Trying to unroll with non-unroll property!")
			};
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Unroll(v(hfunc), v(hhvar), number_or_hole_to_hole_option(factor)));
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_store_at(&mut self, commands: Vec<(Func, Func, Var)>) {
        let mut halide_commands = Vec::new();
        for (func, func2, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hfunc2 = HFunc { name: func2.name, update: func2.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::StoreAt(v(hfunc), v(hfunc2), v(hhvar)));
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_prefetch(&mut self, commands: Vec<(Buf, VarOrHole, NumberOrHole)>) {
        let mut halide_commands = Vec::new();
        for (buf, var, stride) in commands {
            let hbuf = buf_or_hole_to_hole_option(BufOrHole::Buf(buf));
            let hvar = var_or_hole_to_hole_option(var);
            let hstride = number_or_hole_to_hole_option(stride);

            halide_commands.push(HalideCommand::Prefetch(hbuf, hvar, hstride));
        }

        self.commands.append(&mut halide_commands);
    }

    // turn the var var var into this:
    // ComputeAt(HFunc, HFunc, HVar) // Compute func at func at varaiable
    fn to_compute_at(&mut self, commands: Vec<(Func, Option<Func>, Option<Var>)>) {
        let mut halide_commands = Vec::new();
        for (func, compute_at_func, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            match (compute_at_func, hvar) {
                (Some(compute_at_func), Some(hvar)) => {
                    let hhvar = HVar { name: hvar.name };
                    let hcompute_at_func = HFunc { name: compute_at_func.name, update: func.update };
                    halide_commands.push(HalideCommand::ComputeAt(v(hfunc), v(hcompute_at_func), v(hhvar)));
                },
                (None, None) => {
                    // I /think/ that this is only possible this wa.
                    // Not 100% sure what it means with the var set.
                    halide_commands.push(HalideCommand::ComputeRoot(v(hfunc)));
                },
                (None, Some(v)) => panic!("Unexpected variable {} set when processing compute_root", v),
                (Some(v), None) => panic!("Unexpected func {} set when processing compute_root", v)
            }
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_compute_with(&mut self, commands: Vec<(Func, Var, Var)>) {
        let mut halide_commands = Vec::new();
        for (func, compute_with_var, targ_var) in commands {
            let hfunc = HFunc {name: func.name, update: func.update };
            let compute_with_hvar = HVar { name: compute_with_var.name };
            let targ_hvar = HVar {name: targ_var.name };

            halide_commands.push(HalideCommand::ComputeWith(v(hfunc), v(compute_with_hvar), v(targ_hvar)))
        }

        self.commands.append(&mut halide_commands)
    }

    // turn the var var var into this:
    // Reorder(HFunc, HVar, HVar) // Compute func at func at varaiable
    fn to_reorder(&mut self, commands: Vec<(Func, Var, Var)>) {
        let mut halide_commands = Vec::new();
        for (func, compute_at_func, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hvar1 = HVar { name: compute_at_func.name };
            let hvar2 = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Reorder(v(hfunc), (v(hvar1), v(hvar2))));
        }
        self.commands.append(&mut halide_commands)
    }

    // the internal finder returns Reshape::Split(Var, (Var, Var)) which needs to be converted
    // into HalideCommand::Split and Reshape::Fuse((Var, Var), Var) which needs to be converted
    // into HalideCommand::Fuse
    fn to_reshape(&mut self, commands: &Vec<Reshape>) {
        let mut halide_commands = Vec::new();
        for command in commands {
            match command {
                // TODO -- figure out how to get the producer name into here.
                Reshape::Split(func, var, (var1, var2), factor) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar = HVar { name: var.name.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Split(v(hfunc), v(hvar), (v(hvar1), v(hvar2)), number_or_hole_to_hole_option(factor.clone())));
                },
                Reshape::Fuse(func, (var1, var2), var) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar = HVar { name: var.name.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Fuse(v(hfunc), (v(hvar1), v(hvar2)), v(hvar)));
                },
                Reshape::Reorder(func, (var1, var2)) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Reorder(v(hfunc), (v(hvar1), v(hvar2))));
                },
            }
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_func_property(&mut self, commands: Vec<(Func, FuncProperty)>) {
        let mut halide_commands = Vec::new();
        for (func, command) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            match command {
                FuncProperty::StoreOrder(vs) => {
                    let mut hvars = Vec::new();
                    for var in vs {
                        hvars.push(v(HVar { name: var.unwrap().name.clone() }))
                    }
                    halide_commands.push(HalideCommand::StoreOrder(v(hfunc.clone()), hvars))
                },
                FuncProperty::Memoize() => {
                    halide_commands.push(HalideCommand::Memoize(v(hfunc.clone())))
                },
                FuncProperty::AllowRaceConditions() => {
                    halide_commands.push(HalideCommand::AllowRaceConditions(v(hfunc.clone())))
                },
                FuncProperty::Async() => {
                    halide_commands.push(HalideCommand::Async(v(hfunc.clone())))
                },
            }
        }

        self.commands.append(&mut halide_commands)
    }
}

impl Target for HalideProgram {}

impl ToString for HalideProgram {
    fn to_string(&self) -> String {
        let mut result = String::new();
        for command in &self.commands {
            result.push_str(&command.to_string());
            result.push_str("\n");
        }
        result
    }
}

impl ToString for HalideCommand {
    fn to_string(&self) -> String {
        match self {
            HalideCommand::Vectorize(func, var) => format!("{}.vectorize({});", func.to_string(),
            var.to_string()),
            HalideCommand::Parallel(func, var) => format!("{}.parallel({});", func.to_string(),
            var.to_string()),
            HalideCommand::Unroll(func, var, factor) => format!("{}.unroll({}, {});", func.to_string(),
            var.to_string(), factor),
            HalideCommand::Tile() => String::from("X.tile()"),
            HalideCommand::ComputeAt(func1, func2, var) => {
                format!("{}.compute_at({}, {});", func1.to_string(), func2.to_string(), var.to_string())
            }
            HalideCommand::ComputeWith(func, var1, var2) => {
                format!("{}.compute_with({}, {})", func.to_string(), var1.to_string(), var2.to_string())
            }
            HalideCommand::StoreAt(func, func2, var) => {
                format!("{}.store_at({}, {});", func.to_string(), func2.to_string(), var.to_string())
            },
            HalideCommand::ComputeRoot(func) => {
                format!("{}.compute_root();", func.to_string())
            },
            // add cases for 
            HalideCommand::Reorder(func1, (var1, var2)) => {
                format!("{}.reorder({}, {});", func1.to_string(), var1.to_string(), var2.to_string())
            },
            HalideCommand::Split(func, var1, (var2, var3), factor) => {
                format!("{}.split({}, {}, {}, {});", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string(), factor.to_string())
            },
            HalideCommand::Fuse(func, (var1, var2), var3) => {
                format!("{}.fuse({}, {}, {});", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string())
            },
            HalideCommand::Prefetch(hbuf, hvar, hstride) => {
                format!("{}.prefetch({}, {})", hbuf.to_string(), hvar.to_string(), hstride.to_string())
            },
            HalideCommand::Memoize(hbuf) => {
                format!("{}.memoize()", hbuf.to_string())
            },
            HalideCommand::AllowRaceConditions(hbuf) => {
                format!("{}.allow_race_conditions()", hbuf.to_string())
            },
            HalideCommand::Async(hbuf) => {
                format!("{}.async()", hbuf.to_string())
            },
            HalideCommand::StoreOrder(hbuf, vars) => {
                format!("{}.storage_order({})", hbuf, vars.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", "))
            },
        }
    }
}

pub fn generate(_opts: &Options, program: HalideProgram) -> String {
    program.commands.iter().map(|command| command.to_string()).collect::<Vec<String>>().join("\n") + "\n"
}

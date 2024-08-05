use clap::{App, Arg};
use crate::gen::target::Backend;

pub struct Options {
	// Keep track of whether struct is initilzied --- apparently
	// the static way of keeping this around doesn't work in
	// multi-threaded programs, so keep this field around for
	// debugging.
	pub initialized: bool,

	// positional args -- these are files.
	pub source: String,
	pub target: String,
    pub reshapes: String,
    pub dest: String,

    // Other settings flags
    pub execution_dir: String,
    pub halide_dir: String,

    pub halide_program: String,

    pub backend: Backend,

	// debug flags
	pub debug_parser: bool,
	pub debug_synthesizer: bool,
    // Debug the reorder flag?
    pub debug_reorder: bool,
    pub debug_split: bool,
    pub debug_reshape: bool,
    pub debug_execution: bool,
    pub debug_opentuner: bool,

    pub debug_func_table: bool,
    pub debug_reorder_topo: bool,
    pub debug_ilp_solver: bool,

    // opentuner options
    pub opentuner_timeout: i32,
    pub no_opentuner: bool,
}

pub fn parse_options() -> Options {
    let args = App::new("SchedSynth")
        .version("1.0")
        .about("Synthesize Halide schedules from skeletons")
        .arg(
            Arg::with_name("source")
                .help("Source file")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("target")
                .help("Target file")
                .required(true)
                .index(2),
        )
        .arg(
            Arg::with_name("reshapes")
            .help("Reshapes file")
            .required(true)
            .index(3)
        )
        .arg(
            Arg::with_name("dest")
            .help("Destination file")
            .required(true)
            .index(4)
        )
        // optional string flag called execution_dir (used as --execution-dir)
        // with default 'execution'
        .arg(
            Arg::with_name("execution_dir")
            .long("execution-dir")
            .help("Execution directory")
            .takes_value(true)
            .default_value("execution")
        )
        .arg(
            Arg::with_name("halide_dir")
            .long("halide-dir")
            .help("Directory of Halide install")
            .takes_value(true)
            .default_value("halide")
        )
        .arg(
            Arg::with_name("halide_program")
            .long("halide-program")
            .help("The skeleton of the halide program into which we can execute syntheiszed schedules.")
            .takes_value(true)
            .default_value("halide.cpp")
        )
        .arg(
            Arg::new("debug-parser")
            .long("debug-parser")
            .help("debug the skeleton parser")
        )
		.arg(
			Arg::new("debug-synthesizer")
			.long("debug-synthesizer")
			.help("debug the synthesizer")
		)
		.arg(
			Arg::new("debug-reorder")
			.long("debug-reorder")
			.help("debug the reorder inference pass")
		)
		.arg(
			Arg::new("debug-reshape")
			.long("debug-reshape")
			.help("debug the reshape inference pass")
		)
		.arg(
			Arg::new("debug-split")
			.long("debug-split")
			.help("debug the split inference pass")
		)
		.arg(
			Arg::new("debug-execution")
			.long("debug-execution")
			.help("debug the execution pass")
		)
		.arg(
			Arg::new("debug-func-table")
			.long("debug-func-table")
			.help("debug the func table generation")
		)
		.arg(
			Arg::new("debug-reorder-topo")
			.long("debug-reorder-topo")
			.help("debug the reorder toposort pass")
		)
        .arg(
            Arg::new("debug-opentuner")
            .long("debug-opentuner")
            .help("Debug the interface with opentuner")
        )
        .arg(
            Arg::new("debug-ilp-solver")
            .long("debug-ilp-solver")
            .help("Debug the ilp solver interface")
        )
        .arg(
            Arg::new("opentuner-timeout")
            .long("opentuner-timeout")
            .help("Debug the interface with opentuner")
            .takes_value(true)
            .default_value("30")
        )
        .arg(
            Arg::new("no-opentuner")
            .long("no-opentuner")
            .help("Skip opentuner")
        )
        .get_matches();

    // initialize to defaults
    let opts: Options = Options {
        initialized: true,

        source: args.value_of("source").unwrap().into(),
        target: args.value_of("target").unwrap().into(),
        reshapes: args.value_of("reshapes").unwrap().into(),
        dest: args.value_of("dest").unwrap().into(),

        backend: Backend::Halide(),

        execution_dir: args.value_of("execution_dir").unwrap().into(),
        halide_dir: args.value_of("halide_dir").unwrap().into(),

        halide_program: args.value_of("halide_program").unwrap().into(),

        debug_parser: args.is_present("debug-parser"),
		debug_synthesizer: args.is_present("debug-synthesizer"),
        debug_reorder: args.is_present("debug-reorder"),
        debug_reshape: args.is_present("debug-reshape"),
        debug_split: args.is_present("debug-split"),
        debug_func_table: args.is_present("debug-func-table"),
        debug_execution: args.is_present("debug-execution"),
        debug_opentuner: args.is_present("debug-opentuner"),

        debug_reorder_topo: args.is_present("debug-reorder-topo"),
        debug_ilp_solver: args.is_present("debug-ilp-solver"),

        opentuner_timeout: args.value_of("opentuner-timeout").unwrap().parse().expect("Opentuner timeout must be an integer"),
        no_opentuner: args.is_present("no-opentuner"),
    };

    return opts;
}

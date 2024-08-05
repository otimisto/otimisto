use crate::options::options::Options;
use std::process::Command;
use std::fs::File;
use std::time::Instant;
use std::io::Read;
use std::io::Write;
use crate::gen::target::Target;
use crate::gen::target::HoleValue;
use crate::gen::target::HoleBindingMap;
use std::collections::HashMap;
// import fs in rust
use std::fs;

#[derive(Clone)]
pub struct ExecutionResult {
    pub execution_time: f32,
    pub exit_status: i32,
}

pub fn best_schedule<T: Target>(opts: &Options, schedules: Vec<T>) -> T {
    let mut bindings: Vec<HoleBindingMap> = vec![];

    // make the directoyr opts.execution_dir if it doesn't exist
    let _ = fs::create_dir_all(&opts.execution_dir);

    for schedule in schedules.iter() {
        let opentuner = build_opentuner(opts, schedule);

        // Run the opentuner and get the best assignment of variables:
        let binding =
            if schedule.get_holes().len() > 0 {
                if !schedule.can_resolve_holes(opts)  {
                    panic!("Need to provide enough flags to be able to test the programs if there are holes for opentuner");
                }
                if opts.debug_opentuner {
                    println!("Have opentuner holes, running opentuner");
                }
                run_opentuner(opts, opentuner)
            } else {
                if opts.debug_opentuner {
                    println!("No opentuner holes, generating schedule...");
                }
                // No opentuenr holes, skip.
                HoleBindingMap {
                    map: HashMap::new()
                }
            };

        // Setup the bindings for this opentuner.
        bindings.push(binding);
    }

    if schedules.len() == 0 {
        panic!("Trying to get best schedule form empty list");
    } else if schedules.len() == 1 {
        let mut result_schedule = schedules[0].clone();
        result_schedule.fill_holes(&bindings[0]);
        return result_schedule;
    } else {
        // TODO -- fill holes.
        let string_schedules = schedules.iter().map(|x| x.generate()).collect();
        let rankings = evaluate_options(opts, string_schedules);
        let best_program = get_best_ranking(&rankings);
        schedules[best_program as usize].clone()
    }
}

// Build an opentuner wrapper for this program.
pub fn build_opentuner<T: Target>(opts: &Options, schedule: &T) -> String {
    let header = "#!/usr/bin/env python
#
# Autotune flags to g++ to optimize the performance of apps/raytracer.cpp
#
# This is an extremely simplified version meant only for tutorials
#

import opentuner
import os
from opentuner import ConfigurationManipulator
from opentuner import EnumParameter
from opentuner import IntegerParameter
from opentuner import MeasurementInterface
from opentuner import Result

defs = [ NAME_HOLE ]
filename = \"FILE_NAME\"

class InstanceTuner(MeasurementInterface):
    def manipulator(self):
        manipulator = ConfigurationManipulator()
        MANIPULATOR_HOLE
        return manipulator

    def compile(self, cfg, id):
        gcc_cmd = 'g++ ' + filename + ' '

        for variable_def in defs:
            gcc_cmd += ' -D{0}=\"{1}\"'.format(variable_def, cfg[variable_def])

        gcc_cmd += ' -o tmp' + str(id) + '.bin'
        gcc_cmd += ' EXTRA_BUILD_FLAGS'

        # run gcccmd
        os.system(gcc_cmd)

    def run_precompiled(self, input, limit, compile_result, id):
        try:
            run_result = self.call_program('./tmp{0}.bin'.format(id))
            return Result(time=run_result['time'])
        except e:
            print('Warning: Run failed with error ', e)
            return Result(time=999999999999)

    def save_final_config(self, configuration):
        print (configuration.data)

    def compile_and_run(self, desired_result, input, limit):
        cfg = desired_result.configuration.data

        compile_result = self.compile(cfg, 0)
        return self.run_precompiled(input, limit, compile_result, 0)

if __name__ == '__main__':
    argparser = opentuner.default_argparser()
    InstanceTuner.main(argparser.parse_args())
";

    let holes = schedule.get_holes();
    let mut manipulator = "".to_string();
    let mut names = "".to_string();
    for hole in holes {
        manipulator.push_str(&hole.to_opentuner(&"manipulator".to_string()));

        names.push_str(&("\"".to_owned() + &hole.get_name() + "\""));
        names.push_str(", ");
    };

    // Copy the file named by opts.halide_program into
    // 'runnable.cpp'
    let new_runnable_name = opts.execution_dir.clone() + "/runnable.cpp";
    let mut new_runnable = File::create(&new_runnable_name).unwrap();
    let mut orig_runnable = match File::open(&opts.halide_program) {
        Ok(f) => f,
        Err(_e) => panic!("Make sure that a halide template is in {} (or set with --halide-program", opts.halide_program),
    };

    // Replace the SCHED_CONTENT -- if 
    let mut content = String::new();
    orig_runnable.read_to_string(&mut content).unwrap();
    // If SCHED_CONTENT is not in content, then panic
    if !content.contains("SCHED_CONTENT") {
        panic!("SCHED_CONTENT not found in {}.  You need to have a full template file with the defined behaviour where the generated schedule can be inserted (indicated by SCHED_CONTENT). ", &opts.halide_program);
    }
    // Generate the hole-y version and put it in the file.
    let content_with_holes = content.replace("SCHED_CONTENT", &schedule.generate());
    new_runnable.write_all(content_with_holes.as_bytes()).unwrap();

    // Fill up the opentuner template above.
    let with_names = header.replace("NAME_HOLE", &names);
    let with_filename = with_names.replace("FILE_NAME", &new_runnable_name);
    let flags = schedule.get_required_build_flags(opts).join(" ");
    let with_flags = with_filename.replace("EXTRA_BUILD_FLAGS", &flags);
    return with_flags.replace("MANIPULATOR_HOLE", &manipulator);
}


// Run the opentuner string, and get a binding map back.
fn run_opentuner(opts: &Options, opentuner_string: String) -> HoleBindingMap {
    // Produce a file for the opentuner.
    let filename = opts.execution_dir.clone() + "/opentuner_run.py";
    // Write opentuner_string into filename.

    if opts.debug_opentuner {
        println!("writing into file {}", filename.clone());
    }
    let mut file = File::create(filename.clone()).unwrap();
    file.write_all(opentuner_string.as_bytes()).unwrap();

    // Run opentuner.
    let output = Command::new("python3")
        .arg(filename)
        .arg("--stop-after=".to_owned() + &opts.opentuner_timeout.to_string())
        .output()
        .expect("failed to execute process");

    // Parse the output of opentuner, which will give
    // us the mappings.
    let output_string = String::from_utf8_lossy(&output.stdout);
    let error_string = String::from_utf8_lossy(&output.stderr);
    let return_code = output.status.code().unwrap();

    // Return the mappings
    // if the return code is not 0, then something went wrong --- need to inform
    // programmer
    if return_code != 0 {
        println!("Error running opentuner: {}", error_string);
        panic!();
    }

    // Parse the output of opentuner.

    if opts.debug_opentuner {
        println!("Output string is {}", output_string);
        println!("Output error is {}", error_string);
    }

    mappings_from_opentuner_output(output_string.to_string())
}

fn mappings_from_opentuner_output(output_string: String) -> HoleBindingMap {
    // Split output_string into lines.  Each line is of the form
    // name: value.
    // Put those into the map in a HoleBindingMap.
    let mut map = HashMap::<String, HoleValue>::new();
    // parse json object from the string, which looks like '{ ... }'
    // TODO --- parse the json right into the hashmap.
    let formatted = output_string.replace("\'", "\"");
    let data: HashMap<String, i32> = serde_json::from_str(&formatted).unwrap();
    // iterate over the json, creating a HoleValue to put 
    // in the map for every element.
    for (key, value) in data {
        // check if value is an int
        // create a HoleValue with the int
        let hole_value = HoleValue::IntHole(value);
        // insert the HoleValue into the map
        map.insert(key.to_string(), hole_value);
    }

    HoleBindingMap { map: map }
}

// Get the best ranking execution result
fn get_best_ranking(exec_results: &Vec<ExecutionResult>) -> i32 {
    let mut best_ranking = -1;
    // Set as FP infinity
    let mut best_time: f32 = std::f32::INFINITY;

    for (i, result) in exec_results.iter().enumerate() {
        if result.execution_time < best_time {
            best_ranking = i as i32;
            best_time = result.execution_time;
        }
    }

    if best_ranking < 0 {
        panic!("Error: all executions failed!");
    }
    best_ranking
}

pub fn evaluate_options(opts: &Options, schedules: Vec<String>) -> Vec<ExecutionResult> {
    let mut results = Vec::new();
    let mut number = 0;

    for schedule in schedules {
        let temp_file = format!("{}.c", number);
        create_runnable(opts, opts.halide_program.clone(), temp_file.clone(), schedule);
        let runnable = build_runnable(opts, temp_file.clone());
        let result = execute_runnable(opts, runnable);
        results.push(result.clone());

        number = number + 1;
    }

    results
}

// Read in the file in template_file,
// then write it out into opts.execution_dir with
// the SCHED_CONTENT string replaced with the schedule
// Return the (unique
fn create_runnable(opts: &Options, template_file: String, target_file: String, schedule: String) {
    // check if opts.execution_dir is a directory -- if not, create it.
    let execution_dir = std::path::PathBuf::from(&opts.execution_dir);
    if !execution_dir.exists() {
        std::fs::create_dir_all(&execution_dir).expect("Failed to create execution directory");
    }

    let mut template_contents = String::new();
    let target_file = format!("{}/{}", opts.execution_dir.clone(), target_file);

    let mut file = File::open(&template_file).expect(format!("Unable to open template file {}.  Use --halide-program to specify a template that can be filled by the scheduler", template_file).as_str());
    file.read_to_string(&mut template_contents).expect("Unable to read template file");
    let new_contents = template_contents.replace("SCHED_CONTENT", &schedule);
    let mut target_file = File::create(&target_file).expect("Unable to create target file");
    target_file.write_all(new_contents.as_bytes()).expect("Unable to write to target file");
}

// Build the file C file using the Halide build command g++ test.c -I<opts.halide_dir>/include -L<opts.halide_dir>lib -lHalide -lpthread -ldl
fn build_runnable(opts: &Options, target_file: String) -> String {
    let target_file = format!("{}/{}", opts.execution_dir.clone(), target_file);
    let mut halide_include_dir = opts.halide_dir.clone();
    halide_include_dir.push_str("include");
    let mut halide_lib_dir = opts.halide_dir.clone();
    halide_lib_dir.push_str("lib");
    let mut command = String::from("g++ ");
    command.push_str(&target_file);
    command.push_str(" -I");
    command.push_str(&halide_include_dir);
    command.push_str(" -L");
    command.push_str(&halide_lib_dir);
    command.push_str(" -lHalide -lpthread -ldl");
    command.push_str(" -o");
    command.push_str(&target_file);
    command.push_str(".o");
    let output = Command::new("sh")
        .arg("-c")
        .arg(&command)
        .output()
        .expect("Failed to execute build command");
    if opts.debug_execution {
        println!("Compile output: {:?}", output);
    }

    return format!("{}.o", target_file);
}

fn execute_runnable(opts: &Options, executable_name: String) -> ExecutionResult {
    // TODO -- parse the output of the halide runner rather tahan
    // using the time measurement here.
    let start_time = Instant::now();

    let output = Command::new(executable_name)
        .output()
        .expect("failed to execute process");

    let end_time = Instant::now();

    let execution_time = end_time.duration_since(start_time).as_secs_f32();

    if opts.debug_execution {
        println!("Execution Output: {:?}", output);
    }

    ExecutionResult {
        execution_time: execution_time,
        // get the i32 from ExitStatus
        exit_status: output.status.code().unwrap_or(-1),
    }
}

/// Entry point of the innocence interpreter.
use std::{env, process};

use innocence_lib::{run_file, run_prompt};

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if let Err(err_code) = match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => panic!("Usage: innocence [script file path]"),
    } {
        process::exit(err_code);
    } else {
        process::exit(0);
    }
}

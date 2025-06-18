/// Entry point of the innocence interpreter.
mod environment;
mod expressions;
mod interpreter;
mod logger;
mod parser;
mod scanner;
mod semantic_analysis;
mod statements;
mod tokens;
mod utilities;

use std::{env, process};

use innocence_lang::{run_file, run_prompt};

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

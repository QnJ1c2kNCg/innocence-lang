/// Entry point of the innocence interpreter, through the [`run_file`] and [`run_prompt`] functions.
mod environment;
mod expressions;
mod interpreter;
mod logger;
mod parser;
mod prelude;
mod scanner;
mod semantic_analysis;
mod statements;
mod tokens;
mod utilities;

use std::io::Write;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

use crate::semantic_analysis::run_semantic_analysis;

pub fn run_file(file_path: &str) -> Result<(), i32> {
    let source = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read {}", file_path));
    let mut interpreter = Interpreter::new();
    run(&mut interpreter, source)
}

/// This is used for the read-evaluate-print-loop (REPL).
pub fn run_prompt() -> Result<(), i32> {
    let stdin = std::io::stdin();
    println!("Welcome to innocence's REPL");
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        let _ = std::io::stdout().flush();
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();
        run(&mut interpreter, line)?;
    }
}

fn run(interpreter: &mut Interpreter, source: String) -> Result<(), i32> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    let parser = Parser::new(tokens);
    let statements = parser.parse().map_err(|err| {
        eprintln!("Parser error: {:?}", err);
        1
    })?;

    //run_semantic_analysis(&statements).map_err(|err| {
    //eprintln!("{}", err);
    //1
    //})?;

    interpreter.interpret(&statements).map_err(|err| {
        eprintln!("Interpreter error: {:?}", err);
        1
    })?;

    Ok(())
}

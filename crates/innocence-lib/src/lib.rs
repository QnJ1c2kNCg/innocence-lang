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

use std::io::Write;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

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
    let statements = match parser.parse() {
        Ok(stmt) => stmt,
        Err(err) => {
            eprintln!("Parser error: {:?}", err);
            return Err(1);
        }
    };
    interpreter.interpret(&statements).map_err(|err| {
        eprintln!("Interpreter error: {:?}", err);
        1
    })
}

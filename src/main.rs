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

use std::{env, io::Write};

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => panic!("Usage: innocence [script file path]"),
    }
}

fn run_file(file_path: &String) {
    let source = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read {}", file_path));
    let mut interpreter = Interpreter::new();
    run(&mut interpreter, source);
}

/// This is used for the read-evaluate-print-loop (REPL).
fn run_prompt() {
    let stdin = std::io::stdin();
    println!("Welcome to innocence's REPL");
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        let _ = std::io::stdout().flush();
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();
        run(&mut interpreter, line);
    }
}

fn run(interpreter: &mut Interpreter, source: String) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(stmt) => stmt,
        Err(_) => {
            println!("failed to parse!");
            return;
        }
    };
    interpreter.interpret(&statements).unwrap();

    // let ast_root_expr = parser.parse().unwrap();

    // let mut ast_stringer = AstStringer {};

    // println!("AST view:");
    // println!("{}", ast_stringer.stringify(&ast_root_expr));

    // println!("Interpreter:");
    // let mut interpreter =Interpreter::new();
    // let interpreted = interpreter.interpret(&ast_root_expr);
    // println!("{:?}\n", interpreted);
}

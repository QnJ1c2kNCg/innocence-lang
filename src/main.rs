mod ast_stringer;
mod expressions;
mod parser;
mod scanner;
mod tokens;

use std::{env, io::Write};

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
    let source =
        std::fs::read_to_string(file_path).expect(&format!("Failed to read {}", file_path));
    run(source);
}

fn run_prompt() {
    let stdin = std::io::stdin();
    println!("Welcome to innocence's REPL");
    loop {
        print!("> ");
        let _ = std::io::stdout().flush();
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();
        run(line);
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(source);
    println!("{:?}", scanner.scan_tokens());
}

mod scanner;

use std::env;

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
    let scanner = Scanner::new(file_path);
    println!("{:?}", scanner.tokens());
}

fn run_prompt() {
    todo!("Implement REPL");
}

use std::{env, process::exit};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: innocence [script]");
            exit(64);
        }
    }
}

fn run_file(args: &String) {
    todo!()
}

fn run_prompt() {
    todo!()
}

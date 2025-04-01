use std::{cmp, env, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len().cmp(&2) {
        cmp::Ordering::Less => process::exit(run_repl()),
        cmp::Ordering::Equal => process::exit(run_script(&args[1])),
        cmp::Ordering::Greater => {
            eprintln!("Usage: lox [SCRIPT]");
            process::exit(64);
        }
    }
}

fn run_repl() -> i32 {
    0
}

fn run_script(script: &str) -> i32 {
    0
}

mod ast_interpreter;

use std::{
    cmp, env,
    io::{Write, stdin, stdout},
    process,
};

use ast_interpreter::LoxInterpreter;
use loxc::ast::LoxAST;

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
    let mut line = String::new();
    loop {
        line.clear();
        print!("lox > ");
        stdout().flush().unwrap();
        let n = stdin().read_line(&mut line).expect("failed to read input");
        if n == 0 {
            println!("\nbye!");
            break;
        }
        match LoxAST::new(&line) {
            Ok(ast) => {
                let mut interpreter = LoxInterpreter;
                print!("  ");
                stdout().flush().unwrap();
                interpreter.interpret(ast);
            }
            Err(err) => println!("{}", err),
        }
    }
    0
}

fn run_script(_script: &str) -> i32 {
    0
}

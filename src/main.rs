mod ast;
mod error;
mod parser;
mod visitor;

use std::{
    env, fs,
    io::{Read, Write, stdin, stdout},
    process,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len().cmp(&2) {
        std::cmp::Ordering::Less => run_repl(),
        std::cmp::Ordering::Equal => run_script(args.get(1).unwrap()),
        std::cmp::Ordering::Greater => {
            println!("Usage: rlox [script]");
            process::exit(64);
        }
    }
}

fn run_repl() {
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush().unwrap();

        line.clear();
        match stdin().read_line(&mut line) {
            Ok(n) => {
                if n == 0 {
                    break;
                }
                let _ = run(&line);
            }
            Err(err) => eprintln!("{}", err),
        }
    }
}

fn run_script(path: &str) {
    let mut file = fs::File::open(path).unwrap();
    let mut fstr = String::new();
    file.read_to_string(&mut fstr).unwrap();
    if run(&fstr).is_err() {
        process::exit(65);
    }
}

fn run(input: &str) -> Result<(), error::Error> {
    let tree = parser::Parser::parse(input)?;
    ast::util::ASTPrinter::print(tree.root(), &tree);
    Ok(())
}

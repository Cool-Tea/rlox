mod ast;
mod class;
mod environment;
mod error;
mod function;
mod instance;
mod interpreter;
mod parser;
mod value;
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
                line.pop(); // remove line separator
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
    let ast = parser::Parser::parse(input)?;
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.interpret(&ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_var() {
        let input = "var a = 0;
        a = 2;
        print a;";

        assert!(run(input).is_ok());
    }

    #[test]
    fn test_block() {
        let input = "var a = \"global a\";
            var b = \"global b\";
            var c = \"global c\";
            {
                var a = \"outer a\";
                var b = \"outer b\";
                {
                    var a = \"inner a\";
                    print a;
                    print b;
                    print c;
                }
                print a;
                print b;
                print c;
            }
            print a;
            print b;
            print c;";

        assert!(run(input).is_ok());
    }

    #[test]
    fn test_short_circuit() {
        assert!(run("print \"hi\" or 2;").is_ok());
        assert!(run("print nil or \"yes\";").is_ok());
    }

    #[test]
    fn test_control_flow() {
        let fib = "var a = 0;
            var temp;
            for (var b = 1; a < 10000; b = temp + b) {
                print a;
                temp = a;
                a = b;
            }";
        assert!(run(fib).is_ok());
    }

    #[test]
    fn test_func() {
        let hi = "fun sayHi(first, last) {
                print \"Hi, \" + first + \" \" + last + \"!\";
            }
            sayHi(\"Dear\", \"Reader\");";
        assert!(run(hi).is_ok());

        let ret = "fun fib(n) {
                if (n <= 1) return n;
                return fib(n - 2) + fib(n - 1);
            }
            for (var i = 0; i < 20; i = i + 1) {
                print fib(i);
            }";
        assert!(run(ret).is_ok());

        let native = "print clock();";
        assert!(run(native).is_ok());
    }

    #[test]
    fn test_closure() {
        let local = "fun makeCounter() {
                var i = 0;
                fun count() {
                    i = i + 1;
                    print i;
                }

                return count;
            }
            var c1 = makeCounter();
            var c2 = makeCounter();
            c1();
            c1();
            c2();";
        assert!(run(local).is_ok());

        let input = "var a = \"global\";
            {
                fun showA() {
                    print a;
                }
                showA();

                var a = \"block\";
                showA();
            }";
        assert!(run(input).is_ok());
    }
}

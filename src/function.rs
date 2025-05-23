use std::cell::RefCell;
use std::fmt::Debug;
use std::iter::zip;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::Environment;
use crate::error::Error;
use crate::interpreter::Interpreter;
use crate::value::Value;

pub trait Callable: Debug {
    fn arity(&self) -> usize;
    fn call(
        &self,
        args: Vec<Value>,
        interpreter: &mut Interpreter,
        env: &AST,
    ) -> Result<Value, Error>;
    fn identifier(&self) -> String {
        "native".to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    decl: FuncStmt,
    closure: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(decl: &FuncStmt, closure: Rc<RefCell<Environment>>) -> Self {
        Function {
            decl: decl.clone(),
            closure,
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.decl.params.len()
    }

    fn call(
        &self,
        args: Vec<Value>,
        interpreter: &mut Interpreter,
        env: &AST,
    ) -> Result<Value, Error> {
        let mut e = Environment::new(Some(self.closure.clone()));
        for (param, arg) in zip(&self.decl.params, args) {
            e.define(param.lexeme.clone(), arg);
        }

        let body = match env.get_stmt(self.decl.body).unwrap() {
            Stmt::Block(block) => block,
            _ => unreachable!(),
        };

        match interpreter.execute_block(e, &body.stmts, env) {
            Ok(_) => Ok(Value::Nil),
            Err(e) => match e {
                Error::Return(value) => Ok(value),
                _ => Err(e),
            },
        }
    }

    fn identifier(&self) -> String {
        self.decl.name.lexeme.clone()
    }
}

pub mod native {
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::error::RtError;

    use super::*;

    #[derive(Debug, Clone, Copy)]
    pub struct ClockFn;

    impl Callable for ClockFn {
        fn arity(&self) -> usize {
            0
        }

        fn call(&self, _: Vec<Value>, _: &mut Interpreter, _: &AST) -> Result<Value, Error> {
            match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(duration) => Ok(Value::Number(duration.as_secs_f64())),
                Err(e) => {
                    println!("Error: {:?}", e);
                    Err(Error::Runtime(RtError::Other(
                        "Failed to get current time".to_string(),
                    )))
                }
            }
        }

        fn identifier(&self) -> String {
            "clock".to_string()
        }
    }
}

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Token;
use crate::error::Error;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Environment {
    envs: Rc<RefCell<Vec<Environment>>>,
    enclosing: Option<usize>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(envs: Rc<RefCell<Vec<Environment>>>, enclosing: Option<usize>) -> Self {
        Environment {
            envs,
            enclosing,
            values: HashMap::new(),
        }
    }

    fn report(line: usize, col: usize, content: &str, msg: String) -> Error {
        Error::report(line, col, content, msg);
        Error::Runtime
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<Value, Error> {
        if self.values.contains_key(&name.lexeme) {
            Ok(self.values.get(&name.lexeme).unwrap().clone())
        } else if let Some(enclosing) = self.enclosing {
            let envs = self.envs.borrow();
            let enclosing = envs.get(enclosing).unwrap();
            enclosing.get(name)
        } else {
            Err(Self::report(
                name.line,
                name.col,
                &name.lexeme,
                format!("Undefined variable {}.", name.lexeme),
            ))
        }
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), Error> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        } else if let Some(enclosing) = self.enclosing {
            let mut envs = self.envs.borrow_mut();
            let enclosing = envs.get_mut(enclosing).unwrap();
            enclosing.assign(name, value)
        } else {
            Err(Self::report(
                name.line,
                name.col,
                &name.lexeme,
                format!("Undefined variable {}.", name.lexeme),
            ))
        }
    }
}

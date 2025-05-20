use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Token;
use crate::error::Error;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    fn report(line: usize, col: usize, content: &str, msg: String) -> Error {
        Error::report(line, col, content, msg);
        Error::Runtime
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<Value, Error> {
        if self.values.contains_key(&name.lexeme) {
            Ok(self.values.get(&name.lexeme).unwrap().clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
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
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Token;
use crate::error::{Error, RtError};
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Rc<RefCell<Value>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    fn report<T>(err: Error) -> Result<T, Error> {
        err.report();
        Err(err)
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn define(&mut self, name: String, value: Value) -> Result<(), Error> {
        if self.values.contains_key(&name) {
            return Self::report(Error::Semantic(crate::error::SemError::RepeatDefine));
        }
        self.values.insert(name, Rc::new(RefCell::new(value)));
        Ok(())
    }

    pub fn get(&self, name: &Token) -> Result<Rc<RefCell<Value>>, Error> {
        if self.values.contains_key(&name.lexeme) {
            Ok(self.values.get(&name.lexeme).unwrap().clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Self::report(Error::Runtime(RtError::UndefinedVariable(
                name.lexeme.clone(),
            )))
        }
    }
}

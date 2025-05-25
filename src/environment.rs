use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, RtError, SemError, report};
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

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn define(&mut self, name: String, value: Value) -> Result<(), Error> {
        if self.values.contains_key(&name) {
            return report(Error::Semantic(crate::error::SemError::RepeatDefine));
        }
        self.values.insert(name, Rc::new(RefCell::new(value)));
        Ok(())
    }

    pub fn get(&self, name: String) -> Result<Rc<RefCell<Value>>, Error> {
        if self.values.contains_key(&name) {
            Ok(self.values.get(&name).unwrap().clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            if name == "this" {
                report(Error::Semantic(SemError::InvalidThis))
            } else if name == "super" {
                report(Error::Semantic(SemError::InvalidSuper))
            } else {
                report(Error::Runtime(RtError::UndefinedVariable(name.clone())))
            }
        }
    }

    pub fn contain(&self, name: String) -> bool {
        if self.values.contains_key(&name) {
            return true;
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().contain(name);
        }
        false
    }
}

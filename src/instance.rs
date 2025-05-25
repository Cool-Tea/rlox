use crate::class::Class;
use crate::error::Error;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<RefCell<Class>>,
    pub fields: HashMap<String, Rc<RefCell<Value>>>,
}

impl Instance {
    pub fn new(class: Rc<RefCell<Class>>) -> Self {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, Error> {
        if let Some(value) = self.fields.get(name) {
            Ok(value.clone())
        } else {
            self.class.borrow().find_method(name)
        }
    }

    pub fn set(&mut self, name: String, value: Rc<RefCell<Value>>) {
        self.fields.insert(name, value);
    }
}

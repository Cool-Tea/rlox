use crate::class::Class;
use crate::error::Error;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Instance {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, Error> {
        if let Some(value) = self.fields.borrow().get(name) {
            Ok(value.clone())
        } else {
            self.class.find_method(name)
        }
    }

    pub fn set(&self, name: String, value: Rc<RefCell<Value>>) {
        self.fields.borrow_mut().insert(name, value);
    }
}

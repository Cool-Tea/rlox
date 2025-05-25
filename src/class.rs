use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{AST, ClassStmt},
    environment::Environment,
    error::{Error, RtError},
    function::{Callable, Function},
    instance::Instance,
    interpreter::Interpreter,
    value::Value,
};

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub methods: HashMap<String, Rc<Function>>,
    pub init: Option<Rc<Function>>,
}

impl Class {
    pub fn new(decl: &ClassStmt) -> Result<Rc<Self>, Error> {
        Ok(Rc::new(Class {
            name: decl.name.lexeme.clone(),
            superclass: None,
            methods: HashMap::new(),
            init: None,
        }))
    }

    fn report<T>(err: Error) -> Result<T, Error> {
        err.report();
        Err(err)
    }

    pub fn find_method(&self, name: &str) -> Result<Rc<RefCell<Value>>, Error> {
        if let Some(method) = self.methods.get(name) {
            return Ok(Rc::new(RefCell::new(Value::Function(method.clone()))));
        }
        if let Some(superclass) = &self.superclass {
            return superclass.find_method(name);
        }
        Self::report(Error::Runtime(RtError::UndefinedMember(name.to_string())))
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        if let Some(init) = &self.init {
            init.arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        _args: Vec<Rc<RefCell<Value>>>,
        _interpreter: &mut Interpreter,
        _ast: &AST,
    ) -> Result<Rc<RefCell<Value>>, Error> {
        Ok(Rc::new(RefCell::new(Value::Instance(Instance::new(
            Rc::new(RefCell::new(self.clone())),
        )))))
    }

    fn identifier(&self) -> String {
        self.name.to_string()
    }
}

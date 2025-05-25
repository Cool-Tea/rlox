use crate::class::Class;
use crate::instance::Instance;
use std::fmt::Display;
use std::rc::Rc;

use crate::function::Callable;

#[derive(Debug, Clone)]
pub enum Value {
    Function(Rc<dyn Callable>),
    Class(Rc<Class>),
    Instance(Instance),
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(l0), Self::Function(r0)) => l0.identifier() == r0.identifier(),
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Function(clos) => write!(f, "<fn {}>", clos.identifier()),
            Value::Class(class) => write!(f, "<class {}>", class.identifier()),
            Value::Instance(instance) => write!(f, "<instance {}>", instance.class.borrow().name),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(f64),
    String(String),
    Object(HashMap<String, LoxValue>),
}

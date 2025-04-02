#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

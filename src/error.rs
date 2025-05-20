use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Error {
    Parse,
    Semantic,
    Runtime,
    Return(Value), //a hack to implement return in function
}

impl Error {
    pub fn report(line: usize, col: usize, content: &str, msg: String) {
        println!("[{}:{}] Error at '{}': {}", line, col, content, msg);
    }
}

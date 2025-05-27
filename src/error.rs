use crate::value::Value;

#[derive(Debug, Clone)]
pub enum SemError {
    RetFromTop,
    // JmpOutsideLoop(String),
    RepeatDefine,
    InvalidInheritance,
    InvalidSuper,
    InvalidThis,
}

#[derive(Debug, Clone)]
pub enum RtError {
    DivideByZero,
    UndefinedVariable(String),
    UndefinedMember(String),
    CallNonCallable,
    InvalidArgsNumber(usize, usize),
    TypeMismatch,
    StackOverflow,
    Other(String),
}

#[derive(Debug, Clone)]
pub enum Error {
    Parse(String, String),
    Semantic(SemError),
    Runtime(RtError),
    Return(Value), //a hack to implement return in function
}

impl Error {
    pub fn report(&self) {
        match self {
            Error::Parse(token, msg) => {
                println!("Error at '{}': {}.", token, msg);
            }
            Error::Semantic(err) => {
                println!(
                    "Error: {}.",
                    match err {
                        SemError::RetFromTop => "Can't return from top-level code",
                        // SemError::JmpOutsideLoop(s) =>
                        //     format!("Can't use ' {} ' outside of a loop", s),
                        SemError::RepeatDefine => "Already a variable with this name in this scope",
                        SemError::InvalidInheritance => "Superclass must be a class",
                        SemError::InvalidSuper => "Can't use 'super' in a class with no superclass",
                        SemError::InvalidThis => "Can't use 'this' outside of a class",
                    }
                )
            }
            Error::Runtime(err) => {
                println!(
                    "RuntimeError: {}.",
                    match err {
                        RtError::DivideByZero => "Division by zero".to_string(),
                        RtError::UndefinedVariable(s) => format!("Undefined variable '{}'", s),
                        RtError::UndefinedMember(s) => format!("Undefined property '{}'", s),
                        RtError::CallNonCallable =>
                            "Can only call functions and classes".to_string(),
                        RtError::InvalidArgsNumber(got, expected) =>
                            format!("Expected {} arguments but got {}", expected, got),
                        RtError::TypeMismatch =>
                            "Operands must be two numbers or two strings".to_string(),
                        RtError::StackOverflow => "Stack overflow".to_string(),
                        RtError::Other(s) => s.clone(),
                    }
                );
            }
            _ => {
                unreachable!();
            } //println!("[{}:{}] Error at '{}': {}", line, col, content, msg);
        }
    }
}

pub fn report<T>(err: Error) -> Result<T, Error> {
    err.report();
    Err(err)
}

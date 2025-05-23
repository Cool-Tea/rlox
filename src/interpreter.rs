use core::ops::Drop;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use crate::ast::*;
use crate::environment::Environment;
use crate::error::Error;
use crate::error::RtError;
use crate::function::*;
use crate::parser::Rule;
use crate::value::Value;
use crate::visitor::*;

#[derive(Debug, Clone)]
pub struct Interpreter {
    clos_encl: Vec<Weak<RefCell<Environment>>>, // enclosing environment of closure
    global: Rc<RefCell<Environment>>,
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Rc::new(RefCell::new(Environment::new(None)));
        let clock_fn = native::ClockFn;
        global
            .borrow_mut()
            .define(clock_fn.identifier(), Value::Function(Rc::new(clock_fn)));
        Interpreter {
            clos_encl: Vec::new(),
            global: global.clone(),
            env: global.clone(),
        }
    }

    pub fn interpret(&mut self, ast: &AST) -> Result<(), Error> {
        for &stmt in ast.entries() {
            self.visit_stmt(stmt, ast)?;
        }
        Ok(())
    }

    fn report<T>(err: Error) -> Result<T, Error> {
        err.report();
        Err(err)
    }

    fn define(&mut self, name: String, value: Value) {
        self.env.borrow_mut().define(name, value);
    }

    fn new_env(&self) -> Environment {
        Environment::new(Some(self.env.clone()))
    }

    pub fn execute_block(
        &mut self,
        env: Environment,
        stmts: &[usize],
        ast: &AST,
    ) -> Result<(), Error> {
        let prev = self.env.clone();
        self.env = Rc::new(RefCell::new(env));

        for &stmt in stmts {
            if let Err(e) = self.visit_stmt(stmt, ast) {
                self.env = prev;
                return Err(e);
            }
        }

        self.env = prev;
        Ok(())
    }
}

fn as_number(value: Value, op: &Token) -> Result<f64, Error> {
    match value {
        Value::Number(f) => Ok(f),
        _ => {
            let err = Error::Runtime(RtError::TypeMismatch);
            err.report();
            Err(err)
        }
    }
}

impl ExprVisitor<Result<Value, Error>> for Interpreter {
    fn visit_assign(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Assign(expr) => expr,
            _ => unreachable!(),
        };

        let value = self.visit_expr(expr.value, env)?;

        self.env.borrow_mut().assign(&expr.name, value.clone())?;

        Ok(value)
    }

    fn visit_binary(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Binary(expr) => expr,
            _ => unreachable!(),
        };
        let lhs = self.visit_expr(expr.lhs, env)?;
        let rhs = self.visit_expr(expr.rhs, env)?;

        Ok(match expr.op.rule {
            Rule::Minus => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Number(lhs - rhs)
            }
            Rule::Star => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Number(lhs * rhs)
            }
            Rule::Slash => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Number(lhs / rhs)
            }
            Rule::Plus => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                (Value::String(lhs), Value::String(rhs)) => Value::String(lhs + &rhs),
                _ => Value::Nil,
            },
            Rule::Greater => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Bool(lhs > rhs)
            }
            Rule::GreaterEqual => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Bool(lhs >= rhs)
            }
            Rule::Less => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Bool(lhs < rhs)
            }
            Rule::LessEqual => {
                let lhs = as_number(lhs, &expr.op)?;
                let rhs = as_number(rhs, &expr.op)?;
                Value::Bool(lhs <= rhs)
            }
            Rule::Equal => Value::Bool(lhs == rhs),
            Rule::BangEqual => Value::Bool(lhs != rhs),
            _ => Value::Nil,
        })
    }

    fn visit_call(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Call(expr) => expr,
            _ => unreachable!(),
        };

        let callee = self.visit_expr(expr.callee, env)?;
        let mut args = Vec::new();
        for &arg in expr.args.iter() {
            let arg = self.visit_expr(arg, env)?;
            args.push(arg);
        }

        if let Value::Function(func) = callee {
            if func.arity() != args.len() {
                Self::report(Error::Runtime(RtError::InvalidArgsNumber(
                    args.len(),
                    func.arity(),
                )))
            } else {
                func.call(args, self, env)
            }
        } else {
            Self::report(Error::Runtime(RtError::CallNonCallable))
        }
    }

    fn visit_grouping(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Grouping(expr) => expr,
            _ => unreachable!(),
        };

        self.visit_expr(expr.expr, env)
    }

    fn visit_literal(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Literal(expr) => expr,
            _ => unreachable!(),
        };

        Ok(match expr {
            Literal::Number(f) => Value::Number(*f),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
        })
    }

    fn visit_logical(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Logical(expr) => expr,
            _ => unreachable!(),
        };
        let lhs = self.visit_expr(expr.lhs, env)?;
        match expr.op.rule {
            Rule::Or => {
                if lhs.is_truthy() {
                    return Ok(lhs);
                }
            }
            Rule::And => {
                if !lhs.is_truthy() {
                    return Ok(lhs);
                }
            }
            _ => {
                unreachable!();
            }
        }
        self.visit_expr(expr.rhs, env)
    }

    fn visit_unary(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Unary(expr) => expr,
            _ => unreachable!(),
        };

        let rhs = self.visit_expr(expr.rhs, env)?;

        match expr.op.rule {
            Rule::Minus => match rhs {
                Value::Number(f) => Ok(Value::Number(-f)),
                _ => Self::report(Error::Runtime(RtError::TypeMismatch)),
            },
            Rule::Bang => Ok(Value::Bool(!rhs.is_truthy())),
            _ => Ok(Value::Nil),
        }
    }

    fn visit_variable(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        let expr = match env.get_expr(expr).unwrap() {
            Expr::Variable(expr) => expr,
            _ => unreachable!(),
        };

        self.env.borrow().get(&expr.name)
    }
}

impl StmtVisitor<Result<(), Error>> for Interpreter {
    fn visit_block(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Block(stmt) => stmt,
            _ => unreachable!(),
        };
        let e = self.new_env();
        self.execute_block(e, &stmt.stmts, env)
    }

    fn visit_expr_stmt(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Expr(stmt) => stmt,
            _ => unreachable!(),
        };
        self.visit_expr(stmt.expr, env)?;
        Ok(())
    }

    fn visit_func(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Func(stmt) => stmt,
            _ => unreachable!(),
        };
        let func = Function::new(stmt, self.env.clone());
        self.define(stmt.name.lexeme.clone(), Value::Function(Rc::new(func)));
        Ok(())
    }

    fn visit_if(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::If(stmt) => stmt,
            _ => unreachable!(),
        };

        if self.visit_expr(stmt.cond, env)?.is_truthy() {
            self.visit_stmt(stmt.then, env)
        } else if let Some(elze) = stmt.elze {
            self.visit_stmt(elze, env)
        } else {
            Ok(())
        }
    }

    fn visit_print(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Print(stmt) => stmt,
            _ => unreachable!(),
        };
        let value = self.visit_expr(stmt.expr, env)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_return(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Return(stmt) => stmt,
            _ => unreachable!(),
        };
        Err(Error::Return(self.visit_expr(stmt.value, env)?))
    }

    fn visit_var(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::Var(stmt) => stmt,
            _ => unreachable!(),
        };
        let value = if let Some(init) = stmt.init {
            self.visit_expr(init, env)?
        } else {
            Value::Nil
        };

        self.define(stmt.name.lexeme.clone(), value);
        Ok(())
    }

    fn visit_while(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        let stmt = match env.get_stmt(stmt).unwrap() {
            Stmt::While(stmt) => stmt,
            _ => unreachable!(),
        };

        while self.visit_expr(stmt.cond, env)?.is_truthy() {
            self.visit_stmt(stmt.body, env)?;
        }
        Ok(())
    }
}

impl Drop for Interpreter {
    fn drop(&mut self) {
        for clos in self.clos_encl.iter_mut() {
            if let Some(clos) = clos.upgrade() {
                clos.borrow_mut().clear();
            }
        }
    }
}

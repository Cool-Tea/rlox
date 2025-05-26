use core::ops::Drop;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;

use crate::ast::*;
use crate::class::Class;
use crate::environment::Environment;
use crate::error::{Error, RtError, SemError, report};
use crate::function::*;
use crate::parser::Rule;
use crate::value::Value;
use crate::visitor::*;

#[derive(Debug, Clone)]
pub struct Interpreter {
    clos_encl: Vec<Weak<RefCell<Environment>>>, // enclosing environment of closure
    env: Rc<RefCell<Environment>>,
    in_func: bool,
    call_depth: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Rc::new(RefCell::new(Environment::new(None)));
        let clock_fn = native::ClockFn;
        global
            .borrow_mut()
            .define(clock_fn.identifier(), Value::Function(Rc::new(clock_fn)))
            .unwrap();
        Interpreter {
            clos_encl: Vec::new(),
            env: global.clone(),
            in_func: false,
            call_depth: 0,
        }
    }

    pub fn interpret(&mut self, ast: &AST) -> Result<(), Error> {
        for &stmt in ast.entries() {
            self.visit_stmt(stmt, ast)?;
        }
        Ok(())
    }

    fn define(&mut self, name: String, value: Value) -> Result<(), Error> {
        self.env.borrow_mut().define(name, value)?;
        Ok(())
    }

    fn new_env(&self) -> Environment {
        Environment::new(Some(self.env.clone()))
    }

    pub fn execute_block(
        &mut self,
        env: Environment,
        stmts: &[usize],
        ast: &AST,
        is_func: bool,
    ) -> Result<(), Error> {
        let prev = self.env.clone();

        if is_func {
            self.call_depth += 1;
            if self.call_depth > 65536 {
                return report(Error::Runtime(RtError::StackOverflow));
            }
        }
        let backup_func_state = self.in_func;
        self.in_func = is_func;

        self.env = Rc::new(RefCell::new(env));

        for &stmt in stmts {
            if let Err(e) = self.visit_stmt(stmt, ast) {
                self.env = prev;
                return Err(e);
            }
        }

        self.in_func = backup_func_state;
        if is_func {
            self.call_depth -= 1;
        }

        self.env = prev;
        Ok(())
    }
}

fn as_number(value: Rc<RefCell<Value>>, _: &Token) -> Result<f64, Error> {
    match *value.borrow() {
        Value::Number(ref f) => Ok(*f),
        _ => {
            let err = Error::Runtime(RtError::TypeMismatch);
            err.report();
            Err(err)
        }
    }
}

impl ExprVisitor<Result<Rc<RefCell<Value>>, Error>> for Interpreter {
    fn visit_assign(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Assign(expr) => expr,
            _ => unreachable!(),
        };

        let value = self.visit_expr(expr.value, ast)?;
        if let Expr::Binary(binary) = ast.get_expr(expr.lhs).unwrap() {
            if binary.op.rule == Rule::Dot {
                let visit = binary;
                let name = if let Expr::Variable(var) = ast.get_expr(visit.rhs).unwrap() {
                    var.name.clone()
                } else {
                    unreachable!()
                };
                let object = self.visit_expr(visit.lhs, ast)?;
                if let Value::Instance(ref instance) = *object.borrow() {
                    instance.set(name.lexeme.clone(), value.clone());
                } else {
                    return report(Error::Runtime(RtError::UndefinedMember(
                        name.lexeme.clone(),
                    )));
                }
            } else {
                self.visit_expr(binary.lhs, ast)?
                    .replace(value.borrow().clone());
            }
        } else {
            self.visit_expr(expr.lhs, ast)?
                .replace(value.borrow().clone());
        };

        Ok(value)
    }

    fn visit_binary(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Binary(expr) => expr,
            _ => unreachable!(),
        };
        let lhs = self.visit_expr(expr.lhs, ast)?;
        if expr.op.rule == Rule::Dot {
            let name = if let Expr::Variable(var) = ast.get_expr(expr.rhs).unwrap() {
                var.name.clone()
            } else {
                unreachable!()
            };
            if let Value::Instance(ref instance) = *lhs.borrow() {
                let res = instance.get(&name.lexeme)?;
                if let Value::Function(ref func) = *res.borrow() {
                    func.bind(instance.clone());
                    Ok(Rc::new(RefCell::new(Value::Function(func.clone()))))
                } else {
                    Ok(res)
                }
            } else {
                report(Error::Runtime(RtError::UndefinedMember(
                    name.lexeme.clone(),
                )))
            }
        } else {
            let rhs = self.visit_expr(expr.rhs, ast)?;

            Ok(Rc::new(RefCell::new(match expr.op.rule {
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
                    if rhs == 0.0 {
                        return report(Error::Runtime(RtError::DivideByZero));
                    }
                    Value::Number(lhs / rhs)
                }
                Rule::Plus => match (&*lhs.borrow(), &*rhs.borrow()) {
                    (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                    (Value::String(lhs), Value::String(rhs)) => Value::String(lhs.clone() + rhs),
                    _ => return report(Error::Runtime(RtError::TypeMismatch)),
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
                Rule::EqualEqual => Value::Bool(lhs == rhs),
                Rule::BangEqual => Value::Bool(lhs != rhs),

                _ => Value::Nil,
            })))
        }
    }

    fn visit_call(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Call(expr) => expr,
            _ => unreachable!(),
        };

        let callee = self.visit_expr(expr.callee, ast)?;
        let mut args = Vec::new();
        for &arg in expr.args.iter() {
            let arg = self.visit_expr(arg, ast)?;
            args.push(arg);
        }

        match &*callee.borrow() {
            Value::Function(func) => {
                if func.arity() != args.len() {
                    report(Error::Runtime(RtError::InvalidArgsNumber(
                        args.len(),
                        func.arity(),
                    )))
                } else {
                    func.call(args, self, ast)
                }
            }
            Value::Class(class) => {
                if class.arity() != args.len() {
                    report(Error::Runtime(RtError::InvalidArgsNumber(
                        args.len(),
                        class.arity(),
                    )))
                } else {
                    class.call(args, self, ast)
                }
            }
            _ => report(Error::Runtime(RtError::CallNonCallable)),
        }
    }

    fn visit_grouping(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Grouping(expr) => expr,
            _ => unreachable!(),
        };

        self.visit_expr(expr.expr, ast)
    }

    fn visit_literal(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Literal(expr) => expr,
            _ => unreachable!(),
        };

        Ok(Rc::new(RefCell::new(match expr {
            Literal::Number(f) => Value::Number(*f),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
        })))
    }

    fn visit_logical(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Logical(expr) => expr,
            _ => unreachable!(),
        };
        let lhs = self.visit_expr(expr.lhs, ast)?;
        match expr.op.rule {
            Rule::Or => {
                if lhs.borrow().is_truthy() {
                    return Ok(lhs);
                }
            }
            Rule::And => {
                if !lhs.borrow().is_truthy() {
                    return Ok(lhs);
                }
            }
            _ => {
                unreachable!();
            }
        }
        self.visit_expr(expr.rhs, ast)
    }

    fn visit_unary(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Unary(expr) => expr,
            _ => unreachable!(),
        };

        let rhs = self.visit_expr(expr.rhs, ast)?;

        Ok(Rc::new(RefCell::new(match expr.op.rule {
            Rule::Minus => match *rhs.borrow() {
                Value::Number(ref f) => Value::Number(-f),
                _ => return report(Error::Runtime(RtError::TypeMismatch)),
            },
            Rule::Bang => Value::Bool(!rhs.borrow().is_truthy()),
            _ => Value::Nil,
        })))
    }

    fn visit_variable(&mut self, expr: usize, ast: &AST) -> Result<Rc<RefCell<Value>>, Error> {
        let expr = match ast.get_expr(expr).unwrap() {
            Expr::Variable(expr) => expr,
            _ => unreachable!(),
        };

        self.env.borrow().get(expr.name.lexeme.to_string())
    }
}

impl StmtVisitor<Result<(), Error>> for Interpreter {
    fn visit_block(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Block(stmt) => stmt,
            _ => unreachable!(),
        };
        let e = self.new_env();
        self.execute_block(e, &stmt.stmts, ast, self.in_func)
    }

    fn visit_expr_stmt(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Expr(stmt) => stmt,
            _ => unreachable!(),
        };
        self.visit_expr(stmt.expr, ast)?;
        Ok(())
    }

    fn visit_func(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Func(stmt) => stmt,
            _ => unreachable!(),
        };
        let func = Function::new(
            stmt,
            Rc::new(RefCell::new(self.env.borrow().clone())),
            false,
        )?;
        self.define(stmt.name.lexeme.clone(), Value::Function(func))
    }

    fn visit_class(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Class(stmt) => stmt,
            _ => unreachable!(),
        };
        let superclass = if let Some(superclass) = &stmt.superclass {
            if let Value::Class(class) = self
                .env
                .borrow()
                .get(superclass.lexeme.to_string())?
                .borrow()
                .clone()
            {
                Some(class)
            } else {
                return report(Error::Semantic(SemError::InvalidInheritance));
            }
        } else {
            None
        };
        let mut methods = HashMap::new();
        for &method in stmt.methods.iter() {
            let method = match ast.get_stmt(method).unwrap() {
                Stmt::Func(func) => func,
                _ => unreachable!(),
            };
            let mut env = self.env.borrow().clone();
            if let Some(superclass) = &superclass {
                env = Environment::new(Some(Rc::new(RefCell::new(env))));
                let super_obj = superclass.clone();
                env.define("super".to_string(), Value::Class(super_obj))
                    .unwrap();
            }
            let func = Function::new(method, Rc::new(RefCell::new(env)), true)?;
            methods.insert(method.name.lexeme.clone(), func);
        }
        let class = Class::new(stmt, methods, superclass)?;
        self.define(stmt.name.lexeme.clone(), Value::Class(class))
    }

    fn visit_if(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::If(stmt) => stmt,
            _ => unreachable!(),
        };

        if self.visit_expr(stmt.cond, ast)?.borrow().is_truthy() {
            self.visit_stmt(stmt.then, ast)
        } else if let Some(elze) = stmt.elze {
            self.visit_stmt(elze, ast)
        } else {
            Ok(())
        }
    }

    fn visit_print(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Print(stmt) => stmt,
            _ => unreachable!(),
        };
        let value = self.visit_expr(stmt.expr, ast)?;
        println!("{}", value.borrow());
        Ok(())
    }

    fn visit_return(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        if !self.in_func {
            return report(Error::Semantic(SemError::RetFromTop));
        }
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Return(stmt) => stmt,
            _ => unreachable!(),
        };
        Err(Error::Return(
            self.visit_expr(stmt.value, ast)?.borrow().clone(),
        ))
    }

    fn visit_var(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::Var(stmt) => stmt,
            _ => unreachable!(),
        };
        let value = if let Some(init) = stmt.init {
            self.visit_expr(init, ast)?.borrow().clone()
        } else {
            Value::Nil
        };

        self.define(stmt.name.lexeme.clone(), value)
    }

    fn visit_while(&mut self, stmt: usize, ast: &AST) -> Result<(), Error> {
        let stmt = match ast.get_stmt(stmt).unwrap() {
            Stmt::While(stmt) => stmt,
            _ => unreachable!(),
        };

        while self.visit_expr(stmt.cond, ast)?.borrow().is_truthy() {
            self.visit_stmt(stmt.body, ast)?;
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

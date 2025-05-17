use crate::ast::*;
use crate::error::Error;
use crate::parser::Rule;
use crate::value::Value;
use crate::visitor::*;

#[derive(Debug, Clone)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&mut self, ast: &AST) -> Result<(), Error> {
        for &stmt in ast.entries() {
            self.visit_stmt(stmt, ast)?;
        }
        Ok(())
    }

    fn report(line: usize, col: usize, content: &str, msg: String) -> Error {
        Error::report(line, col, content, msg);
        Error::Runtime
    }
}

fn as_number(value: Value, op: &Token) -> Result<f64, Error> {
    match value {
        Value::Number(f) => Ok(f),
        _ => {
            Error::report(op.line, op.col, &op.lexeme, "Expect number.".to_string());
            Err(Error::Runtime)
        }
    }
}

impl ExprVisitor<Result<Value, Error>> for Interpreter {
    fn visit_assign(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        todo!()
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
        todo!()
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
                return Err(Self::report(
                    expr.op.line,
                    expr.op.col,
                    &expr.op.lexeme,
                    "Expect 'and' or 'or' in logical expression.".to_string(),
                ));
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
                _ => Err(Self::report(
                    expr.op.line,
                    expr.op.col,
                    &expr.op.lexeme,
                    "Expect number.".to_string(),
                )),
            },
            Rule::Bang => Ok(Value::Bool(!rhs.is_truthy())),
            _ => Ok(Value::Nil),
        }
    }

    fn visit_variable(&mut self, expr: usize, env: &AST) -> Result<Value, Error> {
        todo!()
    }
}

impl StmtVisitor<Result<(), Error>> for Interpreter {
    fn visit_block(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        todo!()
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
        todo!()
    }

    fn visit_if(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        todo!()
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
        todo!()
    }

    fn visit_var(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        todo!()
    }

    fn visit_while(&mut self, stmt: usize, env: &AST) -> Result<(), Error> {
        todo!()
    }
}

use loxc::ast::LoxAST;
use loxc::ast_node::*;
use loxc::visitor::LoxExprVisitor;

#[derive(Debug, Clone)]
enum LoxValue {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

impl LoxValue {
    fn is_true_value(&self) -> bool {
        match self {
            LoxValue::Bool(b) => *b,
            LoxValue::Nil => false,
            _ => true,
        }
    }

    fn is_equal(&self, rhs: &LoxValue) -> bool {
        match self {
            LoxValue::Bool(lhs) => match rhs {
                LoxValue::Bool(rhs) => rhs == lhs,
                _ => false,
            },
            LoxValue::Number(lhs) => match rhs {
                LoxValue::Number(rhs) => rhs == lhs,
                _ => false,
            },
            LoxValue::String(lhs) => match rhs {
                LoxValue::String(rhs) => rhs == lhs,
                _ => false,
            },
            LoxValue::Nil => {
                matches!(rhs, LoxValue::Nil)
            }
        }
    }
}

pub struct RuntimeError {
    op: Operator,
    msg: String,
}

pub struct LoxInterpreter;

impl LoxInterpreter {
    pub fn interpret(&mut self, ast: LoxAST) {
        for decl in &ast.program.decls {
            match decl.as_ref() {
                Decl::Stmt(stmt) => match stmt {
                    Stmt::Expr(expr) => match self.visit_expr(&expr.expr) {
                        Ok(value) => println!("value = {:?}", value),
                        Err(err) => println!("error[{:?}]:  {}", err.op, err.msg),
                    },
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
    }
}

impl LoxExprVisitor<Result<LoxValue, RuntimeError>> for LoxInterpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        match expr {
            Expr::Number(value) => Ok(LoxValue::Number(*value)),
            Expr::String(s) => Ok(LoxValue::String(s.clone())),
            Expr::Bool(b) => Ok(LoxValue::Bool(*b)),
            Expr::Nil => Ok(LoxValue::Nil),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            _ => todo!(),
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<LoxValue, RuntimeError> {
        let lhs = self.visit_expr(&expr.lhs)?;
        let rhs = self.visit_expr(&expr.rhs)?;

        match expr.op {
            Operator::Assign => todo!(),
            Operator::Or => Ok(LoxValue::Bool(lhs.is_true_value() || rhs.is_true_value())),
            Operator::And => Ok(LoxValue::Bool(lhs.is_true_value() && rhs.is_true_value())),
            Operator::Ne => Ok(LoxValue::Bool(!lhs.is_equal(&rhs))),
            Operator::Eq => Ok(LoxValue::Bool(lhs.is_equal(&rhs))),
            Operator::Lt => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Bool(lhs < rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not comparable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not comparable!".to_string(),
                }),
            },
            Operator::Gt => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Bool(lhs > rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not comparable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not comparable!".to_string(),
                }),
            },
            Operator::Le => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Bool(lhs <= rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not comparable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not comparable!".to_string(),
                }),
            },
            Operator::Ge => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Bool(lhs >= rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not comparable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not comparable!".to_string(),
                }),
            },
            Operator::Add => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Number(lhs + rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not addable!".to_string(),
                    }),
                },
                LoxValue::String(lhs) => match rhs {
                    LoxValue::String(rhs) => Ok(LoxValue::String(lhs + &rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not addable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not addable!".to_string(),
                }),
            },
            Operator::Sub => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Number(lhs - rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not addable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not addable!".to_string(),
                }),
            },
            Operator::Mul => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Number(lhs * rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not addable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not addable!".to_string(),
                }),
            },
            Operator::Div => match lhs {
                LoxValue::Number(lhs) => match rhs {
                    LoxValue::Number(rhs) => Ok(LoxValue::Number(lhs / rhs)),
                    _ => Err(RuntimeError {
                        op: expr.op,
                        msg: "Value is not addable!".to_string(),
                    }),
                },
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not addable!".to_string(),
                }),
            },
            Operator::Visit => todo!(),
            _ => unreachable!(),
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<LoxValue, RuntimeError> {
        let rhs = self.visit_expr(&expr.expr)?;

        match expr.op {
            Operator::Not => Ok(LoxValue::Bool(!rhs.is_true_value())),
            Operator::Minus => match rhs {
                LoxValue::Number(rhs) => Ok(LoxValue::Number(-rhs)),
                _ => Err(RuntimeError {
                    op: expr.op,
                    msg: "Value is not negatable!".to_string(),
                }),
            },
            _ => unreachable!(),
        }
    }

    fn visit_call_expr(&mut self, _expr: &CallExpr) -> Result<LoxValue, RuntimeError> {
        todo!()
    }
}

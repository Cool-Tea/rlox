use crate::ast::*;

pub trait ExprVisitor<R> {
    fn visit_expr(&mut self, expr: usize, env: &AST) -> R {
        match env.get_expr(expr).unwrap() {
            Expr::Assign(_) => self.visit_assign(expr, env),
            Expr::Binary(_) => self.visit_binary(expr, env),
            Expr::Call(_) => self.visit_call(expr, env),
            Expr::Grouping(_) => self.visit_grouping(expr, env),
            Expr::Literal(_) => self.visit_literal(expr, env),
            Expr::Logical(_) => self.visit_logical(expr, env),
            Expr::Unary(_) => self.visit_unary(expr, env),
            Expr::Variable(_) => self.visit_variable(expr, env),
        }
    }
    fn visit_assign(&mut self, expr: usize, env: &AST) -> R;
    fn visit_binary(&mut self, expr: usize, env: &AST) -> R;
    fn visit_call(&mut self, expr: usize, env: &AST) -> R;
    fn visit_grouping(&mut self, expr: usize, env: &AST) -> R;
    fn visit_literal(&mut self, expr: usize, env: &AST) -> R;
    fn visit_logical(&mut self, expr: usize, env: &AST) -> R;
    fn visit_unary(&mut self, expr: usize, env: &AST) -> R;
    fn visit_variable(&mut self, expr: usize, env: &AST) -> R;
}

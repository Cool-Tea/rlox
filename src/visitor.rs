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

pub trait StmtVisitor<R> {
    fn visit_stmt(&mut self, stmt: usize, env: &AST) -> R {
        match env.get_stmt(stmt).unwrap() {
            Stmt::Block(_) => self.visit_block(stmt, env),
            Stmt::Expr(_) => self.visit_expr_stmt(stmt, env),
            Stmt::Func(_) => self.visit_func(stmt, env),
            Stmt::If(_) => self.visit_if(stmt, env),
            Stmt::Print(_) => self.visit_print(stmt, env),
            Stmt::Return(_) => self.visit_return(stmt, env),
            Stmt::Var(_) => self.visit_var(stmt, env),
            Stmt::While(_) => self.visit_while(stmt, env),
        }
    }
    fn visit_block(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_expr_stmt(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_func(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_if(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_print(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_return(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_var(&mut self, stmt: usize, env: &AST) -> R;
    fn visit_while(&mut self, stmt: usize, env: &AST) -> R;
}

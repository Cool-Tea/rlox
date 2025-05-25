use crate::ast::*;

pub trait ExprVisitor<R> {
    fn visit_expr(&mut self, expr: usize, ast: &AST) -> R {
        match ast.get_expr(expr).unwrap() {
            Expr::Assign(_) => self.visit_assign(expr, ast),
            Expr::Binary(_) => self.visit_binary(expr, ast),
            Expr::Call(_) => self.visit_call(expr, ast),
            Expr::Grouping(_) => self.visit_grouping(expr, ast),
            Expr::Literal(_) => self.visit_literal(expr, ast),
            Expr::Logical(_) => self.visit_logical(expr, ast),
            Expr::Unary(_) => self.visit_unary(expr, ast),
            Expr::Variable(_) => self.visit_variable(expr, ast),
        }
    }
    fn visit_assign(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_binary(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_call(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_grouping(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_literal(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_logical(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_unary(&mut self, expr: usize, ast: &AST) -> R;
    fn visit_variable(&mut self, expr: usize, ast: &AST) -> R;
}

pub trait StmtVisitor<R> {
    fn visit_stmt(&mut self, stmt: usize, ast: &AST) -> R {
        match ast.get_stmt(stmt).unwrap() {
            Stmt::Block(_) => self.visit_block(stmt, ast),
            Stmt::Expr(_) => self.visit_expr_stmt(stmt, ast),
            Stmt::Func(_) => self.visit_func(stmt, ast),
            Stmt::Class(_) => self.visit_class(stmt, ast),
            Stmt::If(_) => self.visit_if(stmt, ast),
            Stmt::Print(_) => self.visit_print(stmt, ast),
            Stmt::Return(_) => self.visit_return(stmt, ast),
            Stmt::Var(_) => self.visit_var(stmt, ast),
            Stmt::While(_) => self.visit_while(stmt, ast),
        }
    }
    fn visit_block(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_expr_stmt(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_func(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_if(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_print(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_return(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_var(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_while(&mut self, stmt: usize, ast: &AST) -> R;
    fn visit_class(&mut self, stmt: usize, ast: &AST) -> R;
}

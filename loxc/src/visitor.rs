use crate::ast_node::*;

pub trait LoxExprVisitor<RetType> {
    fn visit_expr(&mut self, expr: &Expr) -> RetType;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> RetType;

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> RetType;

    fn visit_call_expr(&mut self, expr: &CallExpr) -> RetType;
}

pub trait LoxStmtVisitor<RetType> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> RetType;

    fn visit_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> RetType;

    fn visit_for_stmt(&mut self, for_stmt: &ForStmt) -> RetType;

    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) -> RetType;

    fn visit_print_stmt(&mut self, print_stmt: &PrintStmt) -> RetType;

    fn visit_return_stmt(&mut self, return_stmt: &ReturnStmt) -> RetType;

    fn visit_while_stmt(&mut self, while_stmt: &WhileStmt) -> RetType;

    fn visit_block(&mut self, block: &Block) -> RetType;
}

pub trait LoxDeclVisitor<RetType> {
    fn visit_decl(&mut self, decl: &Decl) -> RetType;

    fn visit_class_decl(&mut self, class_decl: &ClassDecl) -> RetType;

    fn visit_fun_decl(&mut self, fun_decl: &FunDecl) -> RetType;

    fn visit_func(&mut self, func: &Func) -> RetType;

    fn visit_var_decl(&mut self, var_decl: &VarDecl) -> RetType;
}

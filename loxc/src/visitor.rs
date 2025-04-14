use crate::ast::LoxAST;
use crate::ast_node::*;

pub trait LoxVisitor {
    type Output;

    fn visit(&mut self, tree: LoxAST) -> Option<Self::Output> {
        self.visit_program(&tree.program)
    }

    fn visit_program(&mut self, program: &Program) -> Option<Self::Output> {
        for decl in &program.decls {
            self.visit_decl(decl);
        }
        None
    }

    fn visit_decl(&mut self, decl: &Decl) -> Option<Self::Output> {
        match decl {
            Decl::Class(class_decl) => self.visit_class_decl(class_decl),
            Decl::Fun(fun_decl) => self.visit_fun_decl(fun_decl),
            Decl::Var(var_decl) => self.visit_var_decl(var_decl),
            Decl::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_class_decl(&mut self, _: &ClassDecl) -> Option<Self::Output> {
        None
    }

    fn visit_fun_decl(&mut self, fun_decl: &FunDecl) -> Option<Self::Output> {
        self.visit_func(&fun_decl.func)
    }

    fn visit_func(&mut self, func: &Func) -> Option<Self::Output> {
        self.visit_block(&func.body)
    }

    fn visit_var_decl(&mut self, var_decl: &VarDecl) -> Option<Self::Output> {
        if let Some(init) = &var_decl.init {
            self.visit_expr(init)
        } else {
            None
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Option<Self::Output> {
        match stmt {
            Stmt::Expr(expr_stmt) => self.visit_expr_stmt(expr_stmt),
            Stmt::For(for_stmt) => self.visit_for_stmt(for_stmt),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::Print(print_stmt) => self.visit_print_stmt(print_stmt),
            Stmt::Return(return_stmt) => self.visit_return_stmt(return_stmt),
            Stmt::While(while_stmt) => self.visit_while_stmt(while_stmt),
            Stmt::Block(block) => self.visit_block(block),
        }
    }

    fn visit_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> Option<Self::Output> {
        self.visit_expr(&expr_stmt.expr)
    }

    fn visit_for_stmt(&mut self, for_stmt: &ForStmt) -> Option<Self::Output> {
        if let Some(init) = &for_stmt.init {
            match init.as_ref() {
                ForInitializer::VarDecl(var_decl) => self.visit_var_decl(var_decl),
                ForInitializer::ExprStmt(expr_stmt) => self.visit_expr_stmt(expr_stmt),
            };
        }
        if let Some(cond) = &for_stmt.cond {
            self.visit_expr(cond);
        }
        if let Some(post) = &for_stmt.post {
            self.visit_expr(post);
        }
        self.visit_stmt(&for_stmt.body)
    }

    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) -> Option<Self::Output> {
        self.visit_expr(&if_stmt.cond);
        self.visit_stmt(&if_stmt.then_body);
        if let Some(else_body) = &if_stmt.else_body {
            self.visit_stmt(else_body)
        } else {
            None
        }
    }

    fn visit_print_stmt(&mut self, print_stmt: &PrintStmt) -> Option<Self::Output> {
        self.visit_expr(&print_stmt.expr)
    }

    fn visit_return_stmt(&mut self, return_stmt: &ReturnStmt) -> Option<Self::Output> {
        if let Some(expr) = &return_stmt.expr {
            self.visit_expr(expr)
        } else {
            None
        }
    }

    fn visit_while_stmt(&mut self, while_stmt: &WhileStmt) -> Option<Self::Output> {
        self.visit_expr(&while_stmt.cond);
        self.visit_stmt(&while_stmt.body)
    }

    fn visit_block(&mut self, block: &Block) -> Option<Self::Output> {
        for decl in &block.decls {
            self.visit_decl(decl);
        }
        None
    }

    fn visit_expr(&mut self, expr: &Expr) -> Option<Self::Output> {
        match expr {
            Expr::Number(_) => None,
            Expr::String(_) => None,
            Expr::Bool(_) => None,
            Expr::Nil => None,
            Expr::Variable(_) => None,
            Expr::This => None,
            Expr::Super => None,
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            Expr::Call(call_expr) => self.visit_call_expr(call_expr),
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Option<Self::Output> {
        self.visit_expr(&expr.lhs);
        self.visit_expr(&expr.rhs)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Option<Self::Output> {
        self.visit_expr(&expr.expr)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Option<Self::Output> {
        for arg in &expr.args {
            self.visit_expr(arg);
        }
        self.visit_expr(&expr.callee)
    }
}

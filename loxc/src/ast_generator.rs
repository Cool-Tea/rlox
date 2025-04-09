use core::panic;

use crate::ast::*;
use crate::parser::Rule;
use pest::iterators::Pair;

pub struct AstGenerator;

impl AstGenerator {
    pub fn visit_program(&mut self, tree: Pair<'_, Rule>) -> Program {
        let mut decls = Vec::new();
        for child in tree.into_inner() {
            match child.as_rule() {
                Rule::Decl => {
                    decls.push(Box::new(self.visit_decl(child)));
                }
                Rule::EOI => {
                    break;
                }
                _ => panic!(),
            }
        }
        Program { decls }
    }

    fn visit_decl(&mut self, tree: Pair<'_, Rule>) -> Decl {
        let inner_decl = tree.into_inner().next().unwrap();
        match inner_decl.as_rule() {
            Rule::ClassDecl => Decl::Class(self.visit_class_decl(inner_decl)),
            Rule::FunDecl => Decl::Fun(self.visit_fun_decl(inner_decl)),
            Rule::VarDecl => Decl::Var(self.visit_var_decl(inner_decl)),
            Rule::Stmt => Decl::Stmt(self.visit_stmt(inner_decl)),
            _ => {
                panic!();
            }
        }
    }

    fn visit_class_decl(&mut self, tree: Pair<'_, Rule>) -> ClassDecl {
        let mut children = tree.into_inner();
        children.next();
        let name = children.next().unwrap().as_str().to_string();
        let superclass = if children.next().unwrap().as_rule() == Rule::Inherit {
            let res = children.next().unwrap().as_str().to_string();
            children.next();
            Some(res)
        } else {
            None
        };
        let mut methods = Vec::new();
        for func in children {
            match func.as_rule() {
                Rule::Func => {
                    methods.push(Box::new(self.visit_func(func)));
                }
                Rule::RBrace => {
                    break;
                }
                _ => panic!(),
            }
        }
        ClassDecl {
            name,
            superclass,
            methods,
        }
    }

    fn visit_fun_decl(&mut self, tree: Pair<'_, Rule>) -> FunDecl {
        let mut children = tree.into_inner();
        children.next();
        let func = self.visit_func(children.next().unwrap());
        FunDecl {
            func: Box::new(func),
        }
    }

    fn visit_var_decl(&mut self, tree: Pair<'_, Rule>) -> VarDecl {
        let mut children = tree.into_inner();
        children.next();
        let var_name = children.next().unwrap().as_str().to_string();
        let init = if children.next().unwrap().as_rule() == Rule::Assign {
            Some(Box::new(self.visit_expr(children.next().unwrap())))
        } else {
            None
        };
        children.next();
        VarDecl {
            name: var_name,
            init,
        }
    }

    fn visit_stmt(&mut self, tree: Pair<'_, Rule>) -> Stmt {
        let inner_stmt = tree.into_inner().next().unwrap();
        match inner_stmt.as_rule() {
            Rule::ExprStmt => Stmt::Expr(self.visit_expr_stmt(inner_stmt)),
            Rule::ForStmt => Stmt::For(self.visit_for_stmt(inner_stmt)),
            Rule::IfStmt => Stmt::If(self.visit_if_stmt(inner_stmt)),
            Rule::PrintStmt => Stmt::Print(self.visit_print_stmt(inner_stmt)),
            Rule::RetStmt => Stmt::Return(self.visit_ret_stmt(inner_stmt)),
            Rule::WhileStmt => Stmt::While(self.visit_while_stmt(inner_stmt)),
            Rule::Block => Stmt::Block(self.visit_block(inner_stmt)),
            _ => panic!(),
        }
    }

    fn visit_expr_stmt(&mut self, tree: Pair<'_, Rule>) -> ExprStmt {
        let mut children = tree.into_inner();
        let expr = self.visit_expr(children.next().unwrap());
        children.next();
        ExprStmt {
            expr: Box::new(expr),
        }
    }

    fn visit_for_stmt(&mut self, tree: Pair<'_, Rule>) -> ForStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let init_stmt = children.next().unwrap();
        let init = match init_stmt.as_rule() {
            Rule::VarDecl => {
                children.next();
                Some(Box::new(ForInitializer::VarDecl(
                    self.visit_var_decl(init_stmt),
                )))
            }
            Rule::ExprStmt => {
                children.next();
                Some(Box::new(ForInitializer::ExprStmt(
                    self.visit_expr_stmt(init_stmt),
                )))
            }
            _ => None,
        };
        let judge_stmt = children.next().unwrap();
        let cond = if judge_stmt.as_rule() == Rule::SemiColon {
            None
        } else {
            children.next();
            Some(Box::new(self.visit_expr(judge_stmt)))
        };
        let post_stmt = children.next().unwrap();
        let post = if post_stmt.as_rule() == Rule::RParen {
            None
        } else {
            children.next();
            Some(Box::new(self.visit_expr(post_stmt)))
        };
        let body = self.visit_stmt(children.next().unwrap());
        ForStmt {
            init,
            cond,
            post,
            body: Box::new(body),
        }
    }

    fn visit_if_stmt(&mut self, tree: Pair<'_, Rule>) -> IfStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let cond = self.visit_expr(children.next().unwrap());
        children.next();
        let then_body = self.visit_stmt(children.next().unwrap());
        let else_body = if let Some(_) = children.next() {
            Some(Box::new(self.visit_stmt(children.next().unwrap())))
        } else {
            None
        };
        IfStmt {
            cond: Box::new(cond),
            then_body: Box::new(then_body),
            else_body,
        }
    }

    fn visit_print_stmt(&mut self, tree: Pair<'_, Rule>) -> PrintStmt {
        let mut children = tree.into_inner();
        children.next();
        let expr = self.visit_expr(children.next().unwrap());
        PrintStmt {
            expr: Box::new(expr),
        }
    }

    fn visit_ret_stmt(&mut self, tree: Pair<'_, Rule>) -> ReturnStmt {
        let mut children = tree.into_inner();
        children.next();
        let ret_expr = children.next().unwrap();
        let expr = if ret_expr.as_rule() == Rule::SemiColon {
            None
        } else {
            children.next();
            Some(Box::new(self.visit_expr(ret_expr)))
        };
        ReturnStmt { expr }
    }

    fn visit_while_stmt(&mut self, tree: Pair<'_, Rule>) -> WhileStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let cond = self.visit_expr(children.next().unwrap());
        children.next();
        let body = self.visit_stmt(children.next().unwrap());
        WhileStmt {
            cond: Box::new(cond),
            body: Box::new(body),
        }
    }

    fn visit_block(&mut self, tree: Pair<'_, Rule>) -> Block {
        let mut children = tree.into_inner();
        children.next();
        let mut decls = Vec::new();
        for child in children {
            match child.as_rule() {
                Rule::Decl => {
                    decls.push(Box::new(self.visit_decl(child)));
                }
                Rule::RBrace => {
                    break;
                }
                _ => panic!(),
            }
        }
        Block { decls }
    }

    fn visit_expr(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        if first.as_rule() == Rule::LogicOr {
            self.visit_logic_or(first)
        } else {
            let lhs = match first.as_rule() {
                Rule::Call => self.visit_call(first),
                Rule::Ident => self.visit_ident(first),
                _ => panic!(),
            };
            children.next();
            let rhs = self.visit_expr(children.next().unwrap());
            Expr::Binary(BinaryExpr {
                op: Operator::Assign,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
    }

    fn visit_logic_or(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_logic_and(children.next().unwrap());
        for child in children {
            match child.as_rule() {
                Rule::LogicAnd => {
                    let rhs = self.visit_logic_and(child);
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::Or,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Or => {}
                _ => panic!(),
            }
        }
        res
    }

    fn visit_logic_and(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_equal(children.next().unwrap());
        for child in children {
            match child.as_rule() {
                Rule::Equal => {
                    let rhs = self.visit_equal(child);
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::And,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::And => {}
                _ => panic!(),
            }
        }
        res
    }

    fn visit_equal(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_comp(children.next().unwrap());
        let mut op = Operator::Eq;
        for child in children {
            match child.as_rule() {
                Rule::Comp => {
                    let rhs = self.visit_comp(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Eq => {
                    op = Operator::Eq;
                }
                Rule::Ne => {
                    op = Operator::Ne;
                }
                _ => panic!(),
            }
        }
        res
    }

    fn visit_comp(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_term(children.next().unwrap());
        let mut op = Operator::Gt;
        for child in children {
            match child.as_rule() {
                Rule::Term => {
                    let rhs = self.visit_term(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Gt => {
                    op = Operator::Gt;
                }
                Rule::Ge => {
                    op = Operator::Ge;
                }
                Rule::Lt => {
                    op = Operator::Lt;
                }
                Rule::Le => {
                    op = Operator::Le;
                }
                _ => panic!(),
            }
        }
        res
    }

    fn visit_term(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_factor(children.next().unwrap());
        let mut op = Operator::Add;
        for child in children {
            match child.as_rule() {
                Rule::Factor => {
                    let rhs = self.visit_factor(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Add => {
                    op = Operator::Add;
                }
                Rule::Sub => {
                    op = Operator::Sub;
                }
                _ => panic!(),
            }
        }
        res
    }

    fn visit_factor(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_unary(children.next().unwrap());
        let mut op = Operator::Mul;
        for child in children {
            match child.as_rule() {
                Rule::Unary => {
                    let rhs = self.visit_unary(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Mul => {
                    op = Operator::Mul;
                }
                Rule::Div => {
                    op = Operator::Div;
                }
                _ => panic!(),
            }
        }
        res
    }

    fn visit_unary(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        if first.as_rule() == Rule::Call {
            self.visit_call(first)
        } else {
            let op = match first.as_rule() {
                Rule::Not => Operator::Not,
                Rule::Minus => Operator::Minus,
                _ => panic!(),
            };
            let expr = self.visit_unary(children.next().unwrap());
            Expr::Unary(UnaryExpr {
                op,
                expr: Box::new(expr),
            })
        }
    }

    fn visit_call(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = self.visit_primary(children.next().unwrap());
        for child in children {
            match child.as_rule() {
                Rule::Calling => {
                    let mut call_children = child.into_inner();
                    call_children.next();
                    let mut args = Vec::new();
                    let arg_raw = call_children.next().unwrap();
                    if arg_raw.as_rule() == Rule::Args {
                        for arg in arg_raw.into_inner() {
                            match arg.as_rule() {
                                Rule::Expr => {
                                    args.push(Box::new(self.visit_expr(arg)));
                                }
                                Rule::Comma => {}
                                _ => panic!(),
                            }
                        }
                    }
                    call_children.next();
                    res = Expr::Call(InvokeExpr {
                        callee: Box::new(res),
                        args,
                    });
                }
                Rule::Visiting => {
                    let mut visit_children = child.into_inner();
                    visit_children.next();
                    let rhs = self.visit_ident(visit_children.next().unwrap());
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::Visit,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                _ => panic!(),
            }
        }
        res
    }

    fn visit_primary(&mut self, tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        match first.as_rule() {
            Rule::Num => self.visit_num(first),
            Rule::String => self.visit_string(first),
            Rule::Ident => self.visit_ident(first),
            Rule::True => self.visit_true(first),
            Rule::False => self.visit_false(first),
            Rule::Nil => self.visit_nil(first),
            Rule::This => self.visit_this(first),
            Rule::Super => {
                children.next();
                Expr::Binary(BinaryExpr {
                    op: Operator::Visit,
                    lhs: Box::new(self.visit_super(first)),
                    rhs: Box::new(self.visit_super(children.next().unwrap())),
                })
            }
            Rule::LParen => {
                let expr = self.visit_expr(children.next().unwrap());
                children.next();
                expr
            }
            _ => {
                panic!();
            }
        }
    }

    fn visit_func(&mut self, tree: Pair<'_, Rule>) -> Func {
        let mut children = tree.into_inner();
        let name = children.next().unwrap().as_str().to_string();
        children.next();
        let mut params = Vec::new();
        let param_raw = children.next().unwrap();
        if param_raw.as_rule() == Rule::Params {
            for param in param_raw.into_inner() {
                match param.as_rule() {
                    Rule::Ident => {
                        params.push(param.as_str().to_string());
                    }
                    Rule::Comma => {}
                    _ => panic!(),
                }
            }
            children.next();
        }
        let body = self.visit_block(children.next().unwrap());
        Func {
            name,
            params,
            body: Box::new(body),
        }
    }

    fn visit_num(&mut self, tree: Pair<'_, Rule>) -> Expr {
        Expr::Number(tree.as_str().parse().unwrap())
    }

    fn visit_string(&mut self, tree: Pair<'_, Rule>) -> Expr {
        Expr::String(tree.as_str().to_string())
    }

    fn visit_ident(&mut self, tree: Pair<'_, Rule>) -> Expr {
        Expr::Variable(tree.as_str().to_string())
    }

    fn visit_keyword(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        let (line, col) = tree.line_col();
        let text = tree.as_str().to_string();
        Terminal { line, col, text }
    }

    fn visit_class(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_inherit(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_l_brace(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_r_brace(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_l_paren(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_r_paren(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_dot(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_fun(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_var(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_assign(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_semi_colon(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_for(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_if(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_else(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_print(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_return(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_while(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_or(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_and(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_eq(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_ne(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_gt(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_ge(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_lt(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_le(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_add(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_sub(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_mul(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_div(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_not(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_minus(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }

    fn visit_true(&mut self, _tree: Pair<'_, Rule>) -> Expr {
        Expr::Bool(true)
    }

    fn visit_false(&mut self, _tree: Pair<'_, Rule>) -> Expr {
        Expr::Bool(false)
    }

    fn visit_nil(&mut self, _tree: Pair<'_, Rule>) -> Expr {
        Expr::Nil
    }

    fn visit_this(&mut self, _tree: Pair<'_, Rule>) -> Expr {
        Expr::This
    }

    fn visit_super(&mut self, _tree: Pair<'_, Rule>) -> Expr {
        Expr::Super
    }

    fn visit_comma(&mut self, tree: Pair<'_, Rule>) -> Terminal {
        self.visit_keyword(tree)
    }
}

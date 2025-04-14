use crate::ast_node::*;
use crate::parser::Rule;
use pest::iterators::Pair;

pub struct LoxAST {
    pub program: Program,
}

impl LoxAST {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Self {
        LoxAST {
            program: Self::visit_program(pair),
        }
    }

    fn visit_program(tree: Pair<'_, Rule>) -> Program {
        let mut decls = Vec::new();
        for child in tree.into_inner() {
            match child.as_rule() {
                Rule::Decl => decls.push(Box::new(Self::visit_decl(child))),
                Rule::EOI => break,
                _ => unreachable!(),
            }
        }
        Program { decls }
    }

    fn visit_decl(tree: Pair<'_, Rule>) -> Decl {
        let inner_decl = tree.into_inner().next().unwrap();
        match inner_decl.as_rule() {
            Rule::ClassDecl => Decl::Class(Self::visit_class_decl(inner_decl)),
            Rule::FunDecl => Decl::Fun(Self::visit_fun_decl(inner_decl)),
            Rule::VarDecl => Decl::Var(Self::visit_var_decl(inner_decl)),
            Rule::Stmt => Decl::Stmt(Self::visit_stmt(inner_decl)),
            _ => unreachable!(),
        }
    }

    fn visit_class_decl(tree: Pair<'_, Rule>) -> ClassDecl {
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
                Rule::Func => methods.push(Box::new(Self::visit_func(func))),
                Rule::RBrace => break,
                _ => unreachable!(),
            }
        }
        ClassDecl {
            name,
            superclass,
            methods,
        }
    }

    fn visit_fun_decl(tree: Pair<'_, Rule>) -> FunDecl {
        let mut children = tree.into_inner();
        children.next();
        let func = Self::visit_func(children.next().unwrap());
        FunDecl {
            func: Box::new(func),
        }
    }

    fn visit_var_decl(tree: Pair<'_, Rule>) -> VarDecl {
        let mut children = tree.into_inner();
        children.next();
        let var_name = children.next().unwrap().as_str().to_string();
        let init = if children.next().unwrap().as_rule() == Rule::Assign {
            Some(Box::new(Self::visit_expr(children.next().unwrap())))
        } else {
            None
        };
        children.next();
        VarDecl {
            name: var_name,
            init,
        }
    }

    fn visit_stmt(tree: Pair<'_, Rule>) -> Stmt {
        let inner_stmt = tree.into_inner().next().unwrap();
        match inner_stmt.as_rule() {
            Rule::ExprStmt => Stmt::Expr(Self::visit_expr_stmt(inner_stmt)),
            Rule::ForStmt => Stmt::For(Self::visit_for_stmt(inner_stmt)),
            Rule::IfStmt => Stmt::If(Self::visit_if_stmt(inner_stmt)),
            Rule::PrintStmt => Stmt::Print(Self::visit_print_stmt(inner_stmt)),
            Rule::RetStmt => Stmt::Return(Self::visit_ret_stmt(inner_stmt)),
            Rule::WhileStmt => Stmt::While(Self::visit_while_stmt(inner_stmt)),
            Rule::Block => Stmt::Block(Self::visit_block(inner_stmt)),
            _ => unreachable!(),
        }
    }

    fn visit_expr_stmt(tree: Pair<'_, Rule>) -> ExprStmt {
        let mut children = tree.into_inner();
        let expr = Self::visit_expr(children.next().unwrap());
        children.next();
        ExprStmt {
            expr: Box::new(expr),
        }
    }

    fn visit_for_stmt(tree: Pair<'_, Rule>) -> ForStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let init_stmt = children.next().unwrap();
        let init = match init_stmt.as_rule() {
            Rule::VarDecl => {
                children.next();
                Some(Box::new(ForInitializer::VarDecl(Self::visit_var_decl(
                    init_stmt,
                ))))
            }
            Rule::ExprStmt => {
                children.next();
                Some(Box::new(ForInitializer::ExprStmt(Self::visit_expr_stmt(
                    init_stmt,
                ))))
            }
            _ => None,
        };
        let judge_stmt = children.next().unwrap();
        let cond = if judge_stmt.as_rule() == Rule::SemiColon {
            None
        } else {
            children.next();
            Some(Box::new(Self::visit_expr(judge_stmt)))
        };
        let post_stmt = children.next().unwrap();
        let post = if post_stmt.as_rule() == Rule::RParen {
            None
        } else {
            children.next();
            Some(Box::new(Self::visit_expr(post_stmt)))
        };
        let body = Self::visit_stmt(children.next().unwrap());
        ForStmt {
            init,
            cond,
            post,
            body: Box::new(body),
        }
    }

    fn visit_if_stmt(tree: Pair<'_, Rule>) -> IfStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let cond = Self::visit_expr(children.next().unwrap());
        children.next();
        let then_body = Self::visit_stmt(children.next().unwrap());
        let else_body = if children.next().is_some() {
            Some(Box::new(Self::visit_stmt(children.next().unwrap())))
        } else {
            None
        };
        IfStmt {
            cond: Box::new(cond),
            then_body: Box::new(then_body),
            else_body,
        }
    }

    fn visit_print_stmt(tree: Pair<'_, Rule>) -> PrintStmt {
        let mut children = tree.into_inner();
        children.next();
        let expr = Self::visit_expr(children.next().unwrap());
        PrintStmt {
            expr: Box::new(expr),
        }
    }

    fn visit_ret_stmt(tree: Pair<'_, Rule>) -> ReturnStmt {
        let mut children = tree.into_inner();
        children.next();
        let ret_expr = children.next().unwrap();
        let expr = if ret_expr.as_rule() == Rule::SemiColon {
            None
        } else {
            children.next();
            Some(Box::new(Self::visit_expr(ret_expr)))
        };
        ReturnStmt { expr }
    }

    fn visit_while_stmt(tree: Pair<'_, Rule>) -> WhileStmt {
        let mut children = tree.into_inner();
        children.next();
        children.next();
        let cond = Self::visit_expr(children.next().unwrap());
        children.next();
        let body = Self::visit_stmt(children.next().unwrap());
        WhileStmt {
            cond: Box::new(cond),
            body: Box::new(body),
        }
    }

    fn visit_block(tree: Pair<'_, Rule>) -> Block {
        let mut children = tree.into_inner();
        children.next();
        let mut decls = Vec::new();
        for child in children {
            match child.as_rule() {
                Rule::Decl => decls.push(Box::new(Self::visit_decl(child))),
                Rule::RBrace => break,
                _ => unreachable!(),
            }
        }
        Block { decls }
    }

    fn visit_expr(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        if first.as_rule() == Rule::LogicOr {
            Self::visit_logic_or(first)
        } else {
            let lhs = match first.as_rule() {
                Rule::Call => Self::visit_call(first),
                Rule::Ident => Self::visit_ident(first),
                _ => unreachable!(),
            };
            children.next();
            let rhs = Self::visit_expr(children.next().unwrap());
            Expr::Binary(BinaryExpr {
                op: Operator::Assign,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
    }

    fn visit_logic_or(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_logic_and(children.next().unwrap());
        for child in children {
            match child.as_rule() {
                Rule::LogicAnd => {
                    let rhs = Self::visit_logic_and(child);
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::Or,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Or => (),
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_logic_and(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_equal(children.next().unwrap());
        for child in children {
            match child.as_rule() {
                Rule::Equal => {
                    let rhs = Self::visit_equal(child);
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::And,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::And => (),
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_equal(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_comp(children.next().unwrap());
        let mut op = Operator::Eq;
        for child in children {
            match child.as_rule() {
                Rule::Comp => {
                    let rhs = Self::visit_comp(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Eq => op = Operator::Eq,
                Rule::Ne => op = Operator::Ne,
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_comp(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_term(children.next().unwrap());
        let mut op = Operator::Gt;
        for child in children {
            match child.as_rule() {
                Rule::Term => {
                    let rhs = Self::visit_term(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Gt => op = Operator::Gt,
                Rule::Ge => op = Operator::Ge,
                Rule::Lt => op = Operator::Lt,
                Rule::Le => op = Operator::Le,
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_term(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_factor(children.next().unwrap());
        let mut op = Operator::Add;
        for child in children {
            match child.as_rule() {
                Rule::Factor => {
                    let rhs = Self::visit_factor(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Add => op = Operator::Add,
                Rule::Sub => op = Operator::Sub,
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_factor(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_unary(children.next().unwrap());
        let mut op = Operator::Mul;
        for child in children {
            match child.as_rule() {
                Rule::Unary => {
                    let rhs = Self::visit_unary(child);
                    res = Expr::Binary(BinaryExpr {
                        op,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                Rule::Mul => op = Operator::Mul,
                Rule::Div => op = Operator::Div,
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_unary(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        if first.as_rule() == Rule::Call {
            Self::visit_call(first)
        } else {
            let op = match first.as_rule() {
                Rule::Not => Operator::Not,
                Rule::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            let expr = Self::visit_unary(children.next().unwrap());
            Expr::Unary(UnaryExpr {
                op,
                expr: Box::new(expr),
            })
        }
    }

    fn visit_call(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let mut res = Self::visit_primary(children.next().unwrap());
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
                                    args.push(Box::new(Self::visit_expr(arg)));
                                }
                                Rule::Comma => {}
                                _ => unreachable!(),
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
                    let rhs = Self::visit_ident(visit_children.next().unwrap());
                    res = Expr::Binary(BinaryExpr {
                        op: Operator::Visit,
                        lhs: Box::new(res),
                        rhs: Box::new(rhs),
                    });
                }
                _ => unreachable!(),
            }
        }
        res
    }

    fn visit_primary(tree: Pair<'_, Rule>) -> Expr {
        let mut children = tree.into_inner();
        let first = children.next().unwrap();
        match first.as_rule() {
            Rule::Num => Self::visit_num(first),
            Rule::String => Self::visit_string(first),
            Rule::Ident => Self::visit_ident(first),
            Rule::True => Self::visit_true(first),
            Rule::False => Self::visit_false(first),
            Rule::Nil => Self::visit_nil(first),
            Rule::This => Self::visit_this(first),
            Rule::Super => Self::visit_super(first),
            Rule::LParen => {
                let expr = Self::visit_expr(children.next().unwrap());
                children.next();
                expr
            }
            _ => unreachable!(),
        }
    }

    fn visit_func(tree: Pair<'_, Rule>) -> Func {
        let mut children = tree.into_inner();
        let name = children.next().unwrap().as_str().to_string();
        children.next();
        let mut params = Vec::new();
        let param_raw = children.next().unwrap();
        if param_raw.as_rule() == Rule::Params {
            for param in param_raw.into_inner() {
                match param.as_rule() {
                    Rule::Ident => params.push(param.as_str().to_string()),
                    Rule::Comma => (),
                    _ => unreachable!(),
                }
            }
            children.next();
        }
        let body = Self::visit_block(children.next().unwrap());
        Func {
            name,
            params,
            body: Box::new(body),
        }
    }

    fn visit_num(tree: Pair<'_, Rule>) -> Expr {
        Expr::Number(tree.as_str().parse().unwrap())
    }

    fn visit_string(tree: Pair<'_, Rule>) -> Expr {
        Expr::String(tree.as_str().to_string())
    }

    fn visit_ident(tree: Pair<'_, Rule>) -> Expr {
        Expr::Variable(tree.as_str().to_string())
    }

    fn visit_true(_tree: Pair<'_, Rule>) -> Expr {
        Expr::Bool(true)
    }

    fn visit_false(_tree: Pair<'_, Rule>) -> Expr {
        Expr::Bool(false)
    }

    fn visit_nil(_tree: Pair<'_, Rule>) -> Expr {
        Expr::Nil
    }

    fn visit_this(_tree: Pair<'_, Rule>) -> Expr {
        Expr::This
    }

    fn visit_super(_tree: Pair<'_, Rule>) -> Expr {
        Expr::Super
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::LoxParser;
    use pest::Parser;

    #[test]
    fn test1() {
        let lox_str = "\
            class Animal {\n\
              init(name) {\n\
                this.name = name;\n\
              }\n\
              speak() {\n\
                print \"My name is \" + this.name;\n\
              }\n\
            }\n\
            var dog = Animal(\"Buddy\");\n\
            dog.speak();\n\
            ";
        let tree = LoxParser::parse(Rule::Program, lox_str)
            .unwrap()
            .next()
            .unwrap();
        let _ = LoxAST::from_pair(tree);
    }

    #[test]
    fn test2() {
        let lox_str = "\
            var a = 3.1415926; // comment\n\
            print /* comment */ a;\n\
            ";
        let tree = LoxParser::parse(Rule::Program, lox_str)
            .unwrap()
            .next()
            .unwrap();
        let _ = LoxAST::from_pair(tree);
    }

    #[test]
    fn test_ast() {
        let lox_str = "\
            class Animal {\n\
              init(name) {\n\
                super.child = name;\n\
                this.name = name;\n\
              }\n\
              speak() {\n\
                print \"My name is \" + this.name;\n\
              }\n\
            }\n\
            var dog = Animal(\"Buddy\");\n\
            dog.speak();\n\
            ";
        let tree = LoxParser::parse(Rule::Program, lox_str)
            .unwrap()
            .next()
            .unwrap();
        let _ = LoxAST::from_pair(tree);
    }
}

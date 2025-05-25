use pest::Parser as PestParser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;

use crate::ast::*;
use crate::error::Error;

#[derive(Parser)]
#[grammar = "lox.pest"]
struct RawParser;

#[derive(Debug, Clone)]
pub struct Parser;

impl Parser {
    fn report<T>(content: &str, msg: String) -> Result<T, Error> {
        let err = Error::Parse(content.to_string(), msg);
        err.report();
        Err(err)
    }

    pub fn parse(input: &str) -> Result<AST, Error> {
        let program = match RawParser::parse(Rule::Program, input) {
            Ok(pairs) => pairs,
            Err(err) => {
                return Self::report(err.line(), err.variant.message().to_string());
            }
        }
        .next()
        .unwrap(); // Program

        let mut ast = AST::new();

        for decl in program.into_inner() {
            if decl.as_rule() == Rule::EOI {
                break;
            }
            let stmt = Self::parse_decl(&mut ast, decl)?;
            ast.add_entry(stmt);
        }

        Ok(ast)
    }

    fn peek_match(it: &Pairs<'_, Rule>, token_type: Rule) -> bool {
        if let Some(token) = it.peek() {
            token.as_rule() == token_type
        } else {
            false
        }
    }

    fn parse_expr(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        match it.peek().unwrap().as_rule() {
            Rule::Call => {
                let target = Self::parse_call(ast, it.next().unwrap())?;
                it.next().unwrap(); // discard Equal
                let value = Self::parse_expr(ast, it.next().unwrap())?;

                Ok(ast.push_expr(Expr::Assign(AssignExpr { lhs: target, value })))
            }
            Rule::LogicOr => Self::parse_or(ast, it.next().unwrap()),
            _ => unreachable!(),
        }
    }

    fn parse_or(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_and(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_and(ast, it.next().unwrap())?;
            let new = Expr::Logical(LogicalExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_and(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_equality(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_equality(ast, it.next().unwrap())?;
            let new = Expr::Logical(LogicalExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_equality(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_comp(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_comp(ast, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_comp(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_term(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_term(ast, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_term(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_factor(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_factor(ast, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_factor(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_unary(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_unary(ast, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = ast.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_unary(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let ret = if Self::peek_match(&it, Rule::Call) {
            Self::parse_call(ast, it.next().unwrap())?
        } else {
            let token = it.next().unwrap();
            let op = token.into();
            let rhs = Self::parse_unary(ast, it.next().unwrap())?;
            let expr = Expr::Unary(UnaryExpr { rhs, op });
            ast.push_expr(expr)
        };
        Ok(ret)
    }

    fn parse_call(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_primary(ast, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let new = match token.as_rule() {
                Rule::LParen => {
                    let args = if Self::peek_match(&it, Rule::RParen) {
                        Vec::<usize>::new()
                    } else {
                        Self::parse_args(ast, it.next().unwrap())?
                    };

                    let token = it.next().unwrap();
                    let op = token.into();
                    Expr::Call(CallExpr {
                        callee: expr,
                        args,
                        op,
                    })
                }
                Rule::Dot => {
                    let op = token.into();
                    let rhs = ast.push_expr(Expr::Variable(VariableExpr {
                        name: it.next().unwrap().into(),
                    }));
                    Expr::Binary(BinaryExpr { lhs: expr, rhs, op })
                }
                _ => unreachable!(),
            };
            expr = ast.push_expr(new);
        }
        Ok(expr)
    }

    fn parse_args(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<Vec<usize>, Error> {
        let mut it = expr.into_inner();
        let mut args: Vec<usize> = Vec::new();

        let expr = Self::parse_expr(ast, it.next().unwrap())?;
        args.push(expr);

        while let Some(token) = it.next() {
            if args.len() >= 255 {
                return Self::report(
                    token.as_str(),
                    "Cannot have more than 255 arguments.".to_string(),
                );
            }
            args.push(Self::parse_expr(ast, it.next().unwrap())?);
        }

        Ok(args)
    }

    fn parse_primary(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let expr = match it.peek().unwrap().as_rule() {
            Rule::False => Expr::Literal(Literal::Bool(false)),
            Rule::True => Expr::Literal(Literal::Bool(true)),
            Rule::Nil => Expr::Literal(Literal::Nil),
            Rule::Number => {
                let token = it.next().unwrap();
                let _t = token.as_rule();
                Expr::Literal(Literal::Number(token.as_str().parse().unwrap()))
            }
            Rule::String => {
                let lexeme = it.next().unwrap().as_str();
                Expr::Literal(Literal::String(lexeme[1..lexeme.len() - 1].to_string()))
            }
            Rule::Identifier | Rule::This => Expr::Variable(VariableExpr {
                name: it.next().unwrap().into(),
            }),
            Rule::Super => {
                let lhs = it.next().unwrap(); // get Super
                let dot = it.next().unwrap(); // get Dot
                let method = it.next().unwrap().into();
                Expr::Binary(BinaryExpr {
                    lhs: ast.push_expr(Expr::Variable(VariableExpr { name: lhs.into() })),
                    rhs: ast.push_expr(Expr::Variable(VariableExpr { name: method })),
                    op: dot.into(),
                })
            }
            Rule::LParen => {
                it.next();
                let expr = Self::parse_expr(ast, it.next().unwrap())?;
                Expr::Grouping(GroupingExpr { expr })
            }

            _ => unreachable!(),
        };
        Ok(ast.push_expr(expr))
    }

    fn parse_stmt(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let stmt = expr.into_inner().next().unwrap();
        match stmt.as_rule() {
            Rule::Block => Self::parse_block(ast, stmt),
            Rule::ExprStmt => Self::parse_expr_stmt(ast, stmt),
            Rule::ForStmt => Self::parse_for(ast, stmt),
            Rule::IfStmt => Self::parse_if(ast, stmt),
            Rule::PrintStmt => Self::parse_print(ast, stmt),
            Rule::ReturnStmt => Self::parse_return(ast, stmt),
            Rule::WhileStmt => Self::parse_while(ast, stmt),
            _ => unreachable!(),
        }
    }

    fn parse_block(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard LBrace
        let mut stmts: Vec<usize> = Vec::new();

        for decl in it {
            if decl.as_rule() == Rule::RBrace {
                break;
            }
            stmts.push(Self::parse_decl(ast, decl)?);
        }

        Ok(ast.push_stmt(Stmt::Block(Block { stmts })))
    }

    fn parse_expr_stmt(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let value = Self::parse_expr(ast, it.next().unwrap())?;
        let stmt = Stmt::Expr(ExprStmt { expr: value });
        Ok(ast.push_stmt(stmt))
    }

    fn parse_for(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        // discard For LParen
        it.next();
        it.next();

        let init = it.next().unwrap();
        let init = match init.as_rule() {
            Rule::SemiColon => None,
            Rule::VarDecl => Some(Self::parse_var(ast, init)?),
            Rule::ExprStmt => Some(Self::parse_expr_stmt(ast, init)?),
            _ => unreachable!(),
        };

        let cond = if it.peek().unwrap().as_rule() == Rule::Expr {
            Self::parse_expr(ast, it.next().unwrap())?
        } else {
            ast.push_expr(Expr::Literal(Literal::Bool(true)))
        };

        it.next(); // discard SemiColon

        let post = if it.peek().unwrap().as_rule() == Rule::Expr {
            Some(Self::parse_expr(ast, it.next().unwrap())?)
        } else {
            None
        };

        it.next(); // discard RParen

        let mut body = Self::parse_stmt(ast, it.next().unwrap())?;

        if let Some(post) = post {
            let post = ast.push_stmt(Stmt::Expr(ExprStmt { expr: post }));
            body = ast.push_stmt(Stmt::Block(Block {
                stmts: vec![body, post],
            }))
        }

        body = ast.push_stmt(Stmt::While(WhileStmt { cond, body }));

        if let Some(init) = init {
            body = ast.push_stmt(Stmt::Block(Block {
                stmts: vec![init, body],
            }));
        }

        Ok(body)
    }

    fn parse_if(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard if
        it.next(); // discard LParen

        let cond = Self::parse_expr(ast, it.next().unwrap())?;
        it.next(); // discard RParen

        let then = Self::parse_stmt(ast, it.next().unwrap())?;
        let elze = if it.next().is_some() {
            Some(Self::parse_stmt(ast, it.next().unwrap())?)
        } else {
            None
        };

        Ok(ast.push_stmt(Stmt::If(IfStmt { cond, then, elze })))
    }

    fn parse_print(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard print
        let value = Self::parse_expr(ast, it.next().unwrap())?;
        let stmt = Stmt::Print(PrintStmt { expr: value });
        Ok(ast.push_stmt(stmt))
    }

    fn parse_return(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let keyword = it.next().unwrap().into();

        let value = if it.peek().unwrap().as_rule() == Rule::Expr {
            Self::parse_expr(ast, it.next().unwrap())?
        } else {
            ast.push_expr(Expr::Literal(Literal::Nil))
        };

        Ok(ast.push_stmt(Stmt::Return(ReturnStmt { value, keyword })))
    }

    fn parse_while(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        // discard While LParen
        it.next();
        it.next();

        let cond = Self::parse_expr(ast, it.next().unwrap())?;
        it.next(); // discard RParen

        let stmt = Self::parse_stmt(ast, it.next().unwrap())?;

        Ok(ast.push_stmt(Stmt::While(WhileStmt { cond, body: stmt })))
    }

    fn parse_decl(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let decl = it.next().unwrap();
        match decl.as_rule() {
            Rule::VarDecl => Self::parse_var(ast, decl),
            Rule::FunDecl => Self::parse_fun(ast, decl),
            Rule::ClassDecl => Self::parse_class(ast, decl),
            Rule::Stmt => Self::parse_stmt(ast, decl),
            _ => unreachable!(),
        }
    }

    fn parse_var(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard Var

        let name: Token = it.next().unwrap().into();

        let init = if it.next().unwrap().as_rule() == Rule::Equal {
            Some(Self::parse_expr(ast, it.next().unwrap())?)
        } else {
            None
        };

        Ok(ast.push_stmt(Stmt::Var(VarStmt { name, init })))
    }

    fn parse_fun(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard Fun
        Self::parse_func(ast, it.next().unwrap())
    }

    fn parse_func(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let name: Token = it.next().unwrap().into();
        it.next(); // discard LParen

        let params = if it.peek().unwrap().as_rule() == Rule::RParen {
            Vec::new()
        } else {
            Self::parse_params(it.next().unwrap())?
        };

        it.next(); // discard RParen
        let body = Self::parse_block(ast, it.next().unwrap())?;

        Ok(ast.push_stmt(Stmt::Func(FuncStmt { name, params, body })))
    }

    fn parse_params(expr: Pair<'_, Rule>) -> Result<Vec<Token>, Error> {
        let mut it = expr.into_inner();
        let mut params: Vec<Token> = Vec::new();

        let expr = it.next().unwrap().into();
        params.push(expr);

        while let Some(token) = it.next() {
            if params.len() >= 255 {
                return Self::report(
                    token.as_str(),
                    "Cannot have more than 255 arguments.".to_string(),
                );
            }
            let token = it.next().unwrap().into();
            params.push(token);
        }

        Ok(params)
    }

    fn parse_class(ast: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard class
        let name: Token = it.next().unwrap().into();
        let superclass = if Self::peek_match(&it, Rule::Less) {
            it.next(); // discard Less
            Some(it.next().unwrap().into())
        } else {
            None
        };

        let mut methods: Vec<usize> = Vec::new();
        it.next(); // discard LBrace

        while let Some(token) = it.next() {
            if token.as_rule() == Rule::RBrace {
                break;
            }
            methods.push(Self::parse_func(ast, token)?);
        }

        Ok(ast.push_stmt(Stmt::Class(ClassStmt {
            name,
            superclass,
            methods,
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_parser() {
        let input = "2.2 + 3.3";
        let file = RawParser::parse(Rule::Expr, input).unwrap().next().unwrap();
        println!("{:?}", file.as_rule());
    }
}

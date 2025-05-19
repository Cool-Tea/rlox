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
    fn report(line: usize, col: usize, content: &str, msg: String) -> Error {
        Error::report(line, col, content, msg);
        Error::Parse
    }

    pub fn parse(input: &str) -> Result<AST, Error> {
        let program = match RawParser::parse(Rule::Program, input) {
            Ok(pairs) => pairs,
            Err(err) => {
                let (line, col) = match err.line_col {
                    pest::error::LineColLocation::Pos(line_col) => line_col,
                    pest::error::LineColLocation::Span(line_col, _) => line_col,
                };
                return Err(Self::report(
                    line,
                    col,
                    err.line(),
                    err.variant.message().to_string(),
                ));
            }
        }
        .next()
        .unwrap(); // Program

        let mut env = AST::new();

        for decl in program.into_inner() {
            if decl.as_rule() == Rule::EOI {
                break;
            }
            let stmt = Self::parse_decl(&mut env, decl)?;
            env.add_entry(stmt);
        }

        Ok(env)
    }

    fn peek_match(it: &Pairs<'_, Rule>, token_type: Rule) -> bool {
        if let Some(token) = it.peek() {
            token.as_rule() == token_type
        } else {
            false
        }
    }

    fn parse_expr(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        match it.peek().unwrap().as_rule() {
            Rule::Call => todo!(),
            Rule::Identifier => {
                let target: Token = it.next().unwrap().into();
                let target = env.push_expr(Expr::Variable(VariableExpr { name: target }));
                let op: Token = it.next().unwrap().into(); // discard Equal
                let value = Self::parse_expr(env, it.next().unwrap())?;

                if let Expr::Variable(variable) = env.get_expr(target).unwrap() {
                    Ok(env.push_expr(Expr::Assign(AssignExpr {
                        name: variable.name.clone(),
                        value,
                    })))
                } else {
                    Err(Self::report(
                        op.line,
                        op.col,
                        &op.lexeme,
                        "Invalid assignment target.".to_string(),
                    ))
                }
            }
            Rule::LogicOr => Self::parse_or(env, it.next().unwrap()),
            _ => unreachable!(),
        }
    }

    fn parse_or(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_and(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_and(env, it.next().unwrap())?;
            let new = Expr::Logical(LogicalExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_and(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_equality(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_equality(env, it.next().unwrap())?;
            let new = Expr::Logical(LogicalExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_equality(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_comp(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_comp(env, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_comp(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_term(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_term(env, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_term(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_factor(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_factor(env, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_factor(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_unary(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let op = token.into();
            let rhs = Self::parse_unary(env, it.next().unwrap())?;
            let new = Expr::Binary(BinaryExpr { lhs: expr, rhs, op });
            expr = env.push_expr(new);
        }

        Ok(expr)
    }

    fn parse_unary(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let ret = if Self::peek_match(&it, Rule::Call) {
            Self::parse_call(env, it.next().unwrap())?
        } else {
            let token = it.next().unwrap();
            let op = token.into();
            let rhs = Self::parse_unary(env, it.next().unwrap())?;
            let expr = Expr::Unary(UnaryExpr { rhs, op });
            env.push_expr(expr)
        };
        Ok(ret)
    }

    fn parse_call(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_primary(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            // TODO: support visiting
            let new = match token.as_rule() {
                Rule::LParen => {
                    let mut args = if Self::peek_match(&it, Rule::RParen) {
                        Vec::<usize>::new()
                    } else {
                        Self::parse_args(env, it.next().unwrap())?
                    };

                    let token = it.next().unwrap();
                    let (line, col) = token.line_col();
                    let op = token.into();
                    Expr::Call(CallExpr {
                        callee: expr,
                        args,
                        op,
                    })
                }
                Rule::Dot => {
                    let op = token.into();
                    let rhs = env.push_expr(Expr::Variable(VariableExpr {
                        name: it.next().unwrap().into(),
                    }));
                    Expr::Logical(LogicalExpr { lhs: expr, rhs, op })
                }
                _ => unreachable!(),
            };
            expr = env.push_expr(new);
        }
        Ok(expr)
    }

    fn parse_args(env: &mut AST, expr: Pair<'_, Rule>) -> Result<Vec<usize>, Error> {
        let mut it = expr.into_inner();
        let mut args: Vec<usize> = Vec::new();

        let expr = Self::parse_expr(env, it.next().unwrap())?;
        args.push(expr);

        while let Some(token) = it.next() {
            if args.len() >= 255 {
                let (line, col) = token.line_col();
                return Err(Self::report(
                    line,
                    col,
                    token.as_str(),
                    "Cannot have more than 255 arguments.".to_string(),
                ));
            }
            args.push(Self::parse_expr(env, it.next().unwrap())?);
        }

        Ok(args)
    }

    fn parse_primary(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let expr = match it.peek().unwrap().as_rule() {
            Rule::False => Expr::Literal(Literal::Bool(false)),
            Rule::True => Expr::Literal(Literal::Bool(true)),
            Rule::Nil => Expr::Literal(Literal::Nil),
            Rule::Number => {
                let token = it.next().unwrap();
                let _t = token.as_rule();
                Expr::Literal(Literal::Number(match token.as_str().parse() {
                    Ok(f) => f,
                    Err(err) => {
                        let (line, col) = token.line_col();
                        return Err(Self::report(line, col, token.as_str(), err.to_string()));
                    }
                }))
            }
            Rule::String => {
                let lexeme = it.next().unwrap().as_str();
                Expr::Literal(Literal::String(lexeme[1..lexeme.len() - 1].to_string()))
            }
            Rule::Identifier => {
                let token = it.next().unwrap();
                let name = token.into();
                Expr::Variable(VariableExpr { name })
            }
            Rule::LParen => {
                it.next();
                let expr = Self::parse_expr(env, it.next().unwrap())?;
                Expr::Grouping(GroupingExpr { expr })
            }
            _ => unreachable!(),
        };
        Ok(env.push_expr(expr))
    }

    fn parse_stmt(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let stmt = expr.into_inner().next().unwrap();
        match stmt.as_rule() {
            Rule::Block => Self::parse_block(env, stmt),
            Rule::ExprStmt => Self::parse_expr_stmt(env, stmt),
            Rule::ForStmt => Self::parse_for(env, stmt),
            Rule::IfStmt => Self::parse_if(env, stmt),
            Rule::PrintStmt => Self::parse_print(env, stmt),
            Rule::ReturnStmt => Self::parse_return(env, stmt),
            Rule::WhileStmt => Self::parse_while(env, stmt),
            _ => unreachable!(),
        }
    }

    fn parse_block(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard LBrace
        let mut stmts: Vec<usize> = Vec::new();

        for decl in it {
            if decl.as_rule() == Rule::RBrace {
                break;
            }
            stmts.push(Self::parse_decl(env, decl)?);
        }

        Ok(env.push_stmt(Stmt::Block(Block { stmts })))
    }

    fn parse_expr_stmt(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let value = Self::parse_expr(env, it.next().unwrap())?;
        let stmt = Stmt::Expr(ExprStmt { expr: value });
        Ok(env.push_stmt(stmt))
    }

    fn parse_for(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        // discard For LParen
        it.next();
        it.next();

        let init = it.next().unwrap();
        let init = match init.as_rule() {
            Rule::SemiColon => None,
            Rule::VarDecl => Some(Self::parse_var(env, init)?),
            Rule::ExprStmt => Some(Self::parse_expr_stmt(env, init)?),
            _ => unreachable!(),
        };

        let cond = if it.peek().unwrap().as_rule() == Rule::Expr {
            Self::parse_expr(env, it.next().unwrap())?
        } else {
            env.push_expr(Expr::Literal(Literal::Bool(true)))
        };

        it.next(); // discard SemiColon

        let post = if it.peek().unwrap().as_rule() == Rule::Expr {
            Some(Self::parse_expr(env, it.next().unwrap())?)
        } else {
            None
        };

        it.next(); // discard RParen

        let mut body = Self::parse_stmt(env, it.next().unwrap())?;

        if let Some(post) = post {
            let post = env.push_stmt(Stmt::Expr(ExprStmt { expr: post }));
            body = env.push_stmt(Stmt::Block(Block {
                stmts: vec![body, post],
            }))
        }

        body = env.push_stmt(Stmt::While(WhileStmt { cond, body }));

        if let Some(init) = init {
            body = env.push_stmt(Stmt::Block(Block {
                stmts: vec![init, body],
            }));
        }

        Ok(body)
    }

    fn parse_if(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard if
        it.next(); // discard LParen

        let cond = Self::parse_expr(env, it.next().unwrap())?;
        it.next(); // discard RParen

        let then = Self::parse_stmt(env, it.next().unwrap())?;
        let elze = if it.next().is_some() {
            Some(Self::parse_stmt(env, it.next().unwrap())?)
        } else {
            None
        };

        Ok(env.push_stmt(Stmt::If(IfStmt { cond, then, elze })))
    }

    fn parse_print(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard print
        let value = Self::parse_expr(env, it.next().unwrap())?;
        let stmt = Stmt::Print(PrintStmt { expr: value });
        Ok(env.push_stmt(stmt))
    }

    fn parse_return(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let keyword = it.next().unwrap().into();

        let value = if it.peek().unwrap().as_rule() == Rule::Expr {
            Self::parse_expr(env, it.next().unwrap())?
        } else {
            env.push_expr(Expr::Literal(Literal::Nil))
        };

        Ok(env.push_stmt(Stmt::Return(ReturnStmt { value, keyword })))
    }

    fn parse_while(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        // discard While LParen
        it.next();
        it.next();

        let cond = Self::parse_expr(env, it.next().unwrap())?;
        it.next(); // discard RParen

        let stmt = Self::parse_stmt(env, it.next().unwrap())?;

        Ok(env.push_stmt(Stmt::While(WhileStmt { cond, body: stmt })))
    }

    fn parse_decl(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let decl = it.next().unwrap();
        match decl.as_rule() {
            Rule::VarDecl => Self::parse_var(env, decl),
            Rule::FunDecl => todo!(),
            Rule::ClassDecl => todo!(),
            Rule::Stmt => Self::parse_stmt(env, decl),
            _ => unreachable!(),
        }
    }

    fn parse_var(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        it.next(); // discard Var

        let name: Token = it.next().unwrap().into();

        let init = if it.next().unwrap().as_rule() == Rule::Equal {
            Some(Self::parse_expr(env, it.next().unwrap())?)
        } else {
            None
        };

        Ok(env.push_stmt(Stmt::Var(VarStmt { name, init })))
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

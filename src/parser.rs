use pest::Parser as PestParser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;

use crate::ast::*;
use crate::error::Error;

#[derive(Parser)]
#[grammar = "lox.pest"]
struct RawParser;

pub struct Parser;

impl Parser {
    fn report(line: usize, col: usize, content: &str, msg: String) -> Error {
        Error::report(line, col, content, msg);
        Error::Parse
    }

    pub fn parse(input: &str) -> Result<AST, Error> {
        let tree = match RawParser::parse(Rule::Expr, input) {
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
                    "Invalid syntax.".to_string(),
                ));
            }
        }
        .next()
        .unwrap(); // expr

        let mut env = AST::new();
        let expr = Self::parse_expr(&mut env, tree)?;
        env.set_root(expr);
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
        let expr = match it.peek().unwrap().as_rule() {
            Rule::Call => todo!(),
            Rule::Identifier => todo!(),
            Rule::LogicOr => Self::parse_or(env, it.next().unwrap())?,
            _ => unreachable!(),
        };
        if Self::peek_match(&it, Rule::Equal) {
            let equals = it.peek().unwrap();
            let value = Self::parse_expr(env, it.next().unwrap())?;

            if let Expr::Variable(var) = env.get_expr(expr).unwrap() {
                let name = var.name.clone();
                let value = Expr::Assign(AssignExpr { name, value });
                Ok(env.push_expr(value))
            } else {
                let (line, col) = equals.line_col();
                Err(Self::report(
                    line,
                    col,
                    equals.as_str(),
                    "Invalid assignment target.".to_string(),
                ))
            }
        } else {
            Ok(expr)
        }
    }

    fn parse_or(env: &mut AST, expr: Pair<'_, Rule>) -> Result<usize, Error> {
        let mut it = expr.into_inner();
        let mut expr = Self::parse_and(env, it.next().unwrap())?;

        while let Some(token) = it.next() {
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
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
            let mut args = if Self::peek_match(&it, Rule::RParen) {
                Vec::<usize>::new()
            } else {
                Self::parse_args(env, it.next().unwrap())?
            };

            let token = it.next().unwrap();
            let (line, col) = token.line_col();
            let op = Token::new(line, col, token.as_str());
            let new = Expr::Call(CallExpr {
                callee: expr,
                args,
                op,
            });
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
                let (line, col) = token.line_col();
                let name = Token::new(line, col, token.as_str());
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

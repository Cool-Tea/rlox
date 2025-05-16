#[derive(Debug, Clone)]
pub struct Token {
    pub line: usize,
    pub col: usize,
    pub lexeme: String,
}

impl Token {
    pub fn new(line: usize, col: usize, src: &str) -> Self {
        Token {
            line,
            col,
            lexeme: src.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(AssignExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    Grouping(GroupingExpr),
    Literal(Literal),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub name: Token,
    pub value: usize,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub lhs: usize,
    pub rhs: usize,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: usize,
    pub args: Vec<usize>,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct GroupingExpr {
    pub expr: usize,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub lhs: usize,
    pub rhs: usize,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub rhs: usize,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub name: Token,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Block),
    Expr(ExprStmt),
    Func(FuncStmt),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    Var(VarStmt),
    While(WhileStmt),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: usize, // expr
}

#[derive(Debug, Clone)]
pub struct FuncStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<usize>, // stmts
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: usize,         // expr
    pub then: usize,         // stmt
    pub elze: Option<usize>, // stmt
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub expr: usize, // expr
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: usize, // expr
    pub keyword: Token,
}

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub name: Token,
    pub init: Option<usize>, // expr
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: usize, // expr
    pub body: usize, // stmt
}

pub struct AST {
    root: usize,
    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,
}

impl AST {
    pub fn new() -> Self {
        AST {
            root: 0,
            exprs: Vec::new(),
            stmts: Vec::new(),
        }
    }

    pub fn push_expr(&mut self, expr: Expr) -> usize {
        self.exprs.push(expr);
        self.exprs.len() - 1
    }

    pub fn get_expr(&self, i: usize) -> Option<&Expr> {
        self.exprs.get(i)
    }

    pub fn push_stmt(&mut self, stmt: Stmt) -> usize {
        self.stmts.push(stmt);
        self.stmts.len() - 1
    }

    pub fn get_stmt(&self, i: usize) -> Option<&Stmt> {
        self.stmts.get(i)
    }

    pub fn root(&self) -> usize {
        self.root
    }

    pub fn set_root(&mut self, root: usize) {
        self.root = root;
    }
}

pub mod util {
    use super::*;
    use crate::visitor::*;

    pub struct ASTPrinter;

    impl ASTPrinter {
        fn parenthesize(&mut self, name: &str, exprs: &[usize], env: &AST) -> String {
            let mut ret = format!("({}", name);

            for &expr in exprs {
                ret.push(' ');
                ret.push_str(&self.visit_expr(expr, env));
            }

            ret.push(')');

            ret
        }

        pub fn print(expr: usize, env: &AST) {
            let mut printer = ASTPrinter;
            println!("{}", printer.visit_expr(expr, env));
        }
    }

    impl ExprVisitor<String> for ASTPrinter {
        fn visit_assign(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Assign(expr) => expr,
                _ => unreachable!(),
            };
            self.parenthesize("assign", &[expr.value], env)
        }

        fn visit_binary(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Binary(expr) => expr,
                _ => unreachable!(),
            };
            self.parenthesize(&expr.op.lexeme, &[expr.lhs, expr.rhs], env)
        }

        fn visit_call(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Call(expr) => expr,
                _ => unreachable!(),
            };
            let mut args = vec![expr.callee];
            for &arg in expr.args.iter() {
                args.push(arg);
            }
            self.parenthesize("call", &args, env)
        }

        fn visit_grouping(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Grouping(expr) => expr,
                _ => unreachable!(),
            };
            self.parenthesize("group", &[expr.expr], env)
        }

        fn visit_literal(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Literal(expr) => expr,
                _ => unreachable!(),
            };
            match expr {
                Literal::Number(f) => f.to_string(),
                Literal::String(s) => s.clone(),
                Literal::Bool(b) => b.to_string(),
                Literal::Nil => "nil".to_string(),
            }
        }

        fn visit_logical(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Logical(expr) => expr,
                _ => unreachable!(),
            };
            self.parenthesize(&expr.op.lexeme, &[expr.lhs, expr.rhs], env)
        }

        fn visit_unary(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Unary(expr) => expr,
                _ => unreachable!(),
            };
            self.parenthesize(&expr.op.lexeme, &[expr.rhs], env)
        }

        fn visit_variable(&mut self, expr: usize, env: &AST) -> String {
            let expr = match env.get_expr(expr).unwrap() {
                Expr::Variable(expr) => expr,
                _ => unreachable!(),
            };
            expr.name.lexeme.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::visitor::ExprVisitor;

    #[test]
    fn test_astprinter() {
        // -123 * (45.67)
        let mut env = AST::new();
        let mut lhs = env.push_expr(Expr::Literal(Literal::Number(123.0)));
        lhs = env.push_expr(Expr::Unary(UnaryExpr {
            rhs: lhs,
            op: Token::new(1, 1, "-"),
        }));
        let mut rhs = env.push_expr(Expr::Literal(Literal::Number(45.67)));
        rhs = env.push_expr(Expr::Grouping(GroupingExpr { expr: rhs }));
        let expr = env.push_expr(Expr::Binary(BinaryExpr {
            lhs,
            rhs,
            op: Token::new(1, 6, "*"),
        }));

        let mut printer = util::ASTPrinter;
        assert_eq!("(* (- 123) (group 45.67))", &printer.visit_expr(expr, &env));
    }
}

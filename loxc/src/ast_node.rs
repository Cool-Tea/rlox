#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Box<Decl>>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Class(ClassDecl),
    Fun(FunDecl),
    Var(VarDecl),
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: String,
    pub superclass: Option<String>,
    pub methods: Vec<Box<Func>>,
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    pub func: Box<Func>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Block>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprStmt),
    For(ForStmt),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum ForInitializer {
    VarDecl(VarDecl),
    ExprStmt(ExprStmt),
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<Box<ForInitializer>>,
    pub cond: Option<Box<Expr>>,
    pub post: Option<Box<Expr>>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Box<Expr>,
    pub then_body: Box<Stmt>,
    pub else_body: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub decls: Vec<Box<Decl>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    Variable(String),
    This,
    Super,
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    /* Binary operator */
    Assign,
    Or,
    And,
    Ne,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Visit, // obj.member

    /* Unary operator */
    Not,
    Minus,
    Group, // (expr)
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: Operator,
    pub expr: Box<Expr>,
}

// func()
#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
        let num: f64 = 2.5;
        let string = "Hello AST".to_string();
        let var = "a".to_string();
        let expr = Expr::Binary(BinaryExpr {
            op: Operator::Assign,
            lhs: Box::new(Expr::Variable(var)),
            rhs: Box::new(Expr::Binary(BinaryExpr {
                op: Operator::Add,
                lhs: Box::new(Expr::Number(num)),
                rhs: Box::new(Expr::String(string)),
            })),
        });
        match expr {
            Expr::Binary(expr) => println!("{:?}", expr),
            _ => unreachable!(),
        }
    }
}

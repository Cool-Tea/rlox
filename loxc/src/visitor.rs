use crate::parser::Rule;
use pest::iterators::Pair;

pub trait LoxVisitor {
    type Output;

    fn visit(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        match tree.as_rule() {
            Rule::Program => self.visit_program(tree),
            Rule::Decl => self.visit_decl(tree),
            Rule::ClassDecl => self.visit_class_decl(tree),
            Rule::FunDecl => self.visit_fun_decl(tree),
            Rule::VarDecl => self.visit_var_decl(tree),
            Rule::Stmt => self.visit_stmt(tree),
            Rule::ExprStmt => self.visit_expr_stmt(tree),
            Rule::ForStmt => self.visit_for_stmt(tree),
            Rule::IfStmt => self.visit_if_stmt(tree),
            Rule::PrintStmt => self.visit_print_stmt(tree),
            Rule::RetStmt => self.visit_ret_stmt(tree),
            Rule::WhileStmt => self.visit_while_stmt(tree),
            Rule::Block => self.visit_block(tree),
            Rule::Expr => self.visit_expr(tree),
            Rule::LogicOr => self.visit_logic_or(tree),
            Rule::LogicAnd => self.visit_logic_and(tree),
            Rule::Equal => self.visit_equal(tree),
            Rule::Comp => self.visit_comp(tree),
            Rule::Term => self.visit_term(tree),
            Rule::Factor => self.visit_factor(tree),
            Rule::Unary => self.visit_unary(tree),
            Rule::Call => self.visit_call(tree),
            Rule::Primary => self.visit_primary(tree),
            Rule::Func => self.visit_func(tree),
            Rule::Params => self.visit_params(tree),
            Rule::Args => self.visit_args(tree),
            Rule::Num => self.visit_num(tree),
            Rule::String => self.visit_string(tree),
            Rule::Ident => self.visit_ident(tree),
            Rule::Class => self.visit_class(tree),
            Rule::Inherit => self.visit_inherit(tree),
            Rule::LBrace => self.visit_l_brace(tree),
            Rule::RBrace => self.visit_r_brace(tree),
            Rule::LParen => self.visit_l_paren(tree),
            Rule::RParen => self.visit_r_paren(tree),
            Rule::Dot => self.visit_dot(tree),
            Rule::Fun => self.visit_fun(tree),
            Rule::Var => self.visit_var(tree),
            Rule::Assign => self.visit_assign(tree),
            Rule::SemiColon => self.visit_semi_colon(tree),
            Rule::For => self.visit_for(tree),
            Rule::If => self.visit_if(tree),
            Rule::Else => self.visit_else(tree),
            Rule::Print => self.visit_print(tree),
            Rule::Return => self.visit_return(tree),
            Rule::While => self.visit_while(tree),
            Rule::Or => self.visit_or(tree),
            Rule::And => self.visit_and(tree),
            Rule::Eq => self.visit_eq(tree),
            Rule::Ne => self.visit_ne(tree),
            Rule::Gt => self.visit_gt(tree),
            Rule::Ge => self.visit_ge(tree),
            Rule::Lt => self.visit_lt(tree),
            Rule::Le => self.visit_le(tree),
            Rule::Add => self.visit_add(tree),
            Rule::Sub => self.visit_sub(tree),
            Rule::Mul => self.visit_mul(tree),
            Rule::Div => self.visit_div(tree),
            Rule::Not => self.visit_not(tree),
            Rule::Minus => self.visit_minus(tree),
            Rule::True => self.visit_true(tree),
            Rule::False => self.visit_false(tree),
            Rule::Nil => self.visit_nil(tree),
            Rule::This => self.visit_this(tree),
            Rule::Super => self.visit_super(tree),
            Rule::Comma => self.visit_comma(tree),
            _ => None,
        }
    }

    fn visit_program(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_decl(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_class_decl(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_fun_decl(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_var_decl(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_expr_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_for_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_if_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_print_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_ret_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_while_stmt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_block(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_expr(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_logic_or(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_logic_and(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_equal(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_comp(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_term(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_factor(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_unary(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_call(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_primary(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_func(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_params(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_args(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_num(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_string(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_ident(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_class(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_inherit(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_l_brace(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_r_brace(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_l_paren(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_r_paren(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_dot(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_fun(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_var(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_assign(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_semi_colon(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_for(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_if(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_else(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_print(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_return(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_while(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_or(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_and(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_eq(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_ne(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_gt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_ge(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_lt(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_le(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_add(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_sub(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_mul(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_div(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_not(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_minus(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_true(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_false(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_nil(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_this(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_super(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }

    fn visit_comma(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
        for child in tree.into_inner() {
            self.visit(child);
        }
        None
    }
}

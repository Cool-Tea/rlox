pub mod parser;
pub mod value;
pub mod visitor;

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::{
        parser::{LoxParser, Rule},
        value::LoxValue,
        visitor::LoxVisitor,
    };

    struct TestVisitor;

    impl LoxVisitor for TestVisitor {
        type Output = ();
        fn visit_string(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            let literal = tree.as_str();
            let val = LoxValue::String(literal[1..literal.len() - 1].to_string());
            println!("[{line}:{col}] String = {:#?}", val);
            None
        }
        fn visit_ident(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            println!("[{line}:{col}] Identifier = {}", tree.as_str());
            None
        }
        fn visit_num(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            let val = LoxValue::Number(tree.as_str().parse().unwrap());
            println!("[{line}:{col}] Number = {:#?}", val);
            None
        }
    }

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
        let mut visitor = TestVisitor {};
        visitor.visit(tree);
    }

    #[test]
    fn test2() {
        let lox_str = "\
            var a = 3.1415926;\n\
            print a;\n\
            ";
        let tree = LoxParser::parse(Rule::Program, lox_str)
            .unwrap()
            .next()
            .unwrap();
        let mut visitor = TestVisitor {};
        visitor.visit(tree);
    }
}

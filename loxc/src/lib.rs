pub mod parser;
pub mod visitor;

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::{
        parser::{LoxParser, Rule},
        visitor::Visitor,
    };

    struct TestVisitor;

    impl Visitor for TestVisitor {
        type Output = ();
        fn visit_string(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            println!("[{line}:{col}] String = {}", tree.as_str());
            None
        }
        fn visit_ident(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            println!("[{line}:{col}] Identifier = {}", tree.as_str());
            None
        }
        fn visit_num(&mut self, tree: pest::iterators::Pair<'_, Rule>) -> Option<Self::Output> {
            let (line, col) = tree.line_col();
            println!("[{line}:{col}] Number = {}", tree.as_str());
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

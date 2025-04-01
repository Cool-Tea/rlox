use pest_derive::Parser;

#[derive(Parser, Debug, Clone, Copy)]
#[grammar = "lox.pest"]
pub struct LoxParser;

#[cfg(test)]
mod tests {
    use pest::{Parser, iterators::Pairs};

    use super::*;

    fn print_recur(tree: Pairs<'_, Rule>) {
        for node in tree {
            println!("{:?}", node);
            print_recur(node.into_inner());
        }
    }

    #[test]
    fn parse_string() {
        let lox_str = "\"hello world!\"";
        let tree = LoxParser::parse(Rule::String, lox_str).expect("failed to parse string");
        print_recur(tree);
    }
}

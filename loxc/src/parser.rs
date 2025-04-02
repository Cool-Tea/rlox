use pest_derive::Parser;

#[derive(Parser)]
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
        let lox_str = "fun returnFunction() {
  var outside = \"outside\";

  fun inner() {
    print outside;
  }

  return inner;
}

var fn = returnFunction();
fn();";
        let tree = LoxParser::parse(Rule::Program, lox_str).expect("failed to parse string");
        print_recur(tree);
    }
}

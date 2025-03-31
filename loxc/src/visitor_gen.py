import re

def to_snake_case(name) -> str:
  return re.sub(r'(?<!^)(?=[A-Z])', '_', name).lower()

def generate_visit(name) -> str:
  code = f"""
  fn visit_{to_snake_case(name)}(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {{
    for child in tree.into_inner() {{
      self.visit(child);
    }}
    None
  }}
"""
  return code

def generate_visitor():

  rules = []
  with open('lox.pest', 'r', encoding='utf-8') as f:
    for line in f:
      if re.match(r'^(.*?) = .*', line):
        name = re.match(r'^(.*?) = .*', line).group(1)
        rules.append(name)

  code = """use pest::iterators::Pair;
use crate::parser::Rule;

pub trait Visitor {
  type Output;
  
  fn visit(&mut self, tree: Pair<'_, Rule>) -> Option<Self::Output> {
    match tree.as_rule() {
      Rule::EOI => None,
"""
  for rule in rules:
    code += f'      Rule::{rule} => self.visit_{to_snake_case(rule)}(tree),\n'
  code += """    }
  }
"""

  for rule in rules:
    code += generate_visit(rule)
      
  code += '}\n'

  with open('visitor.rs', 'w', encoding='utf-8') as f:
    f.write(code)
  print('Visitor generated successfully.')

if __name__ == '__main__':
  generate_visitor()
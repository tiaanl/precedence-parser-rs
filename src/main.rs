mod lexer;
mod parser;

use crate::lexer::Token;
use crate::parser::Parser;

fn main() {
    let tokens = vec![
        Token::Constant(1),
        Token::Minus,
        Token::Constant(2),
        Token::Star,
        Token::Constant(3),
    ];

    let mut parser = Parser::new(tokens);
    match parser.parse_expression() {
        Ok(expr) => println!("Expression: {} = {}", expr, expr.evaluate()),
        Err(err) => eprintln!("ERROR: {}", err),
    }
}

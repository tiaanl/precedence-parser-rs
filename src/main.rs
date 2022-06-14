mod lexer;
mod parser;

use crate::lexer::{Lexer, Token};
use crate::parser::parse_expression;
use clap::Parser;

#[derive(Parser)]
struct Args {
    source: String,
}

fn main() {
    let args = Args::parse();

    let lexer = Lexer::new(args.source.as_str());
    match parse_expression(lexer) {
        Ok(expr) => println!("{} = {}", expr, expr.evaluate()),
        Err(err) => {
            eprintln!("ERROR: {}", err);
            println!("{}", args.source);
            for _ in 0..err.pos() {
                print!(" ");
            }
            println!("^");
        }
    }
}

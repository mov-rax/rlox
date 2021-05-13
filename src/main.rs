#[macro_use]
extern crate lazy_static;
mod lox_lib;

use lox_lib::lox_lib::parser::Expr;
use lox_lib::lox_lib::lexer::Token;
fn main() {
    let tokens = lox_lib::lox_lib::lexer::Scanner::from(r#"(3+2)*3==15"#).scan_tokens().unwrap();
    let mut parser = lox_lib::lox_lib::parser::Parser::from(tokens);
    let expr = parser.parse();


    //println!("{:?}", tokens);
    println!("{:?}", expr);
    println!("Hello, world!");
}

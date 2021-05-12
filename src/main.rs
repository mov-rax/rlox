#[macro_use]
extern crate lazy_static;
mod lox_lib;

use lox_lib::lox_lib::parser::Expr;
use lox_lib::lox_lib::lexer::Token;
fn main() {
    let tokens = lox_lib::lox_lib::lexer::Scanner::from(r#"var whatever = {420}"#).scan_tokens();
    let expr = Expr::Binary(Token::Minus, Box::new(
        Expr::Unary(Token::Minus, Box::new(Expr::Literal(Token::Number(123.0))))
    ), Box::new(
        Expr::Grouping(
            Box::new(Expr::Literal(Token::Number(45.67)))
        )
    ));

    println!("{:?}", tokens);
    println!("{:?}", expr);
    println!("Hello, world!");
}

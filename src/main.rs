mod lox_lib;

use lox_lib::interpreter::Interpreter;
use lox_lib::lexer::Token;
use lox_lib::parser::Expr;
fn main() {
    let time = std::time::Instant::now();
    let tokens = lox_lib::lexer::Scanner::from(r#" -"free""#)
        .scan_tokens()
        .unwrap();
    println!("{:?}", &tokens);
    let mut parser = lox_lib::parser::Parser::from(tokens);
    let expr = parser.parse();

    match expr {
        Ok(v) => {
            println!("{:?}", &v);
            let mut interpreter = Interpreter::from(v);
            let result = interpreter.execute();
            println!("{:?}", result);
            println!("{}", time.elapsed().as_micros());
        },
        Err(e) => println!("{:?}", e),
    }

    //println!("{:?}", tokens);
    //println!("{:?}", expr);
    //println!("Hello, world!");
}

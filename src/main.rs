mod lox_lib;

use lox_lib::interpreter::Interpreter;
use lox_lib::lexer::Token;
use lox_lib::parser::Expr;
fn main() {
    let text = std::fs::read_to_string("test.lox").unwrap();
    let tokens = lox_lib::lexer::Scanner::from(text.as_str())
        .scan_tokens()
        .unwrap();
    println!("{:?}", &tokens);
    let mut parser = lox_lib::parser::Parser::from(tokens);
    let program = parser.parse_all();

    match program{
        Ok(stmts) => {
            let mut interpreter = Interpreter::new();
            let result = interpreter.execute_all(stmts);
            if let Err(e) = result{
                println!("{:?}", e);
            }
        },
        Err(e) => println!("{:?}", e)
    }

    // let expr = parser.parse();

    // match expr {
    //     Ok(v) => {
    //         println!("{:?}", &v);
    //         let mut interpreter = Interpreter::new();
    //         let result = interpreter.execute(v);
    //         println!("{:?}", result);
    //         println!("{}", time.elapsed().as_micros());
    //     },
    //     Err(e) => println!("{:?}", e),
    // }

    //println!("{:?}", tokens);
    //println!("{:?}", expr);
    //println!("Hello, world!");
}

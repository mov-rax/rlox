

pub mod lox_lib{

    pub mod lexer{
        use itertools::{Itertools, PeekingNext};
        use std::str::{Chars, FromStr};
        use itertools::__std_iter::Peekable;
        use std::collections::HashMap;

        #[derive(Debug, PartialEq, Clone)]
        pub enum Token<'a>{
            LeftParen,
            RightParen,
            LeftBrace,
            RightBrace,
            Comma,
            Dot,
            Minus,
            Plus,
            SemiColon,
            Slash,
            Star,
            Bang,
            BangEqual,
            Equal,
            EqualEqual,
            Greater,
            GreaterEqual,
            Less,
            LessEqual,
            Identifier(&'a str),
            String(String),
            Number(f64),
            And,
            Class,
            Else,
            False,
            Fun,
            For,
            If,
            Nil,
            Or,
            Print,
            Return,
            Super,
            This,
            True,
            Var,
            While,
            Eof
        }

        impl<'a> Token<'a>{

        }

        macro_rules! advance {
        ($s:expr, $val:expr, $yes:expr, $no:expr) => {
        {
            let mut temp = $no;
            if let Some(n) = $s.stream.as_mut().unwrap().peek() {
                if *n == $val {
                    temp = $yes;
                    let e = $s.stream.as_mut().unwrap().next();
                    println!("Consumed: {:?}", e);
                     $s.current += 1;
                     $s.total_current += 1;
                }
            }
            temp
        }
        }
    }

        pub struct Scanner<'a>{
            src:&'a str,
            stream:Option<Peekable<Chars<'a>>>,
            total_current: usize,
            current: usize,
            line: usize
        }

        impl<'a> Scanner<'a>{
            pub fn from(source: &'a str) -> Self{
                Self {
                    src: source,
                    stream: None,
                    total_current: 0,
                    current: 0,
                    line: 1
                }
            }

            fn scan_token(&mut self) -> Result<Token<'a>, String>{
                //let mut stream = self.src.get(self.current..).unwrap().chars();
                let mut tok= Token::Eof;
                // char_match!(self, '=', Token::BangEqual, Token::Bang)
                if let Some(c) = self.stream.as_mut().unwrap().next(){
                    self.current += 1;
                    self.total_current += 1;
                    match c{
                        '(' => tok = Token::LeftParen,
                        ')' => tok = Token::RightParen,
                        '{' => tok = Token::LeftBrace,
                        '}' => tok = Token::RightBrace,
                        ',' => tok = Token::Comma,
                        '.' => tok = Token::Dot,
                        '+' => tok = Token::Plus,
                        '-' => tok = Token::Minus,
                        ';' => tok = Token::SemiColon,
                        '*' => tok = Token::Star,
                        '!' => tok = advance!(self, '=', Token::BangEqual, Token::Bang),
                        '=' => tok = advance!(self, '=', Token::EqualEqual, Token::Equal),
                        '<' => tok = advance!(self, '=', Token::LessEqual, Token::Less),
                        '>' => tok = advance!(self, '=', Token::GreaterEqual, Token::Greater),
                        '/' => tok = if let Some(n) = self.stream.as_mut().unwrap().peek(){
                            let mut tok = Token::Eof;
                            if *n == '/'{
                                while let Some(n) = self.stream.as_mut().unwrap().next(){
                                    self.current += 1;
                                    self.total_current += 1;
                                    if n == '\n'{
                                        tok = self.scan_token()?;
                                        break;
                                    }
                                }
                            }
                            tok
                        } else { Token::Slash },
                        ' ' | '\r' | '\t' | '\n' => {
                            let mut line_inc = 0;
                            let mut current_inc = 0;
                            while let Some(_) = self.stream.as_mut().unwrap().peeking_next(|n| match n{
                                ' ' | '\r' | '\t' => {
                                    current_inc += 1;
                                    true
                                },
                                '\n'=> {
                                    line_inc += 1;
                                    true
                                }
                                _ => false
                            }){} // Returns None if not whitespace
                            self.line += line_inc;
                            if line_inc != 0{ // resets character count after each newline.
                                self.current = 0;
                            } else {
                                self.current += current_inc;
                                self.total_current += current_inc;
                            }
                            tok = self.scan_token()?;
                        },
                        '"' => tok = {
                            // let string = self.stream.as_mut().unwrap()
                            //     .take_while(|x| *x != '"')
                            //     .collect::<String>();
                            let mut string = String::new();
                            while let Some(c) = self.stream.as_mut().unwrap().next(){
                                if c != '"'{
                                    string.push(c);
                                } else {
                                    break
                                }
                            }
                            if self.stream.as_mut().unwrap().peek() == None{
                                return Err(format!("Non-terminating string at line {}", self.line));
                            }
                            Token::String(string)
                        },
                        '0'..='9' => {
                            let mut len = 0;
                            while let Some(n) = self.stream.as_mut().unwrap().peek(){
                                if n.is_digit(10){
                                    let _ = self.stream.as_mut().unwrap().next();
                                    len += 1;
                                } else {
                                    break
                                }
                            }
                            if let Some(n) = self.stream.as_mut().unwrap().peek(){
                                if *n == '.'{
                                    let _ = self.stream.as_mut().unwrap().next();
                                    len += 1;
                                    if let Some(n) = self.stream.as_mut().unwrap().peek(){
                                        if !n.is_digit(10){
                                            return Err(format!("Invalid Number at {}:{}", self.line, self.current))
                                        }
                                    }
                                    while let Some(n) = self.stream.as_mut().unwrap().peek(){
                                        if n.is_digit(10){
                                            let _ = self.stream.as_mut().unwrap().next();
                                            len += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                }
                            }
                            //tok = Token::Number(f64::from_str(buf.as_str()).unwrap());
                            let stringy = self.src.get(self.total_current-1..self.total_current+len).unwrap();
                            self.total_current += len;
                            self.current += len;
                            tok = Token::Number(f64::from_str(stringy).unwrap());
                        },
                        'a'..='z' | 'A'..='Z' | '_' => { //Identifier
                            let mut len = 0;
                            while let Some(n) = self.stream.as_mut().unwrap().peek(){
                                if n.is_alphabetic(){
                                    let _ = self.stream.as_mut().unwrap().next();
                                    len += 1;
                                } else {
                                    break
                                }
                            }

                            let stringy = self.src.get(self.total_current-1..self.total_current+len).unwrap();

                            if let Some(n) = match stringy{
                                "if" => Some(Token::If),
                                "else" => Some(Token::Else),
                                "class" => Some(Token::Class),
                                "for" => Some(Token::For),
                                "while" => Some(Token::While),
                                "fun" => Some(Token::Fun),
                                "true" => Some(Token::True),
                                "false" => Some(Token::False),
                                "and" => Some(Token::And),
                                "or" => Some(Token::Or),
                                "nil" => Some(Token::Nil),
                                "this" => Some(Token::This),
                                "super" => Some(Token::Super),
                                "var" => Some(Token::Var),
                                "return" => Some(Token::Return),
                                "print" => Some(Token::Print),
                                _ => None
                            }{
                                tok = n.to_owned();
                            } else {
                                tok = Token::Identifier(stringy);
                            }

                            self.total_current += len;
                            self.current += len;
                        }
                        _ => return Err(format!("Unexpected character at {}:{}", self.line, self.current))
                    };

                } else {
                    tok = Token::Eof;
                }

                Ok(tok)
            }

            pub fn scan_tokens(&mut self) -> Result<Vec<Token<'a>>, String>{
                self.stream = Some(self.src.chars().peekable());
                let mut tokens = Vec::new();
                loop {
                    let tok = self.scan_token()?;
                    if tok != Token::Eof{
                        tokens.push(tok);
                    } else {
                        break
                    }
                }
                return Ok(tokens)
            }
        }
    }

    pub mod parser{
        use itertools::Itertools;
        use std::str::{Chars, FromStr};
        use super::lexer::Token;
        use itertools::__std_iter::Peekable;
        use std::slice::Iter;
        use anyhow::{Result, Error, Context};

        #[derive(Debug, Clone)]
        pub enum Expr<'a>{
            Unary(Token<'a>, Box<Expr<'a>>),
            Binary(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
            Literal(Token<'a>),
            Grouping(Box<Expr<'a>>)
        }

        pub struct Parser<'a>{
            tokens: Vec<Token<'a>>,
            current: usize,
        }

        impl<'a> Parser<'a>{
            pub fn from(tokens: Vec<Token<'a>>) -> Self{
                Self { tokens, current:0 }
            }

            pub fn parse(&'a mut self) -> Expr<'a>{
                self.expression()
            }

            fn peek(&self) -> &Token<'a>{
                &self.tokens[self.current]
            }

            fn is_at_end(&self) -> bool{
               self.peek() == &Token::Eof
            }

            fn previous(&self) -> Token<'a>{
                return self.tokens[self.current-1].clone()
            }

            fn advance(&mut self) -> Token<'a>{
                if let Some(tok) = self.tokens.get(self.current){
                    if tok != &Token::Eof{
                        self.current += 1;
                        return tok.clone()
                    }
                }
                self.previous()
            }

            fn token_match<const N:usize>(&mut self, tokens: [Token<'a>; N]) -> bool{
                for token in &tokens{
                    if let Some(tok) = self.tokens.get(self.current){
                        let matcher = |a:&Token,b:&Token| match a {
                            Token::String(_) => {
                                if let Token::String(_) = b {
                                    return true
                                }
                                false
                            },
                            Token::Number(_) => {
                                if let Token::Number(_) = b{
                                    return true
                                }
                                false
                            },
                            Token::Identifier(_) => {
                                if let Token::Identifier(_) = b{
                                    return true
                                }
                                false
                            }
                            other => other == b
                        };
                        if matcher(token,tok) {
                            self.advance();
                            return true
                        }
                    }
                }
                false
            }

            fn expression(&mut self) -> Expr<'a>{
                self.equality()
            }

            fn equality(&mut self) -> Expr<'a>{
                let mut expr = self.comparison();
                while self.token_match([Token::EqualEqual, Token::BangEqual]){
                    let operator = self.previous();
                    let right = self.comparison();
                    expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
                }
                expr
            }

            fn comparison(&mut self) -> Expr<'a>{
                let mut expr = self.term();
                while self.token_match([Token::Greater, Token::GreaterEqual, Token::Less, Token::LessEqual]){
                    let operator = self.previous();
                    let right = self.term();
                    expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
                }
                expr
            }

            fn term(&mut self) -> Expr<'a>{
                let mut expr = self.factor();
                while self.token_match([Token::Minus, Token::Plus]){
                    let operator = self.previous();
                    let right = self.factor();
                    expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
                }
                expr
            }

            fn factor(&mut self) -> Expr<'a>{
                let mut expr = self.unary();
                while self.token_match([Token::Slash, Token::Star]){
                    let operator = self.previous();
                    let right = self.unary();
                    expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
                }
                expr
            }
            fn unary(&mut self) -> Expr<'a>{
                if self.token_match([Token::Bang, Token::Minus]){
                    let operator = self.previous();
                    let expr = self.unary();
                    return Expr::Unary(operator, Box::new(expr))
                }
                self.primary()
            }

            fn primary(&mut self) -> Expr<'a>{
                if self.token_match([Token::Number(0.0), Token::String(String::new()), Token::True, Token::False, Token::Nil]){
                    let literal = self.previous();
                    return Expr::Literal(literal)
                }
                if self.token_match([Token::LeftParen]){
                    let expr = self.expression();
                    if self.token_match([Token::RightParen]){
                        return Expr::Grouping(Box::new(expr))
                    }
                }
                panic!("Parsing went wrong!!! :(")
            }

        }
    }

}
pub mod lexer {
    use itertools::__std_iter::Peekable;
    use itertools::{Itertools, PeekingNext};
    use std::str::{Chars, FromStr};

    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
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
        //Identifier(&'a str),
        Identifier(String),
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
        Eof,
    }

    macro_rules! advance {
        ($s:expr, $val:expr, $yes:expr, $no:expr) => {{
            let mut temp = $no;
            if let Some(n) = $s.stream.as_mut().unwrap().peek() {
                if *n == $val {
                    temp = $yes;
                    let _ = $s.stream.as_mut().unwrap().next();
                    $s.current += 1;
                    $s.total_current += 1;
                }
            }
            temp
        }};
    }

    pub struct Scanner<'a> {
        src: &'a str,
        stream: Option<Peekable<Chars<'a>>>,
        total_current: usize,
        current: usize,
        line: usize,
    }

    impl<'a> Scanner<'a> {
        pub fn from(source: &'a str) -> Self {
            Self {
                src: source,
                stream: None,
                total_current: 0,
                current: 0,
                line: 1,
            }
        }

        fn scan_token(&mut self) -> Result<Token, String> {
            //let mut stream = self.src.get(self.current..).unwrap().chars();
            let tok;
            // char_match!(self, '=', Token::BangEqual, Token::Bang)
            if let Some(c) = self.stream.as_mut().unwrap().next() {
                self.current += 1;
                self.total_current += 1;
                match c {
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
                    '/' => {
                        tok = if let Some(n) = self.stream.as_mut().unwrap().peek() {
                            let mut tok = Token::Eof;
                            if *n == '/' {
                                while let Some(n) = self.stream.as_mut().unwrap().next() {
                                    self.current += 1;
                                    self.total_current += 1;
                                    if n == '\n' {
                                        tok = self.scan_token()?;
                                        break;
                                    }
                                }
                            } else {
                                tok = Token::Slash
                            }
                            tok
                        } else {
                            Token::Slash
                        }
                    }
                    ' ' | '\r' | '\t' | '\n' => {
                        let mut line_inc = 0;
                        let mut current_inc = 0;
                        while let Some(_) =
                            self.stream.as_mut().unwrap().peeking_next(|n| match n {
                                ' ' | '\r' | '\t' => {
                                    current_inc += 1;
                                    true
                                }
                                '\n' => {
                                    line_inc += 1;
                                    true
                                }
                                _ => false,
                            })
                        {} // Returns None if not whitespace
                        self.line += line_inc;
                        if line_inc != 0 {
                            // resets character count after each newline.
                            self.current = 0;
                        } else {
                            self.current += current_inc;
                            self.total_current += current_inc;
                        }
                        tok = self.scan_token()?;
                    }
                    '"' => {

                        let mut string = String::new();
                        while let Some(c) = self.stream.as_mut().unwrap().peek() {
                            if *c != '"' {
                                string.push(self.stream.as_mut().unwrap().next().unwrap());
                            } else {
                                break;
                            }
                        }
                        match self.stream.as_mut().unwrap().next(){
                            Some('"') => tok = Token::String(string),
                            _ => return Err(format!("Non-terminating string at line {}", self.line))
                        }
                    }
                    '0'..='9' => {
                        let mut len = 0;
                        while let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if n.is_digit(10) {
                                let _ = self.stream.as_mut().unwrap().next();
                                len += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if *n == '.' {
                                let _ = self.stream.as_mut().unwrap().next();
                                len += 1;
                                if let Some(n) = self.stream.as_mut().unwrap().peek() {
                                    if !n.is_digit(10) {
                                        return Err(format!(
                                            "Invalid Number at {}:{}",
                                            self.line, self.current
                                        ));
                                    }
                                }
                                while let Some(n) = self.stream.as_mut().unwrap().peek() {
                                    if n.is_digit(10) {
                                        let _ = self.stream.as_mut().unwrap().next();
                                        len += 1;
                                    } else {
                                        if *n == '.' {
                                            return Err(format!(
                                                "Invalid '.' at {}:{}",
                                                self.line, self.current
                                            ));
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        //tok = Token::Number(f64::from_str(buf.as_str()).unwrap());
                        let stringy = self
                            .src
                            .get(self.total_current - 1..self.total_current + len)
                            .unwrap();
                        self.total_current += len;
                        self.current += len;
                        tok = Token::Number(f64::from_str(stringy).unwrap());
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        //Identifier
                        let mut len = 0;
                        while let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if n.is_alphabetic() {
                                let _ = self.stream.as_mut().unwrap().next();
                                len += 1;
                            } else {
                                break;
                            }
                        }

                        let stringy = self
                            .src
                            .get(self.total_current - 1..self.total_current + len)
                            .unwrap();

                        if let Some(n) = match stringy {
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
                            _ => None,
                        } {
                            tok = n.to_owned();
                        } else {
                            tok = Token::Identifier(stringy.to_string());
                        }

                        self.total_current += len;
                        self.current += len;
                    }
                    _ => {
                        return Err(format!(
                            "Unexpected character at {}:{}",
                            self.line, self.current
                        ))
                    }
                };
            } else {
                tok = Token::Eof;
            }

            Ok(tok)
        }

        pub fn scan_tokens(&mut self) -> Result<Vec<Token>, String> {
            self.stream = Some(self.src.chars().peekable());
            let mut tokens = Vec::new();
            loop {
                let tok = self.scan_token()?;
                if tok != Token::Eof {
                    tokens.push(tok);
                } else {
                    break;
                }
            }
            return Ok(tokens);
        }
    }
}
mod visit {
    use super::parser::Expr;
    use super::lexer::Token;

    pub trait Visitor<T>{
        fn accept(&mut self, expr: &Expr) -> T;
        fn visit_literal_expr(&mut self, expr: &Expr) -> T;
        fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
        fn visit_unary_expr(&mut self, expr: &Expr) -> T;
        fn visit_binary_expr(&mut self, expr: &Expr) -> T;
    }
}

pub mod parser {
    use super::lexer::Token;
    use anyhow::anyhow;
    use anyhow::{Context, Error, Result};
    use itertools::Itertools;
    use itertools::__std_iter::Peekable;
    use std::fmt::{Display, Formatter};
    use std::slice::Iter;
    use std::str::{Chars, FromStr};

    #[derive(Debug, Clone)]
    pub enum Expr {
        Unary(Token, Box<Expr>),
        Binary(Token, Box<Expr>, Box<Expr>),
        Literal(Token),
        Grouping(Box<Expr>),
    }

    impl Expr{
        /// Gets the right node. If Expr does not have right node, panic!
        pub fn right(&self) -> &Expr{
            match self{
                Expr::Binary(_, _, r) => r,
                _ => panic!("Tried to get a right node from {}!", self)
            }
        }
        /// Gets left node. If Expr does not have left node, panic!
        pub fn left(&self) -> &Expr{
            match self{
                Expr::Binary(_, l,_) => l,
                _ => panic!("Tried to get a left node from {}!", self)
            }
        }
        /// Gets expression from node. If Expr does not have an expression, panic!
        pub fn expression(&self) -> &Expr{
            match self{
                Expr::Unary(_, expr) => expr,
                Expr::Grouping(expr) => expr,
                _ => panic!("Tried to get an expression node from {}!", self)
            }
        }
    }

    impl Display for Expr {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Literal(_) => write!(f, "Literal"),
                Self::Grouping(_) => write!(f, "Grouping"),
                Self::Unary(_, _) => write!(f, "Unary"),
                Self::Binary(_, _, _) => write!(f, "Binary"),
            }
        }
    }

    pub struct Parser {
        tokens: Vec<Token>,
        current: usize,
    }

    impl Parser {
        pub fn from(tokens: Vec<Token>) -> Self {
            Self { tokens, current: 0 }
        }

        pub fn parse(&mut self) -> Result<Expr> {
            self.expression()
        }

        fn peek(&self) -> &Token {
            &self.tokens[self.current]
        }

        fn is_at_end(&self) -> bool {
            self.peek() == &Token::Eof
        }

        fn previous(&self) -> Token {
            return self.tokens[self.current - 1].clone();
        }

        fn advance(&mut self) -> Token {
            if let Some(tok) = self.tokens.get(self.current) {
                if tok != &Token::Eof {
                    self.current += 1;
                    return tok.clone();
                }
            }
            self.previous()
        }

        fn token_match<const N: usize>(&mut self, tokens: [Token; N]) -> bool {
            for token in &tokens {
                if let Some(tok) = self.tokens.get(self.current) {
                    let matcher = |a: &Token, b: &Token| match a {
                        Token::String(_) => {
                            if let Token::String(_) = b {
                                return true;
                            }
                            false
                        }
                        Token::Number(_) => {
                            if let Token::Number(_) = b {
                                return true;
                            }
                            false
                        }
                        Token::Identifier(_) => {
                            if let Token::Identifier(_) = b {
                                return true;
                            }
                            false
                        }
                        other => other == b,
                    };
                    if matcher(token, tok) {
                        self.advance();
                        return true;
                    }
                }
            }
            false
        }

        fn expression(&mut self) -> Result<Expr> {
            self.equality()
        }

        fn equality(&mut self) -> Result<Expr> {
            let mut expr = self.comparison();
            while self.token_match([Token::EqualEqual, Token::BangEqual]) {
                let operator = self.previous();
                let right = self.comparison();
                expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)));
            }
            expr
        }

        fn comparison(&mut self) -> Result<Expr> {
            let mut expr = self.term();
            while self.token_match([
                Token::Greater,
                Token::GreaterEqual,
                Token::Less,
                Token::LessEqual,
            ]) {
                let operator = self.previous();
                let right = self.term();
                expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)));
            }
            expr
        }

        fn term(&mut self) -> Result<Expr> {
            let mut expr = self.factor();
            while self.token_match([Token::Minus, Token::Plus]) {
                let operator = self.previous();
                let right = self.factor();
                expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)));
            }
            expr
        }

        fn factor(&mut self) -> Result<Expr> {
            let mut expr = self.unary();
            while self.token_match([Token::Slash, Token::Star]) {
                let operator = self.previous();
                let right = self.unary();
                expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)));
            }
            expr
        }
        fn unary(&mut self) -> Result<Expr> {
            if self.token_match([Token::Bang, Token::Minus]) {
                let operator = self.previous();
                let expr = self.unary();
                return Ok(Expr::Unary(operator, Box::new(expr?)));
            }
            self.primary()
        }

        fn primary(&mut self) -> Result<Expr> {
            if self.token_match([
                Token::Number(0.0),
                Token::String(String::new()),
                Token::True,
                Token::False,
                Token::Nil,
            ]) {
                let literal = self.previous();
                return Ok(Expr::Literal(literal));
            }
            if self.token_match([Token::LeftParen]) {
                let expr = self.expression();
                if self.token_match([Token::RightParen]) {
                    return Ok(Expr::Grouping(Box::new(expr?)));
                }
            }
            Err(anyhow!("Syntax error at token {}", self.current + 1))
        }
    }
}

pub mod interpreter {
    use super::lexer::Token;
    use super::parser::Expr;
    use anyhow::{anyhow, Context, Error, Result};
    use crate::lox_lib::visit::Visitor;

    pub struct Interpreter {
        tree: Expr
    }
    
    impl super::visit::Visitor<Result<Token>> for Interpreter{
        fn accept(&mut self, expr: &Expr) -> Result<Token>{
            match expr{
                Expr::Binary(_,..) => self.visit_binary_expr(expr),
                Expr::Unary(_,..) => self.visit_unary_expr(expr),
                Expr::Grouping(_) => self.visit_grouping_expr(expr),
                Expr::Literal(_) => self.visit_literal_expr(expr)
            }
        }

        fn visit_literal_expr(&mut self, expr: &Expr) -> Result<Token> {
            if let Expr::Literal(tok) = expr {
                return Ok(tok.clone());
            }
            Err(anyhow!("RUNTIME ERROR: Expected Literal, got {}", expr))
        }

        fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Token> {
            if let Expr::Grouping(grouping_expr) = expr {
                return self.accept(expr.expression())
            }
            Err(anyhow!("RUNTIME ERROR: Expected Grouping, got {}", expr))
        }

        fn visit_unary_expr(&mut self, expr: &Expr) -> Result<Token> {
            let right = self.accept(expr.expression())?;
            return match &expr {
                Expr::Unary(tok, e) => {
                    return match *tok{
                        Token::Minus => {
                            return if let Token::Number(num) = right {
                                Ok(Token::Number(-num))
                            } else {
                                Err(anyhow!("Expected Number, got {:?}", right))
                            }
                        },
                        Token::Bang => {
                            return if Self::is_truthy(tok){
                                Ok(Token::False)
                            } else {
                                Ok(Token::True)
                            }
                        },
                        _ => Err(anyhow!("Expected Unary, got {}", expr))
                    }
                },
                _ => Err(anyhow!("Expected Unary, got {}", expr)),
            };
        }

        fn visit_binary_expr(&mut self, expr: &Expr) -> Result<Token> {
            let left = self.accept(expr.left())?;
            let right = self.accept(expr.right())?;

            return if let Expr::Binary(op, _, _) = &expr{
                match op{
                    Token::Minus => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(Token::Number(*l - *r)),
                            (l, r) => Err(anyhow!("Expected Number - Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::Slash => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(Token::Number(*l / *r)),
                            (l, r) => Err(anyhow!("Expected Number / Number, got {:?} * {:?}", l, r))
                        }

                    },
                    Token::Star => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(Token::Number(*l * *r)),
                            (l, r) => Err(anyhow!("Expected Number * Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::Plus => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(Token::Number(*l + *r)),
                            (Token::Number(l), Token::String(r)) => Ok(Token::String(format!("{}{}", l, r))),
                            (Token::String(l), Token::Number(r)) => Ok(Token::String(format!("{}{}", l, r))),
                            (Token::String(l), Token::String(r)) => Ok(Token::String(format!("{}{}", l, r))),
                            (l, r) => Err(anyhow!("Expected (Number | String) + (Number | String), got {:?} + {:?}", l, r))
                        }
                    },
                    Token::Greater => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(if *l > *r {Token::True} else {Token::False}),
                            (l, r) => Err(anyhow!("Expected Number > Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::GreaterEqual => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(if *l >= *r {Token::True} else {Token::False}),
                            (l, r) => Err(anyhow!("Expected Number >= Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::Less => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(if *l < *r {Token::True} else {Token::False}),
                            (l, r) => Err(anyhow!("Expected Number < Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::LessEqual => {
                        match (&left, &right) {
                            (Token::Number(l), Token::Number(r)) => Ok(if *l <= *r {Token::True} else {Token::False}),
                            (l, r) => Err(anyhow!("Expected Number <= Number, got {:?} * {:?}", l, r))
                        }
                    },
                    Token::EqualEqual => Ok(if Self::is_equal(&left, &right) {Token::True} else {Token::False}),
                    Token::BangEqual => Ok(if !Self::is_equal(&left, &right) {Token::True} else {Token::False}),
                    _ => Err(anyhow!("Expected an operator, got {:?}", op))
                }
            } else {
                Err(anyhow!("Expected Binary, got {}", expr))
            }
        }
    }

    impl Interpreter {
        pub fn execute(&mut self) -> Result<Token>{
            let tree = std::mem::replace(&mut self.tree, Expr::Literal(Token::Nil));
            self.accept(&tree)
        }

        pub fn from(tree: Expr) -> Self {
            Self { tree }
        }

        /// Nil & False are Falsy, Everything else is truthy
        fn is_truthy(token: &Token) -> bool{
            match token{
                Token::Nil => false,
                Token::False => false,
                _ => true,
            }
        }

        fn is_equal(l: &Token, r: &Token) -> bool{
            match (l,r){
                (Token::Nil, Token::Nil) => true,
                (Token::Nil, _) | (_, Token::Nil) => false,
                (l, r) => l == r
            }
        }
    }
}

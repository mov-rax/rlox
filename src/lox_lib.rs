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
                            }){}
                         // Returns None if not whitespace
                        self.line += line_inc;
                        if line_inc != 0 {
                            // resets character count after each newline.
                            self.current = 0;
                        } else {
                            self.current += current_inc;
                        }
                        self.total_current += current_inc;
                        tok = self.scan_token()?;
                    }
                    '"' => { // Strings
                        let mut string = String::new();
                        while let Some(c) = self.stream.as_mut().unwrap().peek() {
                            if *c != '"' {
                                string.push(self.stream.as_mut().unwrap().next().unwrap());
                            } else {
                                break;
                            }
                        }

                        match self.stream.as_mut().unwrap().next(){
                            Some('"') => {
                                let len = string.len(); // Adding 2 for opening and closing quotations
                                self.current += 1 + len;
                                self.total_current += 1 + len;
                                tok = Token::String(string);
                            },
                            _ => return Err(format!("Non-terminating string at line {}", self.line))
                        }

                    }
                    '0'..='9' => {
                        let mut buf = String::from(c);
                        let mut len = 0;
                        while let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if n.is_digit(10) {
                                buf.push(self.stream.as_mut().unwrap().next().unwrap());
                                len += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if *n == '.' {
                                buf.push(self.stream.as_mut().unwrap().next().unwrap());
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
                                        buf.push(self.stream.as_mut().unwrap().next().unwrap());
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

                        self.total_current += buf.len();
                        self.current += buf.len();
                        tok = Token::Number(f64::from_str(buf.as_str()).unwrap());
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        //Identifier
                        let mut buf = String::from(c);
                        while let Some(n) = self.stream.as_mut().unwrap().peek() {
                            if n.is_alphanumeric() {
                                buf.push(self.stream.as_mut().unwrap().next().unwrap());
                            } else {
                                break;
                            }
                        }

                        let len = buf.len();

                        if let Some(n) = match buf.as_str() {
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
                            tok = Token::Identifier(buf);
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
            match tokens.last() {
                Some(Token::Eof) => {},
                _ => tokens.push(Token::Eof)
            }
            return Ok(tokens);
        }
    }
}
mod visit {
    use super::parser::Expr;
    use super::lexer::Token;
    use super::parser::Stmt;

    pub trait Visitor<T>{
        fn accept(&mut self, expr: &Expr) -> T;
        fn visit_literal_expr(&mut self, expr: &Expr) -> T;
        fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
        fn visit_unary_expr(&mut self, expr: &Expr) -> T;
        fn visit_binary_expr(&mut self, expr: &Expr) -> T;
        fn visit_expression_stmt(&mut self, stmt: &Stmt) -> T;
        fn visit_print_stmt(&mut self, stmt: &Stmt) -> T;
        fn visit_var_stmt(&mut self, stmt: &Stmt) -> T;
        fn visit_var_expr(&mut self, name:&Expr) -> T;
        fn visit_assign_expr(&mut self, expr:&Expr) -> T;
        fn visit_block_stmt(&mut self, stmt:&Stmt) -> T;
        fn visit_if_stmt(&mut self, stmt:&Stmt) -> T;
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
    use crate::lox_lib::lexer::Token::SemiColon;



    #[derive(Debug, Clone)]
    pub enum Stmt {
        Block(Vec<Stmt>),
        ExprStmt(Expr),
        IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>), // condition, then_branch, else_branch
        PrintStmt(Expr),
        VarStmt(Token, Option<Expr>) // Optional Expr when declaring a Variable
    }

    impl Stmt{
        pub fn expression(&self) -> &Expr{
            match self{
                Stmt::ExprStmt(expr) => expr,
                Stmt::PrintStmt(expr) => expr,
                Stmt::VarStmt(_, expr) => expr.as_ref().unwrap(),
                Stmt::IfStmt(expr, _, _) => expr,
                a => panic!("Tried to get an expression from a {:?}", a)
            }
        }

        pub fn unwrap_var(&self) -> (&Token, Option<&Expr>){
            match self{
                Stmt::VarStmt(tok, expr) => (tok, expr.as_ref()),
                other => panic!("Tried to unwrap a {:?} as a VarStmt!", other)
            }
        }

        pub fn unwrap_block(&self) -> &Vec<Stmt>{
            match self{
                Stmt::Block(stmts) => stmts,
                other => panic!("Tried to unwrap a {:?} as a Block!", other)
            }
        }
        /// Returns (condition, true_branch, Option<else_branch>)
        pub fn unwrap_if(&self) -> (&Expr, &Box<Stmt>, Option<&Box<Stmt>>){
            match self{
                Stmt::IfStmt(cond, true_block, else_block) => (cond, true_block, else_block.as_ref()),
                other => panic!("Tried to unwrap a {:?} as an If!", other)
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
        Unary(Token, Box<Expr>),
        Assign(Token, Box<Expr>),
        Binary(Token, Box<Expr>, Box<Expr>),
        Literal(Token),
        Grouping(Box<Expr>),
        Variable(Token)
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
                Self::Variable(name) => match name {
                    Token::Identifier(name) => write!(f, "Variable({})", name),
                    _ => write!(f, "Variable(UNKNOWN)")
                    },
                Self::Assign(name, value) => write!(f, "Assignment of '{:?}' to {:?}", name, value)
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

        pub fn parse_all(&mut self) -> Result<Vec<Stmt>>{
            let mut statements = Vec::new();
            while !self.is_at_end() {
                statements.push(self.declaration()?);
            }
            Ok(statements)
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

        fn safe_advance(&mut self) -> Result<Token>{
            return match self.tokens.get(self.current) {
                Some(tok) => {
                    self.current += 1;
                    Ok(tok.clone())
                },
                None => Err(anyhow!("Parsing Out-Of-Bounds Detected!"))
            }
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

        fn var_declaration(&mut self) -> Result<Stmt>{
            let name = self.safe_advance()?;
            let init;
            if self.token_match([Token::Equal]){
                init = Some(self.expression()?);
            } else {
                init = None;
            }
            let semi = self.safe_advance()?;
            match semi{
                Token::SemiColon => Ok(Stmt::VarStmt(name, init)),
                other => Err(anyhow!("Expected a ';', got {:?}", other))
            }

        }

        fn declaration(&mut self) -> Result<Stmt>{
            if self.token_match([Token::Var]){
                return self.var_declaration()
            }
            self.statement()
        }

        fn statement(&mut self) -> Result<Stmt>{
            if self.token_match([Token::Print]){
                return self.print_statement()
            }
            if self.token_match([Token::LeftBrace]){
                return Ok(Stmt::Block(self.block()?))
            }
            if self.token_match([Token::If]){
                return Ok(self.if_statement()?)
            }
            self.expression_statement()
        }

        fn if_statement(&mut self) -> Result<Stmt>{
            match self.safe_advance(){
                Ok(Token::LeftParen) => {},
                _ => return Err(anyhow!("Expected '(' after 'if'.")),
            }

            let condition = self.expression()?;
            match self.safe_advance(){
                Ok(Token::RightParen) => {},
                _ => return Err(anyhow!("Expected ')' after if condition.")),
            }

            let then_branch = self.statement()?;
            let else_branch;
            if self.token_match([Token::Else]){
                else_branch = Some(Box::from(self.statement()?));
            } else {
                else_branch = None;
            }
            Ok(Stmt::IfStmt(condition, Box::from(then_branch), else_branch))
        }

        fn print_statement(&mut self) -> Result<Stmt>{
            let value = self.expression()?;
            match self.safe_advance()?{
                Token::SemiColon => Ok(Stmt::PrintStmt(value)),
                other => Err(anyhow!("Expected ';', got {:?}", other))
            }
        }

        fn expression_statement(&mut self) -> Result<Stmt>{
            let expr = self.expression()?;
            match self.safe_advance()?{
                Token::SemiColon => Ok(Stmt::ExprStmt(expr)),
                other => Err(anyhow!("Expected ';', got {:?}", other))
            }
        }

        fn block(&mut self) -> Result<Vec<Stmt>>{
            let mut statements = Vec::new();
            while !self.is_at_end() && self.tokens[self.current] != Token::RightBrace{
                statements.push(self.declaration()?);
            }
            match self.safe_advance() {
                Ok(Token::RightBrace) => Ok(statements),
                _ => Err(anyhow!("Expected '}' after block."))
            }

        }

        fn expression(&mut self) -> Result<Expr> {
            self.assignment()
        }

        fn assignment(&mut self) -> Result<Expr>{
            let expr = self.equality()?;
            if self.token_match([Token::Equal]) {
                //let equals = self.previous();
                let value = self.assignment()?;
                return match &expr{
                    Expr::Variable(ident) =>  Ok(Expr::Assign(ident.clone(), Box::new(value))),
                    other => Err(anyhow!("Invalid assignment Target {:?}", other))
                }
            }
            Ok(expr)
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
            if self.token_match([Token::Identifier(String::new())]){
                return Ok(Expr::Variable(self.previous()))
            }

            if self.token_match([Token::LeftParen]) {
                let expr = self.expression();
                if self.token_match([Token::RightParen]) {
                    return Ok(Expr::Grouping(Box::new(expr?)));
                }
            }
            Err(anyhow!("Syntax error at token {}, [{:?}]", self.current + 1, &self.tokens[self.current]))
        }
    }
}

pub mod interpreter {
    use super::lexer::Token;
    use super::parser::Expr;
    use super::parser::Stmt;
    use anyhow::{anyhow, Context, Error, Result};
    use crate::lox_lib::visit::Visitor;
    use super::environment::Environment;
    use std::any::Any;
    use std::collections::HashMap;

    pub struct Interpreter {
        environment: Box<Environment>
    }
    
    impl super::visit::Visitor<Result<Token>> for Interpreter{
        fn accept(&mut self, expr: &Expr) -> Result<Token>{
            match expr{
                Expr::Binary(_,..) => self.visit_binary_expr(expr),
                Expr::Unary(_,..) => self.visit_unary_expr(expr),
                Expr::Grouping(_) => self.visit_grouping_expr(expr),
                Expr::Literal(_) => self.visit_literal_expr(expr),
                Expr::Variable(_) => self.visit_var_expr(expr),
                Expr::Assign(_,..) => self.visit_assign_expr(expr),
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

        fn visit_expression_stmt(&mut self, stmt: &Stmt) -> Result<Token> {
            self.accept(stmt.expression())
        }

        fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<Token> {
            let value = self.accept(stmt.expression())?;
            match value{
                Token::Number(num) => println!("{}", num),
                Token::String(string) => println!("{}", string),
                other => println!("{:?}", other)
            };
            Ok(Token::Nil)
        }

        fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<Token> {
            let statement = stmt.unwrap_var();
            let value = match statement.1 {
                Some(expr) => self.accept(expr)?,
                None => Token::Nil,
            };
            let name = match statement.0{
                Token::Identifier(name) => name.clone(),
                _ => return Err(anyhow!("No identifier found for Variable."))
            };
            let _ = self.environment.define(name, value);
            Ok(Token::Nil)
        }

        fn visit_var_expr(&mut self, name:&Expr) -> Result<Token>{
            match name{
                Expr::Variable(Token::Identifier(name)) => {
                    let res = self.environment.get(name);
                    match res {
                        Some(data) => Ok(data.clone()),
                        None => Err(anyhow!("Undefined Identifier '{}'.", name))
                    }
                }
                _ => Err(anyhow!("Invalid Variable Identifier."))
            }
        }

        fn visit_assign_expr(&mut self, expr: &Expr) -> Result<Token> {
            let value = match expr {
                Expr::Assign(ident, value) => {
                    let ident = match ident {
                        Token::Identifier(name) => name,
                        _ => panic!("Panic at visit_assign_expr. I'm not sure how this happened.")
                    };
                    (ident, self.accept(value)?)
                },
                other => return Err(anyhow!("Expected Assignment, got {:?}", other))
            };
            self.environment.assign(value.0, value.1)
        }

        fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<Token> {
            let statements = stmt.unwrap_block();
            let previous_env = std::mem::replace(&mut self.environment, Box::from(Environment::new())); // Replaces old environment with a new one
            self.environment.set_enclosing(previous_env); // Inserts old environment into old one.
            self.execute_all(statements);
            let previous_env = self.environment.get_enclosing(); // Get back old value (wrapped in an Option)
            let previous_env = std::mem::replace(previous_env, None).unwrap(); // Gets back ownership of old value.
            self.environment = previous_env;
            Ok(Token::Nil)
        }

        fn visit_if_stmt(&mut self, stmt: &Stmt) -> Result<Token> {
            let (condition, true_branch, else_branch) = stmt.unwrap_if();
            if Self::is_truthy(&self.execute(condition)?){
                return self.execute_stmt(true_branch)
            } else if let Some(else_branch) = else_branch{
                return self.execute_stmt(else_branch)
            }
            Ok(Token::Nil)
        }
    }

    impl Interpreter {
        pub fn new() -> Self{
            Self {
                environment: Box::from(Environment::new())
            }
        }

        pub fn execute(&mut self, expr: &Expr) -> Result<Token>{
            self.accept(&expr)
        }

        pub fn execute_stmt(&mut self, stmt: &Stmt) -> Result<Token>{
            match stmt{
                Stmt::PrintStmt(_) => self.visit_print_stmt(stmt),
                Stmt::ExprStmt(_) => self.visit_expression_stmt(stmt),
                Stmt::VarStmt(_, _) => self.visit_var_stmt(stmt),
                Stmt::Block(_) => self.visit_block_stmt(stmt),
                Stmt::IfStmt(_,..) => self.visit_if_stmt(stmt),
            }
        }

        pub fn execute_all(&mut self, statements: &Vec<Stmt>) -> Result<Token>{
            for i in 0..statements.len()-1{
                let stmt = &statements[i];
                let _ = self.execute_stmt(stmt);
            }
            let last = statements.last().unwrap();
            self.execute_stmt(last)
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

pub mod environment {
    use std::collections::HashMap;
    use super::parser::Expr;
    use crate::lox_lib::lexer::Token;
    use anyhow::{Result, Error, Context, anyhow};

    pub struct Environment{
        values:HashMap<String, Token>,
        enclosing: Option<Box<Environment>>
    }

    impl Environment{
        pub fn new() -> Self{
            Self {
                values: HashMap::new(),
                enclosing: None,
            }
        }

        pub fn define(&mut self, name:String, value:Token){
            let _ = self.values.insert(name, value);
        }

        pub fn get(&mut self, name:&String) -> Option<&Token> {
            match self.values.get(name){
                Some(tok) => Some(tok),
                None => match &mut self.enclosing{
                    Some(env) => env.get(name),
                    None => None
                }
            }
        }

        pub fn assign(&mut self, name:&String, value:Token) -> Result<Token>{
            match self.values.get_mut(name){
                Some(tok) => {
                    *tok = value;
                    Ok(tok.clone())
                },
                None => match &mut self.enclosing{
                    Some(env) => env.assign(name, value),
                    None => Err(anyhow!("Undefined Variable {}", name))
                }
            }
        }

        pub fn set_enclosing(&mut self, env:Box<Environment>){
            self.enclosing = Some(env);
        }

        pub fn get_enclosing(&mut self) -> &mut Option<Box<Environment>> {
            &mut self.enclosing
        }

    }

}

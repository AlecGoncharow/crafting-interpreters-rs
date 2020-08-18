use crate::ast::AstPrinter;
use crate::ast::VisitorError;
use crate::interpreter::Interpreter;
use crate::parser::{ParseError, Parser};
use crate::token::{Token, TokenLiteral, TokenType};
use std::fs;
use std::io;
use std::io::Write;
use std::io::{Error, ErrorKind};

pub struct Lox {
    has_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { has_error: false }
    }

    pub fn error(&mut self, line: usize, msg: &str) {
        self.report(line, "", msg);
    }

    pub fn report(&mut self, line: usize, whr: &str, msg: &str) {
        eprintln!("[line {}] Error {}: {}", line, whr, msg);
        self.has_error = true;
    }
}

fn simple_write(s: &str) -> io::Result<()> {
    let stdout = io::stdout();
    let lock = stdout.lock();
    let mut w = io::BufWriter::new(lock);
    write!(&mut w, "{}", s)?;
    w.flush()?;
    Ok(())
}

pub fn run_file(path: &str) -> io::Result<()> {
    let mut interpreter = Interpreter::new();
    let bytes = fs::read(path)?;
    run(&mut interpreter, &String::from_utf8(bytes).unwrap())?;
    Ok(())
}

/// REPL
pub fn run_prompt() -> io::Result<()> {
    let mut interpreter = Interpreter::new();
    loop {
        simple_write("> ")?;

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => run(&mut interpreter, &input)?,
            Err(e) => eprintln!("{}", e),
        }
    }
}

fn run(interpreter: &mut Interpreter, source: &str) -> io::Result<()> {
    let lox = Lox::new();
    let mut scanner = Scanner::new(lox, source);
    let tokens = scanner.scan_tokens();

    let mut parser = Parser::new(tokens);
    let stmts = match parser.parse() {
        Ok(expr) => expr,
        Err(ParseError::Mismatch(token, msg) | ParseError::TooManyArgs(token, msg)) => {
            scanner.lox.error(token.line, &msg);

            return Err(Error::new(ErrorKind::Other, msg));
        }
    };

    for stmt in &stmts {
        let printer = AstPrinter::new();
        let error = printer.print(stmt);
        if let Err(VisitorError::RuntimeError(token, msg)) = error {
            scanner.lox.error(token.line, &msg);
        }
    }
    let error = interpreter.interpret(&stmts);

    if let Err(VisitorError::RuntimeError(token, msg)) = error {
        scanner.lox.error(token.line, &msg);
        return Err(Error::new(ErrorKind::Other, msg));
    }

    println!("{:?}", interpreter.output());

    Ok(())
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    lox: Lox,

    // tracking cursor
    start: usize,
    current: usize,
    line: usize,
}

fn is_decimal(c: char) -> bool {
    matches!(c, '0'..='9')
}

fn is_alpha(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_alpha_numeric(c: char) -> bool {
    is_decimal(c) || is_alpha(c)
}

fn match_keyword(s: &str) -> Option<TokenType> {
    match s {
        "and" => Some(TokenType::AND),
        "break" => Some(TokenType::BREAK),
        "continue" => Some(TokenType::CONTINUE),
        "class" => Some(TokenType::CLASS),
        "else" => Some(TokenType::ELSE),
        "false" => Some(TokenType::FALSE),
        "for" => Some(TokenType::FOR),
        "fun" => Some(TokenType::FUN),
        "if" => Some(TokenType::IF),
        "nil" => Some(TokenType::NIL),
        "or" => Some(TokenType::OR),
        "print" => Some(TokenType::PRINT),
        "return" => Some(TokenType::RETURN),
        "super" => Some(TokenType::SUPER),
        "this" => Some(TokenType::THIS),
        "true" => Some(TokenType::TRUE),
        "var" => Some(TokenType::VAR),
        "while" => Some(TokenType::WHILE),
        _ => None,
    }
}

impl Scanner {
    pub fn new(lox: Lox, source: &str) -> Self {
        Self {
            source: source.into(),
            tokens: Vec::new(),
            lox,

            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(
            TokenType::EOF,
            "\0",
            TokenLiteral::None,
            self.line,
        ));

        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LEFT_PAREN),
            ')' => self.add_token(TokenType::RIGHT_PAREN),
            '{' => self.add_token(TokenType::LEFT_BRACE),
            '}' => self.add_token(TokenType::RIGHT_BRACE),
            ',' => self.add_token(TokenType::COMMA),
            '.' => self.add_token(TokenType::DOT),
            '-' => self.add_token(TokenType::MINUS),
            '+' => self.add_token(TokenType::PLUS),
            ';' => self.add_token(TokenType::SEMICOLON),
            '*' => self.add_token(TokenType::STAR),

            '!' => {
                let token = if self.match_char('=') {
                    TokenType::BANG_EQUAL
                } else {
                    TokenType::BANG
                };
                self.add_token(token);
            }
            '=' => {
                let token = if self.match_char('=') {
                    TokenType::EQUAL_EQUAL
                } else {
                    TokenType::EQUAL
                };
                self.add_token(token)
            }
            '<' => {
                let token = if self.match_char('=') {
                    TokenType::LESS_EQUAL
                } else {
                    TokenType::LESS
                };
                self.add_token(token)
            }
            '>' => {
                let token = if self.match_char('=') {
                    TokenType::GREATER_EQUAL
                } else {
                    TokenType::GREATER
                };
                self.add_token(token)
            }

            '/' => {
                // check for double slash (comment)
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    // multiline comment
                    while !((self.peek() == '*' && self.peek_2() == '/') || self.is_at_end()) {
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }
                    // consume */
                    self.advance();
                    self.advance();
                } else {
                    self.add_token(TokenType::SLASH)
                }
            }

            // ignore whitespace
            ' ' | '\r' | '\t' => (),

            // newline
            '\n' => self.line += 1,

            // string
            '"' => self.string(),

            // number
            '0'..='9' => self.number(),

            // identifier
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

            _ => self.lox.error(self.line, "Unexpected Character"),
        };
    }

    // ===== Helpers =====

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_value(token_type, TokenLiteral::None);
    }

    fn add_token_with_value(&mut self, token_type: TokenType, literal: TokenLiteral) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, text, literal, self.line));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source_char_at(self.current) != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    // lookahead helper
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_char_at(self.current)
        }
    }

    fn peek_2(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source_char_at(self.current + 1)
        }
    }

    // consume and return char and point current to next char
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source_char_at(self.current - 1)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn source_char_at(&self, i: usize) -> char {
        self.source.as_bytes()[i] as char
    }

    // ===== literals =====
    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.lox.error(self.line, "Unterminated string.");
            return;
        }

        // closing '"'
        self.advance();

        let value = self.source[self.start + 1..self.current - 1].trim();
        let take = String::from(value);
        self.add_token_with_value(TokenType::STRING, TokenLiteral::Str(take));
    }

    fn number(&mut self) {
        while is_decimal(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && is_decimal(self.peek_2()) {
            // consume '.'
            self.advance();

            while is_decimal(self.peek()) {
                self.advance();
            }
        }

        let value = &self.source[self.start..self.current]
            .trim()
            .parse::<f64>()
            .expect("this is not correct");
        self.add_token_with_value(TokenType::NUMBER, TokenLiteral::Number(*value));
    }

    fn identifier(&mut self) {
        while is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let value = self.source[self.start..self.current].trim();

        let token_type = match_keyword(value);

        let (token_type, literal) = if let Some(token_type) = token_type {
            (token_type, TokenLiteral::None)
        } else {
            (
                TokenType::IDENTIFIER,
                TokenLiteral::Identifier(String::from(value)),
            )
        };
        self.add_token_with_value(token_type, literal);
    }
}

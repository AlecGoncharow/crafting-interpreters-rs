use crate::token::{Token, TokenType};
use crate::Lox;
use std::fs;
use std::io;
use std::io::Write;

fn simple_write(s: &str) -> io::Result<()> {
    let stdout = io::stdout();
    let lock = stdout.lock();
    let mut w = io::BufWriter::new(lock);
    write!(&mut w, "{}", s)?;
    w.flush()?;
    Ok(())
}

pub fn run_file(lox: Lox, path: &str) -> io::Result<()> {
    let bytes = fs::read(path)?;
    run(lox, &String::from_utf8(bytes).unwrap())?;
    Ok(())
}

/// REPL
pub fn run_prompt(_lox: Lox) -> io::Result<()> {
    loop {
        simple_write("> ")?;

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                println!("{} bytes read", n);
                println!("{}", input);
            }
            Err(e) => eprintln!("{}", e),
        }
    }
}

fn run(lox: Lox, source: &str) -> io::Result<()> {
    let mut scanner = Scanner::new(lox, source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }

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
                } else {
                    self.add_token(TokenType::SLASH)
                }
            }

            // ignore whitespace
            ' ' | '\r' | '\t' => (),

            '\n' => self.line += 1,

            _ => self.lox.error(self.line, "Unexpected Character"),
        };
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.current];

        self.tokens.push(Token::new(token_type, text, self.line));
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

    // consume next char from source and return
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source_char_at(self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn source_char_at(&self, i: usize) -> char {
        self.source.as_bytes()[i] as char
    }
}

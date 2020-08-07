use crate::token::{Token, TokenLiteral, TokenType};
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

fn is_decimal(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn is_alpha(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_alpha_numeric(c: char) -> bool {
    is_decimal(c) || is_alpha(c)
}

fn match_keyword(s: &str) -> Option<TokenType> {
    match s {
        "and" => Some(TokenType::AND),
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
                    while !(self.peek() == '*' && self.peek_2() == '/') && !self.is_at_end() {
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

        let value = &self.source[self.start + 1..self.current - 1].trim();
        let take = value.clone().into();
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

        let value = &self.source[self.start..self.current].trim();

        let token_type = match_keyword(value);

        let (token_type, literal) = if let Some(token_type) = token_type {
            (token_type, TokenLiteral::None)
        } else {
            (
                TokenType::IDENTIFIER,
                TokenLiteral::Identifier(value.clone().into()),
            )
        };
        self.add_token_with_value(token_type, literal);
    }
}

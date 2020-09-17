use crate::compiler;
use compiler::token::{Token, TokenKind};

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,

    // tracking cursor
    start: usize,
    current: usize,
    line: usize,
    column: usize,
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

fn match_keyword(s: &str) -> Option<TokenKind> {
    match s {
        "and" => Some(TokenKind::AND),
        "break" => Some(TokenKind::BREAK),
        "continue" => Some(TokenKind::CONTINUE),
        "class" => Some(TokenKind::CLASS),
        "else" => Some(TokenKind::ELSE),
        "false" => Some(TokenKind::FALSE),
        "for" => Some(TokenKind::FOR),
        "fun" => Some(TokenKind::FUN),
        "if" => Some(TokenKind::IF),
        "nil" => Some(TokenKind::NIL),
        "or" => Some(TokenKind::OR),
        "print" => Some(TokenKind::PRINT),
        "return" => Some(TokenKind::RETURN),
        "super" => Some(TokenKind::SUPER),
        "this" => Some(TokenKind::THIS),
        "true" => Some(TokenKind::TRUE),
        "var" => Some(TokenKind::VAR),
        "while" => Some(TokenKind::WHILE),
        _ => None,
    }
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.into(),
            tokens: Vec::new(),

            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenKind::EOF, "\0", self.line, self.column));

        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenKind::LEFT_PAREN),
            ')' => self.add_token(TokenKind::RIGHT_PAREN),
            '{' => self.add_token(TokenKind::LEFT_BRACE),
            '}' => self.add_token(TokenKind::RIGHT_BRACE),
            ',' => self.add_token(TokenKind::COMMA),
            '.' => self.add_token(TokenKind::DOT),
            '-' => self.add_token(TokenKind::MINUS),
            '+' => self.add_token(TokenKind::PLUS),
            ';' => self.add_token(TokenKind::SEMICOLON),
            '*' => self.add_token(TokenKind::STAR),

            '!' => {
                let token = if self.match_char('=') {
                    TokenKind::BANG_EQUAL
                } else {
                    TokenKind::BANG
                };
                self.add_token(token);
            }
            '=' => {
                let token = if self.match_char('=') {
                    TokenKind::EQUAL_EQUAL
                } else {
                    TokenKind::EQUAL
                };
                self.add_token(token)
            }
            '<' => {
                let token = if self.match_char('=') {
                    TokenKind::LESS_EQUAL
                } else {
                    TokenKind::LESS
                };
                self.add_token(token)
            }
            '>' => {
                let token = if self.match_char('=') {
                    TokenKind::GREATER_EQUAL
                } else {
                    TokenKind::GREATER
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
                    self.add_token(TokenKind::SLASH)
                }
            }

            // ignore whitespace
            ' ' | '\r' | '\t' => (),

            // newline
            '\n' => {
                self.line += 1;
                self.column = 1;
            }

            // string
            '"' => self.string(),

            // number
            '0'..='9' => self.number(),

            // identifier
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

            _ => self.add_error("Unexpected Character"),
        };
    }

    // ===== Helpers =====

    fn add_token(&mut self, token_type: TokenKind) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, text, self.line, self.column));
    }

    fn add_error(&mut self, msg: &str) {
        self.tokens
            .push(Token::new(TokenKind::ERROR, msg, self.line, self.column));
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
        self.column += 1;
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
            self.add_error("Unterminated string.");
            return;
        }

        // closing '"'
        self.advance();

        self.add_token(TokenKind::STRING);
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

        self.add_token(TokenKind::NUMBER);
    }

    fn identifier(&mut self) {
        while is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let value = self.source[self.start..self.current].trim();

        let token_type = match_keyword(value);

        let token_type = if let Some(token_type) = token_type {
            token_type
        } else {
            TokenKind::IDENTIFIER
        };
        self.add_token(token_type);
    }
}

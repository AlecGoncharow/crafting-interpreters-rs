pub mod chunk;
pub mod debug;
pub mod scanner;
pub mod token;
pub mod value;

use crate::vm::InterpretError;
use chunk::{Chunk, OpCode};
use scanner::Scanner;
use token::{Token, TokenKind};

pub fn compile(source: &str, chunk: Chunk) -> bool {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    parser.consume(TokenKind::EOF, "Expect end of expression.");
    !parser.had_error
}

type ParseResult = Result<(), InterpretError>;

struct Parser {
    current: usize,
    tokens: Vec<Token>,
    had_error: bool,
    panic_mode: bool,
    current_chunk: Chunk,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            current: 0,
            tokens: tokens.into(),
            had_error: false,
            panic_mode: false,
            current_chunk: Chunk::init(),
        }
    }

    pub fn advance(&mut self) -> ParseResult {
        loop {
            let token = self.current_token();
            if token.kind != TokenKind::ERROR {
                return Ok(());
            }

            let clone = token.clone();
            self.error_at_current(&clone.lexeme);
            return Err(InterpretError::CompileError);
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current_token().clone(), msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous_token().clone(), msg);
    }

    fn error_at(&mut self, token: Token, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenKind::EOF => {
                eprint!(" at end");
            }

            TokenKind::ERROR => {}

            _ => {
                eprint!(" at '{}'", token.lexeme);
            }
        }

        eprintln!(": {}", msg);
        self.had_error = true;
    }

    fn consume(&mut self, kind: TokenKind, e_msg: &str) -> ParseResult {
        if self.current_token().kind == kind {
            return self.advance();
        }
        self.error_at_current(e_msg);
        return Err(InterpretError::CompileError);
    }

    fn emit_byte(&mut self, byte: u8) {
        let prev = self.previous_token();
        let loc = (prev.line, prev.column);
        self.current_chunk.write(byte, loc);
    }

    fn emit_byte_2(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn end(&mut self) {
        self.emit_return();
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8);
    }

    // helpers
    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

pub mod chunk;
pub mod debug;
pub mod scanner;
pub mod token;
pub mod value;

use crate::vm::InterpretError;
use chunk::{Chunk, OpCode};
use scanner::Scanner;
use token::{Token, TokenKind};
use value::Value;

pub fn compile(source: &str, chunk: Chunk) -> bool {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    parser.consume(TokenKind::EOF, "Expect end of expression.");
    !parser.had_error
}

pub enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1, // =
    OR = 2,         // or
    AND = 3,        // and
    EQUALITY = 4,   // == !=
    COMPARISON = 5, // < > <= >=
    TERM = 6,       // + -
    FACTOR = 7,     // * /
    UNARY = 8,      // ! -
    CALL = 9,       // . ()
    PRIMARY = 10,
}

type ParseResult = Result<(), InterpretError>;

enum RuleFn {
    Grouping,
    Unary,
    Binary,
    Number,
}

// (Left_Op, Right_Op, Level)
type Rule = (Option<RuleFn>, Option<RuleFn>, Precedence);

struct Parser {
    current: usize,
    tokens: Vec<Token>,
    had_error: bool,
    panic_mode: bool,
    current_chunk: Chunk,
}

fn map_rule(token: TokenKind) -> Rule {
    match token {
        TokenKind::LEFT_PAREN => (Some(RuleFn::Grouping), None, Precedence::NONE),
        TokenKind::RIGHT_PAREN => (None, None, Precedence::NONE),
        TokenKind::LEFT_BRACE => (None, None, Precedence::NONE),
        TokenKind::RIGHT_BRACE => (None, None, Precedence::NONE),
        TokenKind::COMMA => (None, None, Precedence::NONE),
        TokenKind::DOT => (None, None, Precedence::NONE),
        TokenKind::MINUS => (Some(RuleFn::Unary), Some(RuleFn::Binary), Precedence::TERM),
        TokenKind::PLUS => (None, Some(RuleFn::Binary), Precedence::TERM),
        TokenKind::SEMICOLON => (None, None, Precedence::NONE),
        TokenKind::SLASH => (None, Some(RuleFn::Binary), Precedence::FACTOR),
        TokenKind::STAR => (None, Some(RuleFn::Binary), Precedence::FACTOR),
        TokenKind::BANG => (None, None, Precedence::NONE),
        TokenKind::BANG_EQUAL => (None, None, Precedence::NONE),
        TokenKind::EQUAL => (None, None, Precedence::NONE),
        TokenKind::EQUAL_EQUAL => (None, None, Precedence::NONE),
        TokenKind::GREATER => (None, None, Precedence::NONE),
        TokenKind::GREATER_EQUAL => (None, None, Precedence::NONE),
        TokenKind::LESS => (None, None, Precedence::NONE),
        TokenKind::LESS_EQUAL => (None, None, Precedence::NONE),
        TokenKind::IDENTIFIER => (None, None, Precedence::NONE),
        TokenKind::STRING => (None, None, Precedence::NONE),
        TokenKind::NUMBER => (Some(RuleFn::Number), None, Precedence::NONE),
        TokenKind::AND => (None, None, Precedence::NONE),
        TokenKind::CLASS => (None, None, Precedence::NONE),
        TokenKind::ELSE => (None, None, Precedence::NONE),
        TokenKind::FALSE => (None, None, Precedence::NONE),
        TokenKind::FOR => (None, None, Precedence::NONE),
        TokenKind::FUN => (None, None, Precedence::NONE),
        TokenKind::IF => (None, None, Precedence::NONE),
        TokenKind::NIL => (None, None, Precedence::NONE),
        TokenKind::OR => (None, None, Precedence::NONE),
        TokenKind::PRINT => (None, None, Precedence::NONE),
        TokenKind::RETURN => (None, None, Precedence::NONE),
        TokenKind::SUPER => (None, None, Precedence::NONE),
        TokenKind::THIS => (None, None, Precedence::NONE),
        TokenKind::TRUE => (None, None, Precedence::NONE),
        TokenKind::VAR => (None, None, Precedence::NONE),
        TokenKind::WHILE => (None, None, Precedence::NONE),
        TokenKind::ERROR => (None, None, Precedence::NONE),
        TokenKind::EOF => (None, None, Precedence::NONE),
        TokenKind::BREAK => (None, None, Precedence::NONE),
        TokenKind::CONTINUE => (None, None, Precedence::NONE),
    }
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

    // === EMITTERS ===

    fn emit_op(&mut self, op: OpCode) {
        self.emit_byte(op as u8);
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

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_byte_2(OpCode::Constant as u8, index);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.current_chunk.add_constant(value);
        if constant > std::u8::MAX {
            self.error("Too many constants in one chunk");
            return 0;
        }

        constant
    }

    fn end(&mut self) {
        self.emit_return();
    }

    fn emit_return(&mut self) {
        self.emit_op(OpCode::Return);
    }

    // === Expr Parser
    fn expression(&mut self) {
        self.parse_precedence(Precedence::NONE);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        unimplemented!()
    }

    // === Token  Parser

    fn number(&mut self) {
        let value = self.previous_token().lexeme.parse::<f64>().unwrap();
        self.emit_constant(value);
    }

    fn grouping(&mut self) -> ParseResult {
        self.expression();
        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after expression.")
    }

    fn unary(&mut self) -> ParseResult {
        let op_kind = self.previous_token().kind;

        self.parse_precedence(Precedence::UNARY);

        match op_kind {
            TokenKind::MINUS => self.emit_op(OpCode::Negate),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let op_kind = self.previous_token().kind;

        Ok(())
    }

    // helpers
    #[inline(always)]
    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    #[inline(always)]
    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

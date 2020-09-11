pub mod chunk;
pub mod debug;
pub mod scanner;
pub mod token;
pub mod value;

use crate::vm::InterpretError;
use chunk::{Chunk, OpCode};
use debug::disassemble_chunk;
use scanner::Scanner;
use token::{Token, TokenKind};
use value::Value;

pub fn compile(source: &str) -> Result<Chunk, InterpretError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    parser.expression()?;
    parser.consume(TokenKind::EOF, "Expect end of expression.")?;
    parser.end();

    Ok(parser.current_chunk)
}

#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1, // =
    OR = 2,         // OR
    AND = 3,        // AND
    EQUALITY = 4,   // == !=
    COMPARISON = 5, // < > <= >=
    TERM = 6,       // + -
    FACTOR = 7,     // * /
    UNARY = 8,      // ! -
    CALL = 9,       // . ()
    PRIMARY = 10,
}

impl From<u8> for Precedence {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Self::NONE,
            1 => Self::ASSIGNMENT,
            2 => Self::OR,
            3 => Self::AND,
            4 => Self::EQUALITY,
            5 => Self::COMPARISON,
            6 => Self::TERM,
            7 => Self::FACTOR,
            8 => Self::UNARY,
            9 => Self::CALL,
            10 => Self::PRIMARY,
            _ => unreachable!(),
        }
    }
}

type ParseResult = Result<(), InterpretError>;

enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
}

// (Prefix, Infix, precedence of infix)
type Rule = (Option<ParseFn>, Option<ParseFn>, Precedence);

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl From<Rule> for ParseRule {
    fn from(rule: Rule) -> Self {
        Self {
            prefix: rule.0,
            infix: rule.1,
            precedence: rule.2,
        }
    }
}

fn rule(token: TokenKind) -> ParseRule {
    match token {
        TokenKind::LEFT_PAREN => (Some(ParseFn::Grouping), None, Precedence::NONE),
        TokenKind::RIGHT_PAREN => (None, None, Precedence::NONE),
        TokenKind::LEFT_BRACE => (None, None, Precedence::NONE),
        TokenKind::RIGHT_BRACE => (None, None, Precedence::NONE),
        TokenKind::COMMA => (None, None, Precedence::NONE),
        TokenKind::DOT => (None, None, Precedence::NONE),
        TokenKind::MINUS => (
            Some(ParseFn::Unary),
            Some(ParseFn::Binary),
            Precedence::TERM,
        ),
        TokenKind::PLUS => (None, Some(ParseFn::Binary), Precedence::TERM),
        TokenKind::SEMICOLON => (None, None, Precedence::NONE),
        TokenKind::SLASH => (None, Some(ParseFn::Binary), Precedence::FACTOR),
        TokenKind::STAR => (None, Some(ParseFn::Binary), Precedence::FACTOR),
        TokenKind::BANG => (Some(ParseFn::Unary), None, Precedence::NONE),
        TokenKind::BANG_EQUAL => (None, Some(ParseFn::Binary), Precedence::EQUALITY),
        TokenKind::EQUAL => (None, None, Precedence::NONE),
        TokenKind::EQUAL_EQUAL => (None, Some(ParseFn::Binary), Precedence::EQUALITY),
        TokenKind::GREATER => (None, Some(ParseFn::Binary), Precedence::COMPARISON),
        TokenKind::GREATER_EQUAL => (None, Some(ParseFn::Binary), Precedence::COMPARISON),
        TokenKind::LESS => (None, Some(ParseFn::Binary), Precedence::COMPARISON),
        TokenKind::LESS_EQUAL => (None, Some(ParseFn::Binary), Precedence::COMPARISON),
        TokenKind::IDENTIFIER => (None, None, Precedence::NONE),
        TokenKind::STRING => (None, None, Precedence::NONE),
        TokenKind::NUMBER => (Some(ParseFn::Number), None, Precedence::NONE),
        TokenKind::AND => (None, None, Precedence::NONE),
        TokenKind::CLASS => (None, None, Precedence::NONE),
        TokenKind::ELSE => (None, None, Precedence::NONE),
        TokenKind::FALSE => (Some(ParseFn::Literal), None, Precedence::NONE),
        TokenKind::FOR => (None, None, Precedence::NONE),
        TokenKind::FUN => (None, None, Precedence::NONE),
        TokenKind::IF => (None, None, Precedence::NONE),
        TokenKind::NIL => (Some(ParseFn::Literal), None, Precedence::NONE),
        TokenKind::OR => (None, None, Precedence::NONE),
        TokenKind::PRINT => (None, None, Precedence::NONE),
        TokenKind::RETURN => (None, None, Precedence::NONE),
        TokenKind::SUPER => (None, None, Precedence::NONE),
        TokenKind::THIS => (None, None, Precedence::NONE),
        TokenKind::TRUE => (Some(ParseFn::Literal), None, Precedence::NONE),
        TokenKind::VAR => (None, None, Precedence::NONE),
        TokenKind::WHILE => (None, None, Precedence::NONE),
        TokenKind::ERROR => (None, None, Precedence::NONE),
        TokenKind::EOF => (None, None, Precedence::NONE),
        TokenKind::BREAK => (None, None, Precedence::NONE),
        TokenKind::CONTINUE => (None, None, Precedence::NONE),
    }
    .into()
}

struct Parser {
    current: usize,
    tokens: Vec<Token>,
    had_error: bool,
    panic_mode: bool,
    debug: bool,
    current_chunk: Chunk,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            current: 0,
            tokens: tokens.into(),
            had_error: false,
            panic_mode: false,
            debug: true,
            current_chunk: Chunk::init(),
        }
    }

    pub fn advance(&mut self) -> ParseResult {
        loop {
            let token = self.current_token();
            if token.kind != TokenKind::ERROR {
                self.current += 1;
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

        eprint!("[line {}, col: {}] Error", token.line, token.column);

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

        if self.debug && !self.had_error {
            disassemble_chunk(&self.current_chunk, "code");
        }
    }

    fn emit_return(&mut self) {
        self.emit_op(OpCode::Return);
    }

    // === Expr Parser
    fn expression(&mut self) -> ParseResult {
        self.parse_precedence(Precedence::ASSIGNMENT)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult {
        self.advance()?;

        if let Some(fun) = rule(self.previous_token().kind).prefix {
            self.parse_fn(fun)?;
        } else {
            self.error("Expect expression.");
            return Err(InterpretError::CompileError);
        }

        while precedence <= rule(self.current_token().kind).precedence {
            self.advance()?;
            if let Some(fun) = rule(self.previous_token().kind).infix {
                self.parse_fn(fun)?;
            }
        }
        Ok(())
    }

    // === Token  Parser

    fn number(&mut self) -> ParseResult {
        let value = self.previous_token().lexeme.parse::<f64>().unwrap();
        self.emit_constant(value.into());
        Ok(())
    }

    fn grouping(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after expression.")
    }

    fn unary(&mut self) -> ParseResult {
        let op_kind = self.previous_token().kind;

        self.parse_precedence(Precedence::UNARY)?;

        match op_kind {
            TokenKind::MINUS => self.emit_op(OpCode::Negate),
            TokenKind::BANG => self.emit_op(OpCode::Not),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let op_kind = self.previous_token().kind;

        let rule = rule(op_kind);
        self.parse_precedence(Precedence::from(rule.precedence as u8 + 1))?;

        match op_kind {
            TokenKind::PLUS => self.emit_op(OpCode::Add),
            TokenKind::MINUS => self.emit_op(OpCode::Subtract),
            TokenKind::STAR => self.emit_op(OpCode::Multiply),
            TokenKind::SLASH => self.emit_op(OpCode::Divide),

            TokenKind::EQUAL_EQUAL => self.emit_op(OpCode::Equal),
            TokenKind::GREATER => self.emit_op(OpCode::Greater),
            TokenKind::LESS => self.emit_op(OpCode::Less),

            TokenKind::GREATER_EQUAL => {
                self.emit_op(OpCode::Less);
                self.emit_op(OpCode::Not);
            }
            TokenKind::LESS_EQUAL => {
                self.emit_op(OpCode::Greater);
                self.emit_op(OpCode::Not);
            }

            TokenKind::BANG_EQUAL => {
                self.emit_op(OpCode::Equal);
                self.emit_op(OpCode::Not);
            }

            _ => unreachable!(),
        }

        Ok(())
    }

    fn literal(&mut self) -> ParseResult {
        match self.previous_token().kind {
            TokenKind::FALSE => self.emit_op(OpCode::False),
            TokenKind::TRUE => self.emit_op(OpCode::True),
            TokenKind::NIL => self.emit_op(OpCode::Nil),

            _ => unreachable!(),
        }
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

    fn parse_fn(&mut self, fun: ParseFn) -> ParseResult {
        match fun {
            ParseFn::Number => self.number(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Literal => self.literal(),
        }
    }
}

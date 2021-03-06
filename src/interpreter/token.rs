use crate::interpreter::ast::{Expr, Statement};
use std::fmt;
use std::hash::{Hash, Hasher};

#[allow(non_camel_case_types, dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    BREAK,
    CONTINUE,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

#[derive(Debug, Clone)]
pub enum TokenLiteral {
    // probably want some information on where an identifier is declared
    Bool(bool),
    Identifier(String),
    Str(String),
    Number(f64),
    None,
    Uninit,
    Break,
    Continue,
}

impl TokenLiteral {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::None => false,
            _ => true,
        }
    }

    pub fn number(&self) -> Option<f64> {
        self.clone().into()
    }
}

impl Hash for TokenLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Identifier(s) => s.hash(state),
            Self::Str(s) => s.hash(state),
            Self::Number(_) => {
                unimplemented!();
            }
            Self::Bool(b) => b.hash(state),
            Self::None => 0.hash(state),
            _ => unimplemented!(),
        }
    }
}

impl PartialEq for TokenLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::Str(l), Self::Str(r)) | (Self::Identifier(l), Self::Identifier(r)) => l == r,
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::None, Self::None) => true,
            _ => false,
        }
    }
}

impl Eq for TokenLiteral {}

impl From<TokenLiteral> for Expr {
    fn from(token: TokenLiteral) -> Self {
        Self::Literal(token)
    }
}

impl From<TokenLiteral> for Statement {
    fn from(token: TokenLiteral) -> Self {
        Self::Expr(Expr::Literal(token))
    }
}

impl fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Number(n) => {
                let mut s = n.to_string();
                if s.ends_with(".0") {
                    s.truncate(s.len() - 2);
                }

                write!(f, "{}", s)
            }
            Self::Bool(b) => write!(f, "{}", b.to_string()),
            Self::None => write!(f, "nil"),
            Self::Uninit => write!(f, "uninitalized"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
        }
    }
}

impl Into<Option<f64>> for TokenLiteral {
    fn into(self) -> Option<f64> {
        match self {
            Self::Number(n) => Some(n),
            _ => None,
        }
    }
}

impl Into<Option<bool>> for TokenLiteral {
    fn into(self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(b),
            _ => None,
        }
    }
}

impl Into<Option<String>> for TokenLiteral {
    fn into(self) -> Option<String> {
        match self {
            Self::Str(s) | Self::Identifier(s) => Some(s),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    //@TODO add literal somehow
    pub literal: TokenLiteral,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(
        token_type: TokenKind,
        lexeme: &str,
        literal: TokenLiteral,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            kind: token_type,
            lexeme: lexeme.into(),
            literal,
            line,
            column,
        }
    }

    pub fn none() -> Self {
        Self {
            kind: TokenKind::NIL,
            lexeme: "".into(),
            literal: TokenLiteral::None,
            line: 0,
            column: 0,
        }
    }
}

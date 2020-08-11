use crate::ast::{Expr, Statement};
use crate::token::{Token, TokenLiteral, TokenType};
#[derive(Debug)]
pub enum ParseError {
    Mismatch(Token, String),
}

pub struct Parser {
    tokens: Vec<Token>,

    current: usize,
}

pub type StatementsResult = Result<Vec<Statement>, ParseError>;
pub type StatementResult = Result<Statement, ParseError>;
pub type ParseResult = Result<Expr, ParseError>;

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.into(),

            current: 0,
        }
    }

    pub fn parse(&mut self) -> StatementsResult {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }

    // ======= START GRAMMAR ======

    fn statement(&mut self) -> StatementResult {
        if self.match_rule(&[TokenType::PRINT]) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    pub fn print_statement(&mut self) -> StatementResult {
        let expr = self.expression()?;
        self.consume(TokenType::SEMICOLON, "Expect ';' after expression.")?;

        Ok(Statement::Print(expr))
    }

    pub fn expression_statement(&mut self) -> StatementResult {
        let expr = self.expression()?;
        self.consume(TokenType::SEMICOLON, "Expect ';' after expression.")?;

        Ok(Statement::Expr(expr))
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;

        while self.match_rule(&[TokenType::EQUAL_EQUAL, TokenType::BANG_EQUAL]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.addition()?;

        while self.match_rule(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ]) {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParseResult {
        let mut expr = self.multiplication()?;

        while self.match_rule(&[TokenType::MINUS, TokenType::PLUS]) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ParseResult {
        let mut expr = self.unary()?;

        while self.match_rule(&[TokenType::SLASH, TokenType::STAR]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult {
        if self.match_rule(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator.clone(), Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> ParseResult {
        Ok(match self.advance().token_type {
            TokenType::FALSE => Expr::Literal(TokenLiteral::Bool(false)),
            TokenType::TRUE => Expr::Literal(TokenLiteral::Bool(true)),
            TokenType::NIL | TokenType::EOF => Expr::Literal(TokenLiteral::None),
            TokenType::NUMBER | TokenType::STRING => Expr::Literal(self.previous().literal.clone()),
            TokenType::LEFT_PAREN => {
                let expr = self.expression()?;
                self.consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            _ => {
                return Err(ParseError::Mismatch(
                    self.peek().clone(),
                    "Expect expression.".into(),
                ));
            }
        })
    }

    // ====== END GRAMMAR ======

    // ====== HELPERS ========
    // check if EXPR matches any of types of our grammar rule
    fn match_rule(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(*t) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError::Mismatch(self.peek().clone(), msg.into()))
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).expect("peek machine broke")
    }

    fn previous(&self) -> &Token {
        let current = if self.current == 0 {
            0
        } else {
            self.current - 1
        };

        self.tokens.get(current).expect("previous machine broke")
    }

    // function to get to start of next expr
    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::SEMICOLON {
                break;
            }

            match self.peek().token_type {
                TokenType::CLASS
                | TokenType::FUN
                | TokenType::VAR
                | TokenType::FOR
                | TokenType::IF
                | TokenType::WHILE
                | TokenType::PRINT
                | TokenType::RETURN => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

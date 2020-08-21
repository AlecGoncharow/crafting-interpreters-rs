use crate::ast::{Expr, Statement};
use crate::token::{Token, TokenKind, TokenLiteral};
#[derive(Debug)]
pub enum ParseError {
    Mismatch(Token, String),
    TooManyArgs(Token, String),
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
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    // ======= START GRAMMAR ======

    fn declaration(&mut self) -> StatementResult {
        if self.match_rule(TokenKind::VAR) {
            match self.var_declaration() {
                Ok(s) => Ok(s),
                Err(e) => {
                    self.synchronize();
                    Err(e)
                }
            }
        } else if self.match_rule(TokenKind::FUN) {
            self.function("function")
        } else {
            match self.statement() {
                Ok(s) => Ok(s),
                Err(e) => {
                    self.synchronize();
                    Err(e)
                }
            }
        }
    }

    fn var_declaration(&mut self) -> StatementResult {
        let name = self
            .consume(TokenKind::IDENTIFIER, "Expect variable name.")?
            .clone();

        let mut init = Expr::Literal(TokenLiteral::Uninit);
        if self.match_rule(TokenKind::EQUAL) {
            init = self.expression()?;
        }

        self.consume(
            TokenKind::SEMICOLON,
            "Expect ';' after variable declaration",
        )?;

        Ok(Statement::Var(name, init))
    }

    fn function(&mut self, kind: &str) -> StatementResult {
        let name = self
            .consume(TokenKind::IDENTIFIER, &format!("Expect {} name.", kind))?
            .clone();
        self.consume(
            TokenKind::LEFT_PAREN,
            &format!("Expect '(' after {} name.", kind),
        )?;
        let mut paramaters = Vec::new();
        if !self.check(TokenKind::RIGHT_PAREN) {
            loop {
                if paramaters.len() >= 255 {
                    return Err(ParseError::TooManyArgs(
                        self.peek().clone(),
                        "Cannot have more than 255 paramters.".into(),
                    ));
                }

                paramaters.push(
                    self.consume(TokenKind::IDENTIFIER, "Expect paramter name")?
                        .clone(),
                );

                if !self.match_rule(TokenKind::COMMA) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after paramaters")?;

        self.consume(
            TokenKind::LEFT_BRACE,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        let body = self.block()?;

        Ok(Statement::Function(name, paramaters, body, None))
    }

    fn statement(&mut self) -> StatementResult {
        if self.match_rule(TokenKind::PRINT) {
            self.print_statement()
        } else if self.match_rule(TokenKind::LEFT_BRACE) {
            Ok(Statement::Block(self.block()?))
        } else if self.match_rule(TokenKind::IF) {
            self.if_statment()
        } else if self.match_rule(TokenKind::WHILE) {
            self.while_statement()
        } else if self.match_rule(TokenKind::FOR) {
            self.for_statement()
        } else if self.match_rule(TokenKind::RETURN) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> StatementResult {
        let keyword = self.previous().clone();
        let mut value = Expr::none();
        if !self.check(TokenKind::SEMICOLON) {
            value = self.expression()?;
        }
        self.consume(TokenKind::SEMICOLON, "Expect ';' after return value.")?;
        Ok(Statement::Return(keyword, value))
    }

    fn print_statement(&mut self) -> StatementResult {
        let expr = self.expression()?;
        self.consume(TokenKind::SEMICOLON, "Expect ';' after expression.")?;

        Ok(Statement::Print(expr))
    }

    fn if_statment(&mut self) -> StatementResult {
        self.consume(TokenKind::LEFT_PAREN, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after 'if' condition.")?;

        let then_branch = self.statement()?;
        let mut else_branch = Statement::Expr(Expr::none());
        if self.match_rule(TokenKind::ELSE) {
            else_branch = self.statement()?;
        }

        Ok(Statement::If(
            condition,
            then_branch.into(),
            else_branch.into(),
        ))
    }

    // desugar for into: init; while cond; body; increment;
    fn for_statement(&mut self) -> StatementResult {
        self.consume(TokenKind::LEFT_PAREN, "Expect '(' after 'for'.")?;

        let initializer = match self.peek().kind {
            TokenKind::SEMICOLON => {
                self.advance();
                Statement::Expr(Expr::none())
            }
            TokenKind::VAR => {
                self.advance();
                self.var_declaration()?
            }
            _ => self.expression_statement()?,
        };

        let condition = if self.check(TokenKind::SEMICOLON) {
            Expr::Literal(TokenLiteral::Bool(true))
        } else {
            self.expression()?
        };
        self.consume(TokenKind::SEMICOLON, "Expect ';' after loop condition")?;

        let increment = if self.check(TokenKind::RIGHT_PAREN) {
            Statement::Expr(Expr::none())
        } else {
            Statement::ForIncr(self.expression()?)
        };
        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after for clauses.")?;

        let body = Statement::Block(vec![self.statement()?, increment]);

        let for_loop = Statement::While(condition, body.into());

        Ok(Statement::Block(vec![initializer, for_loop]))
    }

    fn while_statement(&mut self) -> StatementResult {
        self.consume(TokenKind::LEFT_PAREN, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Statement::While(condition, body.into()))
    }

    fn block(&mut self) -> StatementsResult {
        let mut stmts = Vec::new();

        while !(self.check(TokenKind::RIGHT_BRACE) || self.is_at_end()) {
            stmts.push(self.declaration()?);
        }

        self.consume(TokenKind::RIGHT_BRACE, "Expect '}' after block")?;
        Ok(stmts)
    }

    fn expression_statement(&mut self) -> StatementResult {
        let expr = self.expression()?;
        self.consume(TokenKind::SEMICOLON, "Expect ';' after expression.")?;

        Ok(Statement::Expr(expr))
    }

    fn expression(&mut self) -> ParseResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult {
        let expr = self.or()?;

        if self.match_rule(TokenKind::EQUAL) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                Ok(Expr::Assign(var, value.into()))
            } else {
                Err(ParseError::Mismatch(
                    equals,
                    "Invalid assignment target.".into(),
                ))
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> ParseResult {
        let mut expr = self.and()?;

        while self.match_rule(TokenKind::OR) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult {
        let mut expr = self.equality()?;

        while self.match_rule(TokenKind::AND) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;

        while self.match_rules(&[TokenKind::EQUAL_EQUAL, TokenKind::BANG_EQUAL]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.addition()?;

        while self.match_rules(&[
            TokenKind::GREATER,
            TokenKind::GREATER_EQUAL,
            TokenKind::LESS,
            TokenKind::LESS_EQUAL,
        ]) {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParseResult {
        let mut expr = self.multiplication()?;

        while self.match_rules(&[TokenKind::MINUS, TokenKind::PLUS]) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ParseResult {
        let mut expr = self.unary()?;

        while self.match_rules(&[TokenKind::SLASH, TokenKind::STAR]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary((expr, operator, right).into());
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult {
        if self.match_rules(&[TokenKind::BANG, TokenKind::MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary((operator, right).into()));
        }

        self.call()
    }

    fn call(&mut self) -> ParseResult {
        let mut expr = self.primary()?;

        loop {
            if self.match_rule(TokenKind::LEFT_PAREN) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult {
        let mut args = Vec::new();

        if !self.check(TokenKind::RIGHT_PAREN) {
            loop {
                if args.len() >= 255 {
                    return Err(ParseError::TooManyArgs(
                        self.peek().clone(),
                        "Cannot have more than 255 arguments.".into(),
                    ));
                }

                args.push(self.expression()?);

                if !self.match_rule(TokenKind::COMMA) {
                    break;
                }
            }
        }
        let paren = self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after arguments.")?;

        Ok(Expr::Call(callee.into(), paren.clone(), args))
    }

    fn primary(&mut self) -> ParseResult {
        Ok(match self.advance().kind {
            TokenKind::FALSE => Expr::Literal(TokenLiteral::Bool(false)),
            TokenKind::TRUE => Expr::Literal(TokenLiteral::Bool(true)),
            TokenKind::NIL | TokenKind::EOF => Expr::Literal(TokenLiteral::None),
            TokenKind::CONTINUE => Expr::Literal(TokenLiteral::Continue),
            TokenKind::BREAK => Expr::Literal(TokenLiteral::Break),
            TokenKind::NUMBER | TokenKind::STRING => Expr::Literal(self.previous().literal.clone()),
            TokenKind::LEFT_PAREN => {
                let expr = self.expression()?;
                self.consume(TokenKind::RIGHT_PAREN, "Expect ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            TokenKind::IDENTIFIER => Expr::Variable(self.previous().clone()),
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
    fn match_rules(&mut self, types: &[TokenKind]) -> bool {
        for t in types {
            if self.check(*t) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn match_rule(&mut self, t: TokenKind) -> bool {
        self.match_rules(&[t])
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn consume(&mut self, token_type: TokenKind, msg: &str) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError::Mismatch(self.peek().clone(), msg.into()))
        }
    }

    fn check(&self, token_type: TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind == token_type
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EOF
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
            if self.previous().kind == TokenKind::SEMICOLON {
                break;
            }

            match self.peek().kind {
                TokenKind::CLASS
                | TokenKind::FUN
                | TokenKind::VAR
                | TokenKind::FOR
                | TokenKind::IF
                | TokenKind::WHILE
                | TokenKind::PRINT
                | TokenKind::RETURN => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

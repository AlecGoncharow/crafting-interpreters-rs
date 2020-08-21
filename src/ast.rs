use crate::environment::Environment;
use crate::interpreter::ExecutorError;
use crate::token::{Token, TokenLiteral};

#[derive(Clone, Debug)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<BinaryExpr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Grouping(Box<Expr>),
    Literal(TokenLiteral),
    Unary(Box<UnaryExpr>),
    Logical(Box<LogicalExpr>),
    Variable(Token),
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl From<(Expr, Token, Expr)> for BinaryExpr {
    fn from((left, operator, right): (Expr, Token, Expr)) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl From<(Expr, Token, Expr)> for Box<BinaryExpr> {
    fn from((left, operator, right): (Expr, Token, Expr)) -> Self {
        Box::new(BinaryExpr {
            left,
            operator,
            right,
        })
    }
}

#[derive(Clone, Debug)]
pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl From<(Expr, Token, Expr)> for LogicalExpr {
    fn from((left, operator, right): (Expr, Token, Expr)) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl From<(Expr, Token, Expr)> for Box<LogicalExpr> {
    fn from((left, operator, right): (Expr, Token, Expr)) -> Self {
        Box::new(LogicalExpr {
            left,
            operator,
            right,
        })
    }
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub expr: Expr,
}

impl From<(Token, Expr)> for UnaryExpr {
    fn from((operator, expr): (Token, Expr)) -> Self {
        Self { operator, expr }
    }
}

impl From<(Token, Expr)> for Box<UnaryExpr> {
    fn from((operator, expr): (Token, Expr)) -> Self {
        Box::new(UnaryExpr { operator, expr })
    }
}

impl Expr {
    pub fn none() -> Self {
        Self::Literal(TokenLiteral::None)
    }

    pub fn literal(&self) -> &TokenLiteral {
        match self {
            Self::Literal(lit) => lit,
            _ => &TokenLiteral::None,
        }
    }
}

impl From<Expr> for Statement {
    fn from(expr: Expr) -> Self {
        Self::Expr(expr)
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expr),
    ForIncr(Expr),
    Function(Token, Vec<Token>, Vec<Statement>, Option<Environment>),
    If(Expr, Box<Statement>, Box<Statement>),
    Print(Expr),
    Var(Token, Expr),
    While(Expr, Box<Statement>),
    Block(Vec<Statement>),
    Return(Token, Expr),
}

pub struct StatementBlock {
    pub statements: Vec<Statement>,
}

impl Statement {
    #[allow(dead_code)]
    pub fn expr(&self) -> Expr {
        match self {
            Self::Expr(expr) | Self::Print(expr) => expr.clone(),
            Self::Var(_, expr) => expr.clone(),
            _ => Expr::none(),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Function(_, args, _, _) => args.len(),
            _ => 0,
        }
    }
}

pub struct AstPrinter {
    buf: String,
}

type VisitorResult = Result<(), ExecutorError>;

impl AstPrinter {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }

    pub fn print(mut self, expr: &Statement) -> VisitorResult {
        self.visit_statement(expr)?;
        println!("{}", self.buf);
        Ok(())
    }

    pub fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> VisitorResult {
        self.buf.push('(');
        self.buf.push_str(name);
        for expr in exprs {
            self.buf.push(' ');
            self.visit_expr(expr)?;
        }
        self.buf.push(')');
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitorResult {
        match expr {
            Expr::Assign(token, expr) => {
                self.parenthesize("=", &[&Expr::Literal(token.literal.clone()), expr])?;
            }
            Expr::Binary(inner) => {
                self.parenthesize(&inner.operator.lexeme, &[&inner.left, &inner.right])?
            }
            Expr::Logical(inner) => {
                self.parenthesize(&inner.operator.lexeme, &[&inner.left, &inner.right])?
            }
            Expr::Call(callee, _, arguments) => {
                self.buf.push_str("(call ");
                self.visit_expr(callee)?;
                for arg in arguments {
                    self.buf.push(' ');
                    self.visit_expr(arg)?;
                }
                self.buf.push(')');
            }
            Expr::Grouping(expression) => self.parenthesize("group", &[&expression])?,
            Expr::Literal(literal) => self.buf.push_str(&literal.to_string()),
            Expr::Unary(inner) => self.parenthesize(&inner.operator.lexeme, &[&inner.expr])?,
            Expr::Variable(token) => self.buf.push_str(&token.literal.to_string()),
        }

        Ok(())
    }

    fn visit_statement(&mut self, stmt: &Statement) -> VisitorResult {
        match stmt {
            Statement::Expr(expr) | Statement::ForIncr(expr) => {
                self.visit_expr(expr)?;
            }
            Statement::If(cond, then_branch, else_branch) => {
                self.buf.push_str("(if ");
                self.visit_expr(cond)?;
                self.buf.push_str(" then ");
                self.visit_statement(then_branch)?;
                self.buf.push_str(" else ");
                self.visit_statement(else_branch)?;
                self.buf.push(')');
            }
            Statement::Function(name, args, body, _) => {
                self.buf.push_str("(func_decl ");
                self.buf.push_str(&name.lexeme);
                self.buf.push(' ');
                args.iter()
                    .for_each(|arg| self.buf.push_str(&(arg.lexeme.clone() + " ")));
                self.buf.push_str("(block ");
                for stmt in body {
                    self.visit_statement(stmt)?;
                }
                self.buf.push(')');
                self.buf.push(')');
            }
            Statement::Print(expr) => {
                self.parenthesize("print", &[expr])?;
            }
            Statement::Var(token, expr) => {
                self.parenthesize("=", &[&Expr::Literal(token.literal.clone()), expr])?;
            }
            Statement::Return(_token, expr) => {
                self.parenthesize("return", &[expr])?;
            }
            Statement::While(expr, stmt) => {
                self.buf.push_str("(while ");
                self.visit_expr(expr)?;
                self.visit_statement(stmt)?;
                self.buf.push(')');
            }
            Statement::Block(stmts) => {
                self.buf.push_str("(block ");
                for stmt in stmts {
                    self.visit_statement(stmt)?;
                }
                self.buf.push(')');
            }
        }

        Ok(())
    }
}

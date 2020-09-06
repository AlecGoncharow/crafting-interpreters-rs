use super::ExecutorError;
use super::token::{Token, TokenLiteral};

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<BinaryExpr>),
    Call(Box<Expr>, Token, Vec<Statement>),
    Get(Box<Expr>, Token),
    Set(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(TokenLiteral),
    This(Token),
    Unary(Box<UnaryExpr>),
    Logical(Box<LogicalExpr>),
    Variable(Token),
}

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expr(Expr),
    ForIncr(Expr),
    Function(StatementFunction),
    If(Expr, Box<Statement>, Box<Statement>),
    Print(Expr),
    Var(Token, Expr),
    While(Expr, Box<Statement>),
    Class(Token, Vec<StatementFunction>),
    Block(Box<StatementBlock>),
    Return(Token, Expr),
}

#[derive(Clone, PartialEq, Debug)]
pub struct StatementFunction {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: StatementBlock,
}

impl From<(Token, Vec<Token>, StatementBlock)> for StatementFunction {
    fn from((name, params, body): (Token, Vec<Token>, StatementBlock)) -> Self {
        Self { name, params, body }
    }
}

impl From<(Token, Vec<Token>, StatementBlock)> for Box<StatementFunction> {
    fn from((name, params, body): (Token, Vec<Token>, StatementBlock)) -> Self {
        Box::new(StatementFunction { name, params, body })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StatementBlock {
    pub statements: Vec<Statement>,
}

impl From<Vec<Statement>> for StatementBlock {
    fn from(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

impl From<Vec<Statement>> for Box<StatementBlock> {
    fn from(statements: Vec<Statement>) -> Self {
        Box::new(StatementBlock { statements })
    }
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
            Self::Function(func) => func.params.len(),
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
                    self.visit_statement(arg)?;
                }
                self.buf.push(')');
            }
            Expr::Get(_object, name) => {
                self.buf.push_str("(Get ");
                self.buf.push_str(&name.lexeme);
                self.buf.push(')');
            }
            Expr::Set(_object, name, value) => {
                self.buf.push_str("(Set ");
                self.buf.push_str(&name.lexeme);
                self.buf.push(' ');
                self.visit_expr(value)?;
                self.buf.push(')');
            }
            Expr::Grouping(expression) => self.parenthesize("group", &[&expression])?,
            Expr::Literal(literal) => self.buf.push_str(&literal.to_string()),
            Expr::This(kw) => self.buf.push_str(&kw.lexeme),
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
            Statement::Function(func) => {
                self.buf.push_str("(func_decl ");
                self.buf.push_str(&func.name.lexeme);
                self.buf.push(' ');
                func.params
                    .iter()
                    .for_each(|arg| self.buf.push_str(&(arg.lexeme.clone() + " ")));
                self.buf.push_str("(block ");
                for stmt in &func.body.statements {
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
            Statement::Class(name, methods) => {
                self.buf.push_str("(class_decl ");
                self.buf.push_str(&name.lexeme);
                self.buf.push(' ');
                for method in methods {
                    self.visit_statement(&Statement::Function(method.clone()))?;
                }
                self.buf.push(')');
            }
            Statement::Block(stmts) => {
                self.buf.push_str("(block ");
                for stmt in &stmts.statements {
                    self.visit_statement(&stmt)?;
                }
                self.buf.push(')');
            }
        }

        Ok(())
    }
}

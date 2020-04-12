use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::rc::Rc;
use std::result::Result;

pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Rc<Expr>),
    If(Rc<Expr>, Rc<Stmt>, Option<Rc<Stmt>>),
    Print(Rc<Expr>),
    Var(Token, Option<Rc<Expr>>),
    While(Rc<Expr>, Rc<Stmt>),
}

pub enum Expr {
    Asign(Token, Rc<Expr>),
    Binary(Rc<Expr>, Token, Rc<Expr>),
    Grouping(Rc<Expr>),
    Literal(Literal),
    Logical(Rc<Expr>, Token, Rc<Expr>),
    Unary(Token, Rc<Expr>),
    Variable(Token),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Stmt::*;
        match self {
            Block(statemets) => write!(f, "(do {})", join(statemets, " ")),
            Expression(expression) => write!(f, "(expr {})", expression),
            If(condition, then, None) => write!(f, "(if {} {})", condition, then),
            If(condition, then, Some(r#else)) => {
                write!(f, "(if {} {} {})", condition, then, r#else)
            }
            Print(expression) => write!(f, "(print {})", expression),
            Var(name, None) => write!(f, "(def {})", name.lexeme),
            Var(name, Some(initializer)) => write!(f, "(def {} {})", name.lexeme, initializer),
            While(condition, body) => write!(f, "(while {} {})", condition, body),
        }
    }
}

fn join<D: Display>(ds: &[D], separator: &str) -> String {
    ds.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(separator)
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Asign(name, value) => write!(f, "(set! {} {})", name.lexeme, value),
            Binary(left, operator, right) => write!(f, "({} {} {})", operator.lexeme, left, right),
            Grouping(expression) => write!(f, "(group {})", expression),
            Literal(value) => write!(f, "{}", value),
            Logical(left, operator, right) => write!(f, "({} {} {})", operator.lexeme, left, right),
            Unary(operator, right) => write!(f, "({} {})", operator.lexeme, right),
            Variable(name) => write!(f, "{}", name.lexeme),
        }
    }
}

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
            Expression(expr) => write!(f, "(expr {})", expr),
            If(cond, then, None) => write!(f, "(if {} {})", cond, then),
            If(cond, then, Some(r#else)) => write!(f, "(if {} {} {})", cond, then, r#else),
            Print(expr) => write!(f, "(print {})", expr),
            Var(name, None) => write!(f, "(def {})", name.lexeme),
            Var(name, Some(init)) => write!(f, "(def {} {})", name.lexeme, init),
            While(cond, body) => write!(f, "(while {} {})", cond, body),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Asign(name, value) => write!(f, "(set! {} {})", name.lexeme, value),
            Binary(left, op, right) => write!(f, "({} {} {})", op.lexeme, left, right),
            Grouping(expr) => write!(f, "(group {})", expr),
            Literal(value) => write!(f, "{}", value),
            Logical(left, op, right) => write!(f, "({} {} {})", op.lexeme, left, right),
            Unary(op, right) => write!(f, "({} {})", op.lexeme, right),
            Variable(name) => write!(f, "{}", name.lexeme),
        }
    }
}

fn join<D: Display>(ds: &[D], separator: &str) -> String {
    ds.iter()
        .map(D::to_string)
        .collect::<Vec<_>>()
        .join(separator)
}

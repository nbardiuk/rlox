use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::rc::Rc;
use std::result::Result;

#[derive(Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Rc<Expr>),
    Function(Token, Vec<Token>, Vec<Stmt>),
    If(Rc<Expr>, Rc<Stmt>, Option<Rc<Stmt>>),
    Print(Rc<Expr>),
    Var(Token, Option<Rc<Expr>>),
    While(Rc<Expr>, Rc<Stmt>),
}

pub enum Expr {
    Asign(Token, Rc<Expr>),
    Binary(Rc<Expr>, Token, Rc<Expr>),
    Call(Rc<Expr>, Token, Vec<Expr>),
    Grouping(Rc<Expr>),
    Literal(Literal),
    Logical(Rc<Expr>, Token, Rc<Expr>),
    Unary(Token, Rc<Expr>),
    Variable(Token),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.lexeme)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Stmt::*;
        match self {
            Block(statemets) => write!(f, "(do {})", join(statemets, " ")),
            Expression(expr) => write!(f, "(expr {})", expr),
            Function(name, params, body) => write!(
                f,
                "(defn {} ({}) {})",
                name,
                join(params, " "),
                join(body, " ")
            ),
            If(cond, then, None) => write!(f, "(if {} {})", cond, then),
            If(cond, then, Some(r#else)) => write!(f, "(if {} {} {})", cond, then, r#else),
            Print(expr) => write!(f, "(print {})", expr),
            Var(name, None) => write!(f, "(def {})", name),
            Var(name, Some(init)) => write!(f, "(def {} {})", name, init),
            While(cond, body) => write!(f, "(while {} {})", cond, body),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Asign(name, value) => write!(f, "(set! {} {})", name, value),
            Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Call(callee, _, args) => write!(f, "({} {})", callee, join(args, " ")),
            Grouping(expr) => write!(f, "(group {})", expr),
            Literal(value) => write!(f, "{}", value),
            Logical(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Unary(op, right) => write!(f, "({} {})", op, right),
            Variable(name) => write!(f, "{}", name),
        }
    }
}

fn join<D: Display>(ds: &[D], separator: &str) -> String {
    ds.iter()
        .map(D::to_string)
        .collect::<Vec<_>>()
        .join(separator)
}

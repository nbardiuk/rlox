use crate::token::{Literal, Token};
use std::fmt::{Display, Error, Formatter};
use std::result::Result;

#[derive(Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Class(Box<Token>, Option<Box<Token>>, Vec<Stmt>),
    Expression(Box<Expr>),
    Function(Box<Token>, Vec<Token>, Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Print(Box<Expr>),
    Return(Box<Token>, Option<Box<Expr>>),
    Var(Box<Token>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Stmt>),
}

#[derive(Clone)]
pub enum Expr {
    Asign(Box<Token>, Box<Expr>),
    Binary(Box<Expr>, Box<Token>, Box<Expr>),
    Call(Box<Expr>, Box<Token>, Vec<Expr>),
    Get(Box<Expr>, Box<Token>),
    Grouping(Box<Expr>),
    Literal(Box<Literal>),
    Logical(Box<Expr>, Box<Token>, Box<Expr>),
    Unary(Box<Token>, Box<Expr>),
    Set(Box<Expr>, Box<Token>, Box<Expr>),
    Super(Box<Token>, Box<Token>),
    This(Box<Token>),
    Variable(Box<Token>),
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
            Class(name, None, methods) => write!(f, "(class {} {})", name, join(methods, " ")),
            Class(name, Some(superclass), methods) => {
                write!(f, "(class {} {} {})", name, superclass, join(methods, " "))
            }
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
            Return(_, None) => write!(f, "(return)"),
            Return(_, Some(expr)) => write!(f, "(return {})", expr),
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
            Get(object, name) => write!(f, "(get {} :{})", object, name),
            Grouping(expr) => write!(f, "(group {})", expr),
            Literal(value) => write!(f, "{}", value),
            Logical(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Unary(op, right) => write!(f, "({} {})", op, right),
            Set(object, name, value) => write!(f, "(set {} :{} {})", object, name, value),
            Super(_keyword, method) => write!(f, "(get super :{})", method),
            This(_keyword) => write!(f, "this"),
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

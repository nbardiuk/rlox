use std::cmp::{Eq, PartialEq};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::result::Result;

#[derive(Clone, Debug)]
pub struct Token {
    column: usize,
    pub lexeme: String,
    pub line: usize,
    pub literal: Literal,
    pub typ: TokenType,
}
impl Eq for Token {}
impl PartialEq for Token {
    fn eq(&self, r: &Self) -> bool {
        self.column == r.column && self.line == r.line
    }
}
impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u8(self.column as u8);
    }
}
impl Token {
    pub fn new(typ: TokenType, lexeme: &str, literal: Literal, line: usize, column: usize) -> Self {
        Self {
            typ,
            lexeme: lexeme.to_string(),
            literal,
            line,
            column,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Literal::*;
        match self {
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Number(n) => write!(f, "{}", n),
            String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum TokenType {
    And,
    Bang,
    BangEqual,
    Class,
    Comma,
    Dot,
    EOF,
    Else,
    Equal,
    EqualEqual,
    False,
    For,
    Fun,
    Greater,
    GreaterEqual,
    Identifier,
    If,
    LeftBrace,
    LeftParen,
    Less,
    LessEqual,
    Minus,
    Nil,
    Number,
    Or,
    Plus,
    Print,
    Return,
    RightBrace,
    RightParen,
    Semicolon,
    Slash,
    Star,
    String,
    Super,
    This,
    True,
    Var,
    While,
}

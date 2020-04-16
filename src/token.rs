use std::cmp::{Eq, PartialEq};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::result::Result;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Token {
    pub typ: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: usize,
    pub column: usize,
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

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}
impl Eq for Literal {}
impl PartialEq for Literal {
    fn eq(&self, r: &Self) -> bool {
        use Literal::*;
        match (self, r) {
            (Number(a), Number(b)) => {
                ((a * 10000.).round() as u64) == ((b * 10000.).round() as u64)
            }
            (Bool(a), Bool(b)) => a == b,
            (Nil, Nil) => true,
            (String(a), String(b)) => a == b,
            _ => false,
        }
    }
}
impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Literal::*;
        match self {
            Bool(b) => {
                0.hash(state);
                b.hash(state);
            }
            Nil => {
                1.hash(state);
            }
            Number(f) => {
                2.hash(state);
                ((f * 10000.).round() as u64).hash(state);
            }
            String(s) => {
                3.hash(state);
                s.hash(state);
            }
        }
    }
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

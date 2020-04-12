use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub typ: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: usize,
}

impl Token {
    pub fn new(typ: TokenType, lexeme: &str, literal: Literal, line: usize) -> Self {
        Self {
            typ,
            lexeme: lexeme.to_string(),
            literal,
            line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{:?} {} {}", self.typ, self.lexeme, self.literal)
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

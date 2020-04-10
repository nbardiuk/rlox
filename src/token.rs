use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::result::Result;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub lexeme: &'a str,
    pub literal: Literal<'a>,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(typ: TokenType, lexeme: &'a str, literal: Literal<'a>, line: usize) -> Self {
        Self {
            typ,
            lexeme,
            literal,
            line,
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{:?} {} {}", self.typ, self.lexeme, self.literal)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(f64),
    Nil,
    False,
    True,
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Literal::*;
        match self {
            String(s) => write!(f, "\"{}\"", s),
            Number(n) => write!(f, "{}", n),
            Nil => write!(f, "nil"),
            True => write!(f, "true"),
            False => write!(f, "false"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

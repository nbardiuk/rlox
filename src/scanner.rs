use crate::lox::Lox;

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self, lox: &mut Lox) -> Vec<Token> {
        while !self.is_at_end() {
            // we are at the beginning of the next lexeme.
            self.start = self.current;
            self.scan_token(lox);
        }
        self.tokens.push(Token::new(EOF, "", None, self.line));
        self.tokens.clone()
    }

    fn scan_token(&mut self, lox: &mut Lox) {
        let c = self.advance();
        match c {
            Some('(') => self.add_token(LeftParen),
            Some(')') => self.add_token(RightParen),
            Some('{') => self.add_token(LeftBrace),
            Some('}') => self.add_token(RightBrace),
            Some(',') => self.add_token(Comma),
            Some('.') => self.add_token(Dot),
            Some('-') => self.add_token(Minus),
            Some('+') => self.add_token(Plus),
            Some(';') => self.add_token(Semicolon),
            Some('*') => self.add_token(Star),
            Some('!') => {
                let token = if self.matches('=') { BangEqual } else { Bang };
                self.add_token(token)
            }
            Some('=') => {
                let token = if self.matches('=') { EqualEqual } else { Equal };
                self.add_token(token)
            }
            Some('<') => {
                let token = if self.matches('=') { LessEqual } else { Less };
                self.add_token(token)
            }
            Some('>') => {
                let token = if self.matches('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.add_token(token)
            }
            _ => lox.error(self.line, "Unexpected character."),
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current) != Some(expected) {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }

    fn add_token(&mut self, typ: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token::new(typ, text, None, self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[derive(Clone)]
pub struct Token<'a> {
    typ: TokenType,
    lexeme: &'a str,
    literal: Option<&'a str>, //TODO in example this is a Java Object
    line: usize,
}

impl<'a> Token<'a> {
    fn new(typ: TokenType, lexeme: &'a str, literal: Option<&'a str>, line: usize) -> Self {
        Self {
            typ,
            lexeme,
            literal,
            line,
        }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_fmt(format_args!(
            "{:?} {} {}",
            self.typ,
            self.lexeme,
            self.literal.as_ref().unwrap_or(&"")
        ))
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenType {
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
use TokenType::*;

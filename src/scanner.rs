use crate::lox::Lox;
use std::io::Write;

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

    pub fn scan_tokens<W: Write>(&mut self, lox: &mut Lox<W>) -> Vec<Token> {
        while !self.is_at_end() {
            // we are at the beginning of the next lexeme.
            self.start = self.current;
            self.scan_token(lox);
        }
        self.tokens.push(Token::new(EOF, "", None, self.line));
        self.tokens.clone()
    }

    fn scan_token<W: Write>(&mut self, lox: &mut Lox<W>) {
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
            Some('/') => {
                if self.matches('/') {
                    while self.peek() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash)
                };
            }
            Some(' ') | Some('\r') | Some('\t') => {}
            Some('\n') => self.line += 1,
            Some('"') => self.string(lox),
            _ => lox.error(self.line, "Unexpected character."),
        }
    }

    fn string<W: Write>(&mut self, lox: &mut Lox<W>) {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            lox.error(self.line, "Unterminated string.");
            return;
        }

        self.advance();

        let text = &self.source[self.start + 1..self.current - 1];
        self.add_literal_token(String, text)
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.peek() != Some(expected) {
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

    fn add_literal_token(&mut self, typ: TokenType, literal: &'a str) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(typ, text, Some(literal), self.line))
    }

    fn add_token(&mut self, typ: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token::new(typ, text, None, self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[cfg(test)]
mod spec {
    use super::*;

    fn assert_tokens<'a>(source: &'a str, expected: Vec<Token<'a>>) {
        let mut lox = Lox::<Vec<u8>>::new();
        assert_eq!(Scanner::new(source).scan_tokens(&mut lox), expected);
        assert_eq!(lox.output(), "");
        assert!(!lox.has_error, "no scanning erros");
    }

    fn assert_tokens_error<'a>(source: &'a str, errors: &'a str, expected: Vec<Token<'a>>) {
        let mut lox = Lox::<Vec<u8>>::new();
        assert_eq!(Scanner::new(source).scan_tokens(&mut lox), expected);
        assert!(lox.has_error, "has scanning erros");
        assert_eq!(lox.output(), errors);
    }

    #[test]
    fn empty_source() {
        assert_tokens("", vec![Token::new(EOF, "", None, 1)]);
    }

    #[test]
    fn single_character_tokens() {
        assert_tokens(
            "(){},.-+;/*",
            vec![
                Token::new(LeftParen, "(", None, 1),
                Token::new(RightParen, ")", None, 1),
                Token::new(LeftBrace, "{", None, 1),
                Token::new(RightBrace, "}", None, 1),
                Token::new(Comma, ",", None, 1),
                Token::new(Dot, ".", None, 1),
                Token::new(Minus, "-", None, 1),
                Token::new(Plus, "+", None, 1),
                Token::new(Semicolon, ";", None, 1),
                Token::new(Slash, "/", None, 1),
                Token::new(Star, "*", None, 1),
                Token::new(EOF, "", None, 1),
            ],
        );
    }

    #[test]
    fn one_or_two_characters_tokens() {
        assert_tokens(
            "!!====>=><=<",
            vec![
                Token::new(Bang, "!", None, 1),
                Token::new(BangEqual, "!=", None, 1),
                Token::new(EqualEqual, "==", None, 1),
                Token::new(Equal, "=", None, 1),
                Token::new(GreaterEqual, ">=", None, 1),
                Token::new(Greater, ">", None, 1),
                Token::new(LessEqual, "<=", None, 1),
                Token::new(Less, "<", None, 1),
                Token::new(EOF, "", None, 1),
            ],
        );
    }

    #[test]
    fn line_comment() {
        assert_tokens("//anything goes here", vec![Token::new(EOF, "", None, 1)]);
        assert_tokens(
            "-// comment body",
            vec![
                Token::new(Minus, "-", None, 1),
                Token::new(EOF, "", None, 1),
            ],
        );
        assert_tokens(
            "-// comment until new line \n+",
            vec![
                Token::new(Minus, "-", None, 1),
                Token::new(Plus, "+", None, 2),
                Token::new(EOF, "", None, 2),
            ],
        );
    }

    #[test]
    fn ignore_white_space() {
        assert_tokens("   \r  \t   ", vec![Token::new(EOF, "", None, 1)]);
        assert_tokens(
            "  \t  \n -  \n",
            vec![
                Token::new(Minus, "-", None, 2),
                Token::new(EOF, "", None, 3),
            ],
        );
    }

    #[test]
    fn unexpected_tokens() {
        assert_tokens_error(
            "@-#\n^+",
            "\
[line 1] Error: Unexpected character.
[line 1] Error: Unexpected character.
[line 2] Error: Unexpected character.
",
            vec![
                Token::new(Minus, "-", None, 1),
                Token::new(Plus, "+", None, 2),
                Token::new(EOF, "", None, 2),
            ],
        );
    }

    #[test]
    fn string_literal() {
        assert_tokens_error(
            "\"unterminated",
            "\
[line 1] Error: Unterminated string.
",
            vec![Token::new(EOF, "", None, 1)],
        );

        assert_tokens(
            "+\"string literal\"*",
            vec![
                Token::new(Plus, "+", None, 1),
                Token::new(String, "\"string literal\"", Some("string literal"), 1),
                Token::new(Star, "*", None, 1),
                Token::new(EOF, "", None, 1),
            ],
        );

        assert_tokens(
            "\"any #@^&\"",
            vec![
                Token::new(String, "\"any #@^&\"", Some("any #@^&"), 1),
                Token::new(EOF, "", None, 1),
            ],
        );

        assert_tokens(
            "\"\
multiline
string
\"",
            vec![
                Token::new(
                    String,
                    "\"multiline\nstring\n\"",
                    Some("multiline\nstring\n"),
                    3,
                ),
                Token::new(EOF, "", None, 3),
            ],
        );
    }
}

use crate::lox::Lox;
use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;
use std::io::Write;

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn keywords(identifier: &str) -> Option<TokenType> {
    use TokenType::*;
    match identifier {
        "and" => Some(And),
        "class" => Some(Class),
        "else" => Some(Else),
        "false" => Some(False),
        "for" => Some(For),
        "fun" => Some(Fun),
        "if" => Some(If),
        "nil" => Some(Nil),
        "or" => Some(Or),
        "print" => Some(Print),
        "return" => Some(Return),
        "super" => Some(Super),
        "this" => Some(This),
        "true" => Some(True),
        "var" => Some(Var),
        "while" => Some(While),
        _ => None,
    }
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
        use TokenType::*;
        while !self.is_at_end() {
            // we are at the beginning of the next lexeme.
            self.start = self.current;
            self.scan_token(lox);
        }
        self.tokens
            .push(Token::new(EOF, "", Literal::Nil, self.line));
        self.tokens.clone()
    }

    fn scan_token<W: Write>(&mut self, lox: &mut Lox<W>) {
        use TokenType::*;
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
            Some(d) if d.is_ascii_digit() => self.number(),
            Some(i) if is_alpha(i) => self.identifier(),
            _ => lox.error(self.line, "Unexpected character."),
        }
    }

    fn identifier(&mut self) {
        while self.peek().map(is_alphanumeric).unwrap_or(false) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let typ = keywords(text).unwrap_or(TokenType::Identifier);
        self.add_token(typ);
    }

    fn number(&mut self) {
        while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.advance();
        }

        if self.peek() == Some('.')
            && self
                .peek_next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
        {
            self.advance();
            while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                self.advance();
            }
        }

        let d = self.source[self.start..self.current].parse().unwrap_or(0.);
        self.add_literal_token(TokenType::Number, Literal::Number(d));
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
        self.add_literal_token(TokenType::String, Literal::String(text))
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
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

    fn add_literal_token(&mut self, typ: TokenType, literal: Literal<'a>) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token::new(typ, text, literal, self.line))
    }

    fn add_token(&mut self, typ: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(typ, text, Literal::Nil, self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    use TokenType::*;

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
        assert_tokens("", vec![Token::new(EOF, "", Literal::Nil, 1)]);
    }

    #[test]
    fn single_character_tokens() {
        assert_tokens(
            "(){},.-+;/*",
            vec![
                Token::new(LeftParen, "(", Literal::Nil, 1),
                Token::new(RightParen, ")", Literal::Nil, 1),
                Token::new(LeftBrace, "{", Literal::Nil, 1),
                Token::new(RightBrace, "}", Literal::Nil, 1),
                Token::new(Comma, ",", Literal::Nil, 1),
                Token::new(Dot, ".", Literal::Nil, 1),
                Token::new(Minus, "-", Literal::Nil, 1),
                Token::new(Plus, "+", Literal::Nil, 1),
                Token::new(Semicolon, ";", Literal::Nil, 1),
                Token::new(Slash, "/", Literal::Nil, 1),
                Token::new(Star, "*", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
    }

    #[test]
    fn one_or_two_characters_tokens() {
        assert_tokens(
            "!!====>=><=<",
            vec![
                Token::new(Bang, "!", Literal::Nil, 1),
                Token::new(BangEqual, "!=", Literal::Nil, 1),
                Token::new(EqualEqual, "==", Literal::Nil, 1),
                Token::new(Equal, "=", Literal::Nil, 1),
                Token::new(GreaterEqual, ">=", Literal::Nil, 1),
                Token::new(Greater, ">", Literal::Nil, 1),
                Token::new(LessEqual, "<=", Literal::Nil, 1),
                Token::new(Less, "<", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
    }

    #[test]
    fn line_comment() {
        assert_tokens(
            "//anything goes here",
            vec![Token::new(EOF, "", Literal::Nil, 1)],
        );
        assert_tokens(
            "-// comment body",
            vec![
                Token::new(Minus, "-", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "-// comment until new line \n+",
            vec![
                Token::new(Minus, "-", Literal::Nil, 1),
                Token::new(Plus, "+", Literal::Nil, 2),
                Token::new(EOF, "", Literal::Nil, 2),
            ],
        );
    }

    #[test]
    fn ignore_white_space() {
        assert_tokens("   \r  \t   ", vec![Token::new(EOF, "", Literal::Nil, 1)]);
        assert_tokens(
            "  \t  \n -  \n",
            vec![
                Token::new(Minus, "-", Literal::Nil, 2),
                Token::new(EOF, "", Literal::Nil, 3),
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
                Token::new(Minus, "-", Literal::Nil, 1),
                Token::new(Plus, "+", Literal::Nil, 2),
                Token::new(EOF, "", Literal::Nil, 2),
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
            vec![Token::new(EOF, "", Literal::Nil, 1)],
        );

        assert_tokens(
            "+\"string literal\"*",
            vec![
                Token::new(Plus, "+", Literal::Nil, 1),
                Token::new(
                    String,
                    "\"string literal\"",
                    Literal::String("string literal"),
                    1,
                ),
                Token::new(Star, "*", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );

        assert_tokens(
            "\"any #@^&\"",
            vec![
                Token::new(String, "\"any #@^&\"", Literal::String("any #@^&"), 1),
                Token::new(EOF, "", Literal::Nil, 1),
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
                    Literal::String("multiline\nstring\n"),
                    3,
                ),
                Token::new(EOF, "", Literal::Nil, 3),
            ],
        );
    }

    #[test]
    fn numbers() {
        assert_tokens(
            "1234",
            vec![
                Token::new(Number, "1234", Literal::Number(1234.), 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "23.45",
            vec![
                Token::new(Number, "23.45", Literal::Number(23.45), 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            ".345",
            vec![
                Token::new(Dot, ".", Literal::Nil, 1),
                Token::new(Number, "345", Literal::Number(345.), 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "4567.",
            vec![
                Token::new(Number, "4567", Literal::Number(4567.), 1),
                Token::new(Dot, ".", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
    }

    #[test]
    fn identifiers() {
        assert_tokens(
            "ab_c",
            vec![
                Token::new(Identifier, "ab_c", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "orand",
            vec![
                Token::new(Identifier, "orand", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "__123",
            vec![
                Token::new(Identifier, "__123", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "A123a",
            vec![
                Token::new(Identifier, "A123a", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
    }

    #[test]
    fn keywords() {
        assert_tokens(
            "and",
            vec![
                Token::new(And, "and", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "class",
            vec![
                Token::new(Class, "class", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "else",
            vec![
                Token::new(Else, "else", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "false",
            vec![
                Token::new(False, "false", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "for",
            vec![
                Token::new(For, "for", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "fun",
            vec![
                Token::new(Fun, "fun", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "if",
            vec![
                Token::new(If, "if", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "nil",
            vec![
                Token::new(Nil, "nil", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "or",
            vec![
                Token::new(Or, "or", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "print",
            vec![
                Token::new(Print, "print", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "return",
            vec![
                Token::new(Return, "return", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "super",
            vec![
                Token::new(Super, "super", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "this",
            vec![
                Token::new(This, "this", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "true",
            vec![
                Token::new(True, "true", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "var",
            vec![
                Token::new(Var, "var", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
        assert_tokens(
            "while",
            vec![
                Token::new(While, "while", Literal::Nil, 1),
                Token::new(EOF, "", Literal::Nil, 1),
            ],
        );
    }
}

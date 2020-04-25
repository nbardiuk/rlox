use TokenType::*;

#[derive(Debug)]
pub struct Token<'s> {
    pub typ: TokenType,
    pub lexeme: &'s str,
    pub line: usize,
}

#[derive(Debug)]
pub struct Scanner<'s> {
    start: &'s str,
    current: &'s str,
    line: usize,
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            start: source,
            current: source,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'s> {
        self.skip_whitespace();

        self.start = self.current;

        match self.advance() {
            Some(c) if is_alpha(c) => self.identifier(),
            Some(c) if c.is_ascii_digit() => self.number(),
            Some('"') => self.string(),
            Some('(') => self.token(LeftParen),
            Some(')') => self.token(RightParen),
            Some('{') => self.token(LeftBrace),
            Some('}') => self.token(RightBrace),
            Some(';') => self.token(Semicolon),
            Some(',') => self.token(Comma),
            Some('.') => self.token(Dot),
            Some('-') => self.token(Minus),
            Some('+') => self.token(Plus),
            Some('/') => self.token(Slash),
            Some('*') => self.token(Star),
            Some('!') => {
                let t = if self.matches('=') { BangEqual } else { Bang };
                self.token(t)
            }
            Some('=') => {
                let t = if self.matches('=') { EqualEqual } else { Equal };
                self.token(t)
            }
            Some('<') => {
                let t = if self.matches('=') { LessEqual } else { Less };
                self.token(t)
            }
            Some('>') => {
                let t = if self.matches('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.token(t)
            }
            Some(_) => self.error("Unexpected character."),
            None => self.token(Eof),
        }
    }

    fn skip_whitespace(&mut self) {
        'whitespace: loop {
            match self.peek() {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                }
                Some('/') => {
                    if let Some('/') = self.peek_next() {
                        'comment: loop {
                            if let Some('\n') | None = self.peek() {
                                break 'comment;
                            }
                            self.advance();
                        }
                    } else {
                        break 'whitespace;
                    }
                }
                _ => {
                    break 'whitespace;
                }
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.current.chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.current.chars().nth(1)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.shift();
        Some(c)
    }

    fn matches(&mut self, c: char) -> bool {
        if let Some(n) = self.peek() {
            if n == c {
                self.shift();
                return true;
            }
        }
        false
    }

    fn shift(&mut self) {
        self.current = &self.current[1..];
    }

    fn identifier(&mut self) -> Token<'s> {
        'id: loop {
            match self.peek() {
                Some(a) if is_alpha(a) || a.is_ascii_digit() => {
                    self.advance();
                }
                _ => {
                    break 'id;
                }
            }
        }
        self.token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let mut chars = self.start.chars();
        match chars.next() {
            Some('a') => self.check_keyword(1, "nd", And),
            Some('c') => self.check_keyword(1, "lass", Class),
            Some('e') => self.check_keyword(1, "lse", Else),
            Some('f') => match chars.next() {
                Some('a') => self.check_keyword(2, "lse", False),
                Some('o') => self.check_keyword(2, "r", For),
                Some('u') => self.check_keyword(2, "n", Fun),
                _ => Identifier,
            },
            Some('i') => self.check_keyword(1, "f", If),
            Some('n') => self.check_keyword(1, "il", Nil),
            Some('o') => self.check_keyword(1, "r", Or),
            Some('p') => self.check_keyword(1, "rint", Print),
            Some('r') => self.check_keyword(1, "eturn", Return),
            Some('s') => self.check_keyword(1, "uper", Super),
            Some('t') => match chars.next() {
                Some('h') => self.check_keyword(2, "is", This),
                Some('r') => self.check_keyword(2, "ue", True),
                _ => Identifier,
            },
            Some('v') => self.check_keyword(1, "ar", Var),
            Some('w') => self.check_keyword(1, "hile", While),
            _ => Identifier,
        }
    }

    fn lexeme_len(&self) -> usize {
        self.start.len() - self.current.len()
    }

    fn check_keyword(&self, start: usize, rest: &str, keyword: TokenType) -> TokenType {
        let keyword_len = start + rest.len();
        if self.lexeme_len() == keyword_len && &self.start[start..keyword_len] == rest {
            keyword
        } else {
            Identifier
        }
    }

    fn number(&mut self) -> Token<'s> {
        'digits: loop {
            match self.peek() {
                Some(d) if d.is_ascii_digit() => {
                    self.advance();
                }
                _ => {
                    break 'digits;
                }
            }
        }

        if self.peek() == Some('.') && self.peek_next().filter(|c| c.is_ascii_digit()).is_some() {
            self.advance();

            'fraction: loop {
                match self.peek() {
                    Some(d) if d.is_ascii_digit() => {
                        self.advance();
                    }
                    _ => {
                        break 'fraction;
                    }
                }
            }
        }

        self.token(Number)
    }

    fn string(&mut self) -> Token<'s> {
        'string: loop {
            if let Some('"') | None = self.peek() {
                break 'string;
            }
            if let Some('\n') = self.peek() {
                self.line += 1;
            }
            self.advance();
        }

        if !self.matches('"') {
            return self.error("Unterminated string.");
        }

        self.token(String)
    }

    fn token(&self, typ: TokenType) -> Token<'s> {
        Token {
            typ,
            line: self.line,
            lexeme: &self.start[..self.lexeme_len()],
        }
    }

    fn error(&self, message: &'s str) -> Token<'s> {
        Token {
            typ: Error,
            line: self.line,
            lexeme: message,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum TokenType {
    And,
    Bang,
    BangEqual,
    Class,
    Comma,
    Dot,
    Eof,
    Error,
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

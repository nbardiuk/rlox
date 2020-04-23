use TokenType::*;

#[derive(Debug)]
pub struct Scanner<'s> {
    start: &'s str,
    current: &'s str,
    line: usize,
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            start: source,
            current: source,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if let Some(c) = self.advance() {
            if c.is_ascii_digit() {
                return self.number();
            }
            match c {
                '"' => self.string(),
                '(' => self.token(LeftParen),
                ')' => self.token(RightParen),
                '{' => self.token(LeftBrace),
                '}' => self.token(RightBrace),
                ';' => self.token(Semicolon),
                ',' => self.token(Comma),
                '.' => self.token(Dot),
                '-' => self.token(Minus),
                '+' => self.token(Plus),
                '/' => self.token(Slash),
                '*' => self.token(Star),
                '!' => {
                    let t = if self.matches('=') { BangEqual } else { Bang };
                    self.token(t)
                }
                '=' => {
                    let t = if self.matches('=') { EqualEqual } else { Equal };
                    self.token(t)
                }
                '<' => {
                    let t = if self.matches('=') { LessEqual } else { Less };
                    self.token(t)
                }
                '>' => {
                    let t = if self.matches('=') {
                        GreaterEqual
                    } else {
                        Greater
                    };
                    self.token(t)
                }
                _ => self.error("Unexpected character."),
            }
        } else {
            self.token(Eof)
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

    fn is_at_end(&self) -> bool {
        self.peek() == None
    }

    fn number(&mut self) -> Token {
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

    fn string(&mut self) -> Token {
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

    fn token(&self, typ: TokenType) -> Token {
        Token {
            typ,
            line: self.line,
            lexeme: &self.start[..self.start.len() - self.current.len()],
        }
    }

    fn error(&self, message: &'s str) -> Token {
        Token {
            typ: Error,
            line: self.line,
            lexeme: message,
        }
    }
}

#[derive(Debug)]
pub struct Token<'s> {
    pub typ: TokenType,
    pub lexeme: &'s str,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
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

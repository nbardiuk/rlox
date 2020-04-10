use crate::expr::Expr;
use crate::lox::Lox;
use crate::token::Token;
use crate::token::TokenType;
use std::io::Write;
use std::rc::Rc;

struct ParserError {}

pub struct Parser<'a, W: Write> {
    tokens: Vec<Token<'a>>,
    current: usize,
    lox: &'a mut Lox<W>,
}

impl<'a, W: Write> Parser<'a, W> {
    pub fn new(lox: &'a mut Lox<W>, tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            current: 0,
            lox,
        }
    }

    pub fn parse(&mut self) -> Option<Expr<'a>> {
        self.expression().ok()
    }

    fn expression(&mut self) -> Result<Expr<'a>, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        let mut expr = self.comparison()?;

        while self.matches(&[BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        let mut expr = self.addition()?;

        while self.matches(&[Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        let mut expr = self.multiplication()?;

        while self.matches(&[Minus, Plus]) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        let mut expr = self.unary()?;

        while self.matches(&[Slash, Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        if self.matches(&[Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Result::Ok(Expr::Unary(operator, Rc::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr<'a>, ParserError> {
        use TokenType::*;
        if self.matches(&[False, True, Nil, Number, String]) {
            return Result::Ok(Expr::Literal(self.previous().literal));
        }
        if self.matches(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, "Expect ')' after expression")?;
            return Result::Ok(Expr::Grouping(Rc::new(expr)));
        }
        self.error(self.peek(), "Expect expression")
    }

    fn _syncronize(&mut self) {
        use TokenType::*;
        self.advance();
        while !self.is_at_end() {
            if self.previous().typ == Semicolon {
                return;
            };
            match self.peek().typ {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn consume(&mut self, typ: TokenType, message: &'a str) -> Result<Token<'a>, ParserError> {
        if self.check(typ) {
            return Result::Ok(self.advance());
        }
        self.error(self.peek(), message)
    }

    fn error<T>(&mut self, token: Token<'a>, message: &'a str) -> Result<T, ParserError> {
        self.lox.error_token(token, message);
        Result::Err(ParserError {})
    }

    fn previous(&self) -> Token<'a> {
        self.tokens[self.current - 1] // TODO use get instead of unchecked indexing
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        for typ in types {
            if self.check(*typ) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, typ: TokenType) -> bool {
        !self.is_at_end() && self.peek().typ == typ
    }

    fn advance(&mut self) -> Token<'a> {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().typ == TokenType::EOF
    }

    fn peek(&self) -> Token<'a> {
        self.tokens[self.current] // TODO use get instead of unchecked indexing
    }
}

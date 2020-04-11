use crate::ast::Expr;
use crate::lox::Lox;
use crate::token::Token;
use crate::token::TokenType;
use std::io::Write;
use std::rc::Rc;

struct ParserError {}

pub struct Parser<'a, W: Write> {
    tokens: Vec<Token>,
    current: usize,
    lox: &'a mut Lox<W>,
}

impl<'a, W: Write> Parser<'a, W> {
    pub fn new(lox: &'a mut Lox<W>, tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            lox,
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        self.expression().ok()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.comparison()?;

        while self.matches(&[BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.addition()?;

        while self.matches(&[Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.multiplication()?;

        while self.matches(&[Minus, Plus]) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.unary()?;

        while self.matches(&[Slash, Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Result::Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        if self.matches(&[Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Result::Ok(Expr::Unary(operator, Rc::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
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

    fn consume(&mut self, typ: TokenType, message: &'a str) -> Result<Token, ParserError> {
        if self.check(typ) {
            return Result::Ok(self.advance());
        }
        self.error(self.peek(), message)
    }

    fn error<T>(&mut self, token: Token, message: &'a str) -> Result<T, ParserError> {
        self.lox.error_token(token, message);
        Result::Err(ParserError {})
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone() // TODO use get instead of unchecked indexing
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

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().typ == TokenType::EOF
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone() // TODO use get instead of unchecked indexing
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    use crate::scanner::Scanner;

    fn tree<'a>(source: &'a str) -> String {
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        let tree = parser.parse().map(|e| e.to_string()).unwrap_or_default();
        format!("{}{}", lox.output(), tree)
    }

    #[test]
    fn not_expression() {
        assert_eq!(
            tree("anything\nnot valid"),
            "[line 1] Error at \'anything\': Expect expression\n"
        );
    }

    #[test]
    fn primary() {
        assert_eq!(tree("1321.31"), "1321.31");
        assert_eq!(tree("\"asdf 123\""), "\"asdf 123\"");
        assert_eq!(tree("true"), "true");
        assert_eq!(tree("false"), "false");
        assert_eq!(tree("nil"), "nil");
    }

    #[test]
    fn uniary_bang() {
        assert_eq!(tree("!true"), "(! true)");
        assert_eq!(tree("!!false"), "(! (! false))");
    }

    #[test]
    fn unary_minus() {
        assert_eq!(tree("-1"), "(- 1)");
        assert_eq!(tree("--1"), "(- (- 1))");
    }

    #[test]
    fn multiplication_slash() {
        assert_eq!(tree("2/-3"), "(/ 2 (- 3))");
        assert_eq!(tree("-4/2"), "(/ (- 4) 2)");
        assert_eq!(tree("1/2/3"), "(/ (/ 1 2) 3)");
        assert_eq!(tree("1/"), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("/1"), "[line 1] Error at \'/\': Expect expression\n");
    }

    #[test]
    fn multiplication_star() {
        assert_eq!(tree("2*-3"), "(* 2 (- 3))");
        assert_eq!(tree("-4*2"), "(* (- 4) 2)");
        assert_eq!(tree("1*2*3"), "(* (* 1 2) 3)");
        assert_eq!(tree("1*"), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("*1"), "[line 1] Error at \'*\': Expect expression\n");
    }

    #[test]
    fn multiplication() {
        assert_eq!(tree("1*2/3*4/5"), "(/ (* (/ (* 1 2) 3) 4) 5)");
    }

    #[test]
    fn addition_plus() {
        assert_eq!(tree("1+-2"), "(+ 1 (- 2))");
        assert_eq!(tree("-1+2"), "(+ (- 1) 2)");
        assert_eq!(tree("1+2+3"), "(+ (+ 1 2) 3)");
        assert_eq!(tree("1*2 + 3*4"), "(+ (* 1 2) (* 3 4))");
        assert_eq!(tree("1 + 2/3 + 4"), "(+ (+ 1 (/ 2 3)) 4)");
        assert_eq!(tree("1+"), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("+1"), "[line 1] Error at \'+\': Expect expression\n");
    }

    #[test]
    fn addition_minus() {
        assert_eq!(tree("1--2"), "(- 1 (- 2))");
        assert_eq!(tree("-1-2"), "(- (- 1) 2)");
        assert_eq!(tree("1-2-3"), "(- (- 1 2) 3)");
        assert_eq!(tree("1*2 - 3*4"), "(- (* 1 2) (* 3 4))");
        assert_eq!(tree("1 - 2/3 - 4"), "(- (- 1 (/ 2 3)) 4)");
        assert_eq!(tree("1-"), "[line 1] Error at end: Expect expression\n");
    }

    #[test]
    fn comparison_greater() {
        assert_eq!(tree("1>2"), "(> 1 2)");
        assert_eq!(tree("1>2>3"), "(> (> 1 2) 3)");
        assert_eq!(tree("1+2>3*4"), "(> (+ 1 2) (* 3 4))");
        assert_eq!(tree("1>"), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree(">1"), "[line 1] Error at \'>\': Expect expression\n");
    }

    #[test]
    fn comparison_greater_eq() {
        assert_eq!(tree("1>=2"), "(>= 1 2)");
        assert_eq!(tree("1>=2>=3"), "(>= (>= 1 2) 3)");
        assert_eq!(tree("1+2>=3*4"), "(>= (+ 1 2) (* 3 4))");
        assert_eq!(tree("1>="), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree(">=1"), "[line 1] Error at \'>=\': Expect expression\n");
    }

    #[test]
    fn comparison_less() {
        assert_eq!(tree("1<2"), "(< 1 2)");
        assert_eq!(tree("1<2<3"), "(< (< 1 2) 3)");
        assert_eq!(tree("1+2<3*4"), "(< (+ 1 2) (* 3 4))");
        assert_eq!(tree("1<"), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("<1"), "[line 1] Error at \'<\': Expect expression\n");
    }

    #[test]
    fn comparison_less_eq() {
        assert_eq!(tree("1<=2"), "(<= 1 2)");
        assert_eq!(tree("1<=2<=3"), "(<= (<= 1 2) 3)");
        assert_eq!(tree("1+2<=3*4"), "(<= (+ 1 2) (* 3 4))");
        assert_eq!(tree("1<="), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("<=1"), "[line 1] Error at \'<=\': Expect expression\n");
    }

    #[test]
    fn equality_eq() {
        assert_eq!(tree("1==2"), "(== 1 2)");
        assert_eq!(tree("1==2==3"), "(== (== 1 2) 3)");
        assert_eq!(tree("1+2==3<=4==5*6"), "(== (== (+ 1 2) (<= 3 4)) (* 5 6))");
        assert_eq!(tree("1=="), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("==1"), "[line 1] Error at \'==\': Expect expression\n");
    }

    #[test]
    fn equality_not_eq() {
        assert_eq!(tree("1!=2"), "(!= 1 2)");
        assert_eq!(tree("1!=2!=3"), "(!= (!= 1 2) 3)");
        assert_eq!(tree("1+2!=3<=4!=5*6"), "(!= (!= (+ 1 2) (<= 3 4)) (* 5 6))");
        assert_eq!(tree("1!="), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree("!=1"), "[line 1] Error at \'!=\': Expect expression\n");
    }

    #[test]
    fn grouping() {
        assert_eq!(tree("(1)"), "(group 1)");
        assert_eq!(tree("1 + (2 + 3)"), "(+ 1 (group (+ 2 3)))");
        assert_eq!(tree("1 / (2 - 3)"), "(/ 1 (group (- 2 3)))");
        assert_eq!(tree("-(1 - 2)"), "(- (group (- 1 2)))");
        assert_eq!(tree("!(1 >= 2)"), "(! (group (>= 1 2)))");
        assert_eq!(tree("("), "[line 1] Error at end: Expect expression\n");
        assert_eq!(tree(")"), "[line 1] Error at \')\': Expect expression\n");
        assert_eq!(tree("1)"), "1"); // TODO unmatched closing paren is ignored
        assert_eq!(
            tree("(1"),
            "[line 1] Error at end: Expect \')\' after expression\n"
        );
    }
}

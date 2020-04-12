use crate::ast::Expr;
use crate::ast::Stmt;
use crate::lox::Lox;
use crate::token::Token;
use crate::token::TokenType;
use std::io::Write;
use std::rc::Rc;

#[derive(Debug)]
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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(statement) = self.declaration() {
                statements.push(statement);
            }
        }
        statements
    }

    fn declaration(&mut self) -> Option<Stmt> {
        use TokenType::*;
        let statement = if self.matches(&[Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if statement.is_err() {
            self.syncronize();
        }

        statement.ok()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let name = self.consume(Identifier, "Expect variable name.")?;

        let initializer = if self.matches(&[Equal]) {
            Some(Rc::new(self.expression()?))
        } else {
            None
        };

        self.consume(Semicolon, "Expect ';' after variable declaration")?;

        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        if self.matches(&[If]) {
            self.if_statement()
        } else if self.matches(&[Print]) {
            self.print_statement()
        } else if self.matches(&[LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;

        self.consume(LeftParen, "Expect '(' after if.")?;
        let condition = Rc::new(self.expression()?);
        self.consume(RightParen, "Expect ')' after if.")?;

        let then = Rc::new(self.statement()?);

        let r#else = if self.matches(&[Else]) {
            Some(Rc::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then, r#else))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        use TokenType::*;

        let mut statements = vec![];
        while !self.check(RightBrace) && !self.is_at_end() {
            if let Some(statement) = self.declaration() {
                statements.push(statement);
            }
        }

        self.consume(RightBrace, "Expect '}' after block")?;

        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let value = self.expression()?;
        self.consume(Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(Rc::new(value)))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let value = self.expression()?;
        self.consume(Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expression(Rc::new(value)))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let expr = self.equality()?;

        if self.matches(&[Equal]) {
            if let Expr::Variable(name) = expr {
                Ok(Expr::Asign(name, Rc::new(self.assignment()?)))
            } else {
                self.error(self.previous(), "Invalid assignment target.")
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.comparison()?;

        while self.matches(&[BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.addition()?;

        while self.matches(&[Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.multiplication()?;

        while self.matches(&[Minus, Plus]) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.unary()?;

        while self.matches(&[Slash, Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        if self.matches(&[Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Rc::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        if self.matches(&[False, True, Nil, Number, String]) {
            return Ok(Expr::Literal(self.previous().literal));
        }
        if self.matches(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, "Expect ')' after expression")?;
            return Ok(Expr::Grouping(Rc::new(expr)));
        }
        if self.matches(&[Identifier]) {
            return Ok(Expr::Variable(self.previous()));
        }
        self.error(self.peek(), "Expect expression")
    }

    fn syncronize(&mut self) {
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
            return Ok(self.advance());
        }
        self.error(self.peek(), message)
    }

    fn error<T>(&mut self, token: Token, message: &'a str) -> Result<T, ParserError> {
        self.lox.error_token(token, message);
        Err(ParserError {})
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

    fn parse<'a>(source: &'a str) -> Vec<String> {
        use crate::scanner::Scanner;
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);

        let mut parser = Parser::new(&mut lox, tokens);
        let mut tree = parser
            .parse()
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>();

        let mut output = lox
            .output()
            .lines()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();

        output.append(&mut tree);
        output
    }

    #[test]
    fn not_expression() {
        assert_eq!(
            parse("anything\nnot valid"),
            vec!["[line 2] Error at \'not\': Expect \';\' after value."]
        );
    }

    #[test]
    fn primary() {
        assert_eq!(parse("1321.31;"), vec!["(expr 1321.31)"]);
        assert_eq!(parse("\"asdf 123\";"), vec!["(expr \"asdf 123\")"]);
        assert_eq!(parse("true;"), vec!["(expr true)"]);
        assert_eq!(parse("false;"), vec!["(expr false)"]);
        assert_eq!(parse("nil;"), vec!["(expr nil)"]);
        assert_eq!(parse("variable_name;"), vec!["(expr variable_name)"]);
    }

    #[test]
    fn uniary_bang() {
        assert_eq!(parse("!true;"), vec!["(expr (! true))"]);
        assert_eq!(parse("!!false;"), vec!["(expr (! (! false)))"]);
    }

    #[test]
    fn unary_minus() {
        assert_eq!(parse("-1;"), vec!["(expr (- 1))"]);
        assert_eq!(parse("--1;"), vec!["(expr (- (- 1)))"]);
    }

    #[test]
    fn multiplication_slash() {
        assert_eq!(parse("2/-3;"), vec!["(expr (/ 2 (- 3)))"]);
        assert_eq!(parse("-4/2;"), vec!["(expr (/ (- 4) 2))"]);
        assert_eq!(parse("1/2/3;"), vec!["(expr (/ (/ 1 2) 3))"]);
        assert_eq!(
            parse("1/"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("/1"),
            vec!["[line 1] Error at \'/\': Expect expression"]
        );
    }

    #[test]
    fn multiplication_star() {
        assert_eq!(parse("2*-3;"), vec!["(expr (* 2 (- 3)))"]);
        assert_eq!(parse("-4*2;"), vec!["(expr (* (- 4) 2))"]);
        assert_eq!(parse("1*2*3;"), vec!["(expr (* (* 1 2) 3))"]);
        assert_eq!(
            parse("1*"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("*1"),
            vec!["[line 1] Error at \'*\': Expect expression"]
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            parse("1*2/3*4/5;"),
            vec!["(expr (/ (* (/ (* 1 2) 3) 4) 5))"]
        );
    }

    #[test]
    fn addition_plus() {
        assert_eq!(parse("1+-2;"), vec!["(expr (+ 1 (- 2)))"]);
        assert_eq!(parse("-1+2;"), vec!["(expr (+ (- 1) 2))"]);
        assert_eq!(parse("1+2+3;"), vec!["(expr (+ (+ 1 2) 3))"]);
        assert_eq!(parse("1*2 + 3*4;"), vec!["(expr (+ (* 1 2) (* 3 4)))"]);
        assert_eq!(parse("1 + 2/3 + 4;"), vec!["(expr (+ (+ 1 (/ 2 3)) 4))"]);
        assert_eq!(
            parse("1+"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("+1"),
            vec!["[line 1] Error at \'+\': Expect expression"]
        );
    }

    #[test]
    fn addition_minus() {
        assert_eq!(parse("1--2;"), vec!["(expr (- 1 (- 2)))"]);
        assert_eq!(parse("-1-2;"), vec!["(expr (- (- 1) 2))"]);
        assert_eq!(parse("1-2-3;"), vec!["(expr (- (- 1 2) 3))"]);
        assert_eq!(parse("1*2 - 3*4;"), vec!["(expr (- (* 1 2) (* 3 4)))"]);
        assert_eq!(parse("1 - 2/3 - 4;"), vec!["(expr (- (- 1 (/ 2 3)) 4))"]);
        assert_eq!(
            parse("1-"),
            vec!["[line 1] Error at end: Expect expression"]
        );
    }

    #[test]
    fn comparison_greater() {
        assert_eq!(parse("1>2;"), vec!["(expr (> 1 2))"]);
        assert_eq!(parse("1>2>3;"), vec!["(expr (> (> 1 2) 3))"]);
        assert_eq!(parse("1+2>3*4;"), vec!["(expr (> (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1>"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse(">1"),
            vec!["[line 1] Error at \'>\': Expect expression"]
        );
    }

    #[test]
    fn comparison_greater_eq() {
        assert_eq!(parse("1>=2;"), vec!["(expr (>= 1 2))"]);
        assert_eq!(parse("1>=2>=3;"), vec!["(expr (>= (>= 1 2) 3))"]);
        assert_eq!(parse("1+2>=3*4;"), vec!["(expr (>= (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1>="),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse(">=1"),
            vec!["[line 1] Error at \'>=\': Expect expression"]
        );
    }

    #[test]
    fn comparison_less() {
        assert_eq!(parse("1<2;"), vec!["(expr (< 1 2))"]);
        assert_eq!(parse("1<2<3;"), vec!["(expr (< (< 1 2) 3))"]);
        assert_eq!(parse("1+2<3*4;"), vec!["(expr (< (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1<"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("<1"),
            vec!["[line 1] Error at \'<\': Expect expression"]
        );
    }

    #[test]
    fn comparison_less_eq() {
        assert_eq!(parse("1<=2;"), vec!["(expr (<= 1 2))"]);
        assert_eq!(parse("1<=2<=3;"), vec!["(expr (<= (<= 1 2) 3))"]);
        assert_eq!(parse("1+2<=3*4;"), vec!["(expr (<= (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1<="),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("<=1"),
            vec!["[line 1] Error at \'<=\': Expect expression"]
        );
    }

    #[test]
    fn equality_eq() {
        assert_eq!(parse("1==2;"), vec!["(expr (== 1 2))"]);
        assert_eq!(parse("1==2==3;"), vec!["(expr (== (== 1 2) 3))"]);
        assert_eq!(
            parse("1+2==3<=4==5*6;"),
            vec!["(expr (== (== (+ 1 2) (<= 3 4)) (* 5 6)))"]
        );
        assert_eq!(
            parse("1=="),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("==1"),
            vec!["[line 1] Error at \'==\': Expect expression"]
        );
    }

    #[test]
    fn equality_not_eq() {
        assert_eq!(parse("1!=2;"), vec!["(expr (!= 1 2))"]);
        assert_eq!(parse("1!=2!=3;"), vec!["(expr (!= (!= 1 2) 3))"]);
        assert_eq!(
            parse("1+2!=3<=4!=5*6;"),
            vec!["(expr (!= (!= (+ 1 2) (<= 3 4)) (* 5 6)))"]
        );
        assert_eq!(
            parse("1!="),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("!=1"),
            vec!["[line 1] Error at \'!=\': Expect expression"]
        );
    }

    #[test]
    fn grouping() {
        assert_eq!(parse("(1);"), vec!["(expr (group 1))"]);
        assert_eq!(parse("1 + (2 + 3);"), vec!["(expr (+ 1 (group (+ 2 3))))"]);
        assert_eq!(parse("1 / (2 - 3);"), vec!["(expr (/ 1 (group (- 2 3))))"]);
        assert_eq!(parse("-(1 - 2);"), vec!["(expr (- (group (- 1 2))))"]);
        assert_eq!(parse("!(1 >= 2);"), vec!["(expr (! (group (>= 1 2))))"]);
        assert_eq!(parse("("), vec!["[line 1] Error at end: Expect expression"]);
        assert_eq!(
            parse(")"),
            vec!["[line 1] Error at \')\': Expect expression"]
        );
        assert_eq!(
            parse("1)"),
            vec!["[line 1] Error at \')\': Expect \';\' after value."]
        );
        assert_eq!(
            parse("(1"),
            vec!["[line 1] Error at end: Expect \')\' after expression"]
        );
    }

    #[test]
    fn expression() {
        assert_eq!(parse("1!=2;"), vec!["(expr (!= 1 2))"]);
        assert_eq!(
            parse("1+1;\n2-3;"),
            vec!["(expr (+ 1 1))", "(expr (- 2 3))"]
        );
        assert_eq!(
            parse("1"),
            vec!["[line 1] Error at end: Expect \';\' after value."]
        );
        assert_eq!(
            parse("1;2"),
            vec![
                "[line 1] Error at end: Expect \';\' after value.",
                "(expr 1)"
            ]
        );
    }

    #[test]
    fn print() {
        assert_eq!(parse("print 1!=2;"), vec!["(print (!= 1 2))"]);
        assert_eq!(
            parse("print 1;print 2-3;"),
            vec!["(print 1)", "(print (- 2 3))"]
        );
        assert_eq!(
            parse("print"),
            vec!["[line 1] Error at end: Expect expression"]
        );
        assert_eq!(
            parse("print 1;2"),
            vec![
                "[line 1] Error at end: Expect \';\' after value.",
                "(print 1)"
            ]
        );
    }

    #[test]
    fn declaration_var() {
        assert_eq!(
            parse("var a = 1; var b = 1 > 2; var n;"),
            vec!["(def a 1)", "(def b (> 1 2))", "(def n)"]
        );
        assert_eq!(
            parse(
                "var snake_case = true;
                var lisp-case = false;
                var camelCase = true;
                var 0name = false;
                var _0name = true;
                "
            ),
            vec![
                "[line 2] Error at \'-\': Expect \';\' after variable declaration",
                "[line 4] Error at \'0\': Expect variable name.",
                "(def snake_case true)",
                "(def camelCase true)",
                "(def _0name true)"
            ]
        );
    }

    #[test]
    fn assignmen_var() {
        assert_eq!(
            parse("a=1; b=1>2; a=a;"),
            vec![
                "(expr (set! a 1))",
                "(expr (set! b (> 1 2)))",
                "(expr (set! a a))"
            ]
        );
        assert_eq!(parse("a = b = c;"), vec!["(expr (set! a (set! b c)))"]);
        assert_eq!(
            parse(
                "a + (b = 1);
                 (a = 1) + b;"
            ),
            vec![
                "(expr (+ a (group (set! b 1))))",
                "(expr (+ (group (set! a 1)) b))"
            ]
        );
        assert_eq!(
            parse(
                "1 = a;
                 \"a\" = 1;
                 a = 1 = b;
                "
            ),
            vec![
                "[line 1] Error at \'=\': Invalid assignment target.",
                "[line 2] Error at \'=\': Invalid assignment target.",
                "[line 3] Error at \'=\': Invalid assignment target."
            ]
        );
    }

    #[test]
    fn blocks() {
        assert_eq!(parse("{}{}{{}}"), vec!["(do )", "(do )", "(do (do ))"]);
        assert_eq!(
            parse("{var a=1;print a;}{a=2;}"),
            vec!["(do (def a 1) (print a))", "(do (expr (set! a 2)))"]
        );
        assert_eq!(
            parse("a = {b = 1};"),
            vec!["[line 1] Error at \'{\': Expect expression"]
        );
        assert_eq!(
            parse("{\n{"),
            vec![
                "[line 2] Error at end: Expect \'}\' after block",
                "[line 2] Error at end: Expect \'}\' after block"
            ]
        );
    }

    #[test]
    fn ifs() {
        assert_eq!(
            parse("if (condition) print when_true; else print when_false;"),
            vec!["(if condition (print when_true) (print when_false))"]
        );
        assert_eq!(
            parse("if (a) if (b) 1; else 2;"),
            vec!["(if a (if b (expr 1) (expr 2)))"]
        );
        assert_eq!(
            parse("if (a) { if (b) when_true; } else when_false;"),
            vec!["(if a (do (if b (expr when_true))) (expr when_false))"]
        );
        assert_eq!(
            parse("if a 1; else 2;"),
            vec![
                "[line 1] Error at \'a\': Expect \'(\' after if.",
                "[line 1] Error at \'else\': Expect expression"
            ]
        );
        assert_eq!(
            parse("if (a 1; else 2;"),
            vec![
                "[line 1] Error at \'1\': Expect \')\' after if.",
                "[line 1] Error at \'else\': Expect expression"
            ]
        );
        assert_eq!(
            parse("if (a); 1; else 2;"),
            vec![
                "[line 1] Error at \';\': Expect expression",
                "[line 1] Error at \'else\': Expect expression",
                "(expr 1)"
            ]
        );
    }
}

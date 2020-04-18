use crate::ast::Expr;
use crate::ast::Stmt;
use crate::lox::Lox;
use crate::token::Token;
use crate::token::TokenType;
use std::io::Write;
use std::rc::Rc;

const MAX_ARGS: usize = 255;

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
        let statement = if self.matches(&[Class]) {
            self.class_declaration()
        } else if self.matches(&[Fun]) {
            self.function("function")
        } else if self.matches(&[Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if statement.is_err() {
            self.syncronize();
        }

        statement.ok()
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let name = self.consume(Identifier, &format!("Expect {} name.", kind))?;

        self.consume(LeftParen, &format!("Expect '(' after {} name.", kind))?;
        let mut parameters = vec![];
        if !self.check(RightParen) {
            loop {
                if parameters.len() >= MAX_ARGS {
                    return self.error(
                        self.peek(),
                        &format!("Cannot have more than {} parameters.", MAX_ARGS),
                    );
                }
                parameters.push(self.consume(Identifier, "Expect parameter name.")?);
                if !self.matches(&[Comma]) {
                    break;
                }
            }
        }
        self.consume(RightParen, "Expect ')' after parameters.")?;

        self.consume(LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;

        Ok(Stmt::Function(name, parameters, body))
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let name = self.consume(Identifier, "Expect class name.")?;

        self.consume(LeftBrace, "Expect '{' before class body.")?;

        let mut methods = vec![];
        while !self.check(RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }
        self.consume(RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class(name, methods))
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
        if self.matches(&[For]) {
            self.for_statement()
        } else if self.matches(&[If]) {
            self.if_statement()
        } else if self.matches(&[Print]) {
            self.print_statement()
        } else if self.matches(&[Return]) {
            self.return_statement()
        } else if self.matches(&[While]) {
            self.while_statement()
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
        self.consume(RightParen, "Expect ')' after condition.")?;

        let then = Rc::new(self.statement()?);

        let r#else = if self.matches(&[Else]) {
            Some(Rc::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then, r#else))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;

        self.consume(LeftParen, "Expect '(' after for.")?;

        let initializer = if self.matches(&[Semicolon]) {
            None
        } else if self.matches(&[Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(crate::token::Literal::Bool(true))
        };
        self.consume(Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(RightParen) {
            Some(Rc::new(self.expression()?))
        } else {
            None
        };
        self.consume(RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        if let Some(inc) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(inc)]);
        }
        body = Stmt::While(Rc::new(condition), Rc::new(body));
        if let Some(init) = initializer {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;

        self.consume(LeftParen, "Expect '(' after while.")?;
        let condition = Rc::new(self.expression()?);
        self.consume(RightParen, "Expect ')' after condition.")?;

        let body = Rc::new(self.statement()?);

        Ok(Stmt::While(condition, body))
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

    fn return_statement(&mut self) -> Result<Stmt, ParserError> {
        use TokenType::*;
        let keyword = self.previous();
        let value = if !self.check(Semicolon) {
            Some(Rc::new(self.expression()?))
        } else {
            None
        };
        self.consume(Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return(keyword, value))
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
        let expr = self.or()?;

        if self.matches(&[Equal]) {
            if let Expr::Variable(name) = expr {
                Ok(Expr::Asign(name, Rc::new(self.assignment()?)))
            } else if let Expr::Get(object, name) = expr {
                Ok(Expr::Set(object, name, Rc::new(self.assignment()?)))
            } else {
                self.error(self.previous(), "Invalid assignment target.")
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.and()?;

        while self.matches(&[Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut expr = self.equality()?;

        while self.matches(&[And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
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
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;

        let mut expr = self.primary()?;
        loop {
            if self.matches(&[LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(&[Dot]) {
                let name = self.consume(Identifier, "Expect property name after '.'.")?;
                expr = Expr::Get(Rc::new(expr), name);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        use TokenType::*;
        let mut args = vec![];
        if !self.check(RightParen) {
            loop {
                if args.len() >= MAX_ARGS {
                    return self.error(
                        self.peek(),
                        &format!("Cannot have more than {} arguments.", MAX_ARGS),
                    );
                }
                args.push(self.expression()?);
                if !self.matches(&[Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::Call(Rc::new(callee), paren, args))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        use TokenType::*;
        if self.matches(&[False, True, Nil, Number, String]) {
            return Ok(Expr::Literal(self.previous().literal));
        }
        if self.matches(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Rc::new(expr)));
        }
        if self.matches(&[This]) {
            return Ok(Expr::This(self.previous()));
        }
        if self.matches(&[Identifier]) {
            return Ok(Expr::Variable(self.previous()));
        }
        self.error(self.peek(), "Expect expression.")
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

    fn consume(&mut self, typ: TokenType, message: &str) -> Result<Token, ParserError> {
        if self.check(typ) {
            return Ok(self.advance());
        }
        self.error(self.peek(), message)
    }

    fn error<T>(&mut self, token: Token, message: &str) -> Result<T, ParserError> {
        self.lox.error_token(&token, message);
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
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("/1"),
            vec!["[line 1] Error at \'/\': Expect expression."]
        );
    }

    #[test]
    fn multiplication_star() {
        assert_eq!(parse("2*-3;"), vec!["(expr (* 2 (- 3)))"]);
        assert_eq!(parse("-4*2;"), vec!["(expr (* (- 4) 2))"]);
        assert_eq!(parse("1*2*3;"), vec!["(expr (* (* 1 2) 3))"]);
        assert_eq!(
            parse("1*"),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("*1"),
            vec!["[line 1] Error at \'*\': Expect expression."]
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
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("+1"),
            vec!["[line 1] Error at \'+\': Expect expression."]
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
            vec!["[line 1] Error at end: Expect expression."]
        );
    }

    #[test]
    fn comparison_greater() {
        assert_eq!(parse("1>2;"), vec!["(expr (> 1 2))"]);
        assert_eq!(parse("1>2>3;"), vec!["(expr (> (> 1 2) 3))"]);
        assert_eq!(parse("1+2>3*4;"), vec!["(expr (> (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1>"),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse(">1"),
            vec!["[line 1] Error at \'>\': Expect expression."]
        );
    }

    #[test]
    fn comparison_greater_eq() {
        assert_eq!(parse("1>=2;"), vec!["(expr (>= 1 2))"]);
        assert_eq!(parse("1>=2>=3;"), vec!["(expr (>= (>= 1 2) 3))"]);
        assert_eq!(parse("1+2>=3*4;"), vec!["(expr (>= (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1>="),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse(">=1"),
            vec!["[line 1] Error at \'>=\': Expect expression."]
        );
    }

    #[test]
    fn comparison_less() {
        assert_eq!(parse("1<2;"), vec!["(expr (< 1 2))"]);
        assert_eq!(parse("1<2<3;"), vec!["(expr (< (< 1 2) 3))"]);
        assert_eq!(parse("1+2<3*4;"), vec!["(expr (< (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1<"),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("<1"),
            vec!["[line 1] Error at \'<\': Expect expression."]
        );
    }

    #[test]
    fn comparison_less_eq() {
        assert_eq!(parse("1<=2;"), vec!["(expr (<= 1 2))"]);
        assert_eq!(parse("1<=2<=3;"), vec!["(expr (<= (<= 1 2) 3))"]);
        assert_eq!(parse("1+2<=3*4;"), vec!["(expr (<= (+ 1 2) (* 3 4)))"]);
        assert_eq!(
            parse("1<="),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("<=1"),
            vec!["[line 1] Error at \'<=\': Expect expression."]
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
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("==1"),
            vec!["[line 1] Error at \'==\': Expect expression."]
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
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("!=1"),
            vec!["[line 1] Error at \'!=\': Expect expression."]
        );
    }

    #[test]
    fn grouping() {
        assert_eq!(parse("(1);"), vec!["(expr (group 1))"]);
        assert_eq!(parse("1 + (2 + 3);"), vec!["(expr (+ 1 (group (+ 2 3))))"]);
        assert_eq!(parse("1 / (2 - 3);"), vec!["(expr (/ 1 (group (- 2 3))))"]);
        assert_eq!(parse("-(1 - 2);"), vec!["(expr (- (group (- 1 2))))"]);
        assert_eq!(parse("!(1 >= 2);"), vec!["(expr (! (group (>= 1 2))))"]);
        assert_eq!(
            parse("("),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse(")"),
            vec!["[line 1] Error at \')\': Expect expression."]
        );
        assert_eq!(
            parse("1)"),
            vec!["[line 1] Error at \')\': Expect \';\' after value."]
        );
        assert_eq!(
            parse("(1"),
            vec!["[line 1] Error at end: Expect \')\' after expression."]
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
            vec!["[line 1] Error at end: Expect expression."]
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
            vec!["[line 1] Error at \'{\': Expect expression."]
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
                "[line 1] Error at \'else\': Expect expression."
            ]
        );
        assert_eq!(
            parse("if (a 1; else 2;"),
            vec![
                "[line 1] Error at \'1\': Expect \')\' after condition.",
                "[line 1] Error at \'else\': Expect expression."
            ]
        );
        assert_eq!(
            parse("if (a); 1; else 2;"),
            vec![
                "[line 1] Error at \';\': Expect expression.",
                "[line 1] Error at \'else\': Expect expression.",
                "(expr 1)"
            ]
        );
    }

    #[test]
    fn logical() {
        assert_eq!(parse("1 or 2 or 3;"), vec!["(expr (or (or 1 2) 3))"]);
        assert_eq!(parse("1 and 2 and 3;"), vec!["(expr (and (and 1 2) 3))"]);
        assert_eq!(
            parse("1 or 2 and 3 or 4;"),
            vec!["(expr (or (or 1 (and 2 3)) 4))"]
        );
        assert_eq!(
            parse("1 and 2 or 3 and 4;"),
            vec!["(expr (or (and 1 2) (and 3 4)))"]
        );
        assert_eq!(
            parse("1 > 2 or 3 < 4;"),
            vec!["(expr (or (> 1 2) (< 3 4)))"]
        );
        assert_eq!(
            parse("1 > 2 and 3 < 4;"),
            vec!["(expr (and (> 1 2) (< 3 4)))"]
        );
    }

    #[test]
    fn whiles() {
        assert_eq!(parse("while (true) {}"), vec!["(while true (do ))"]);
        assert_eq!(
            parse("while (a or b) print c;"),
            vec!["(while (or a b) (print c))"]
        );
        assert_eq!(
            parse("while true {}"),
            vec!["[line 1] Error at \'true\': Expect \'(\' after while."]
        );
        assert_eq!(
            parse("while (true {}"),
            vec!["[line 1] Error at \'{\': Expect \')\' after condition."]
        );
        assert_eq!(
            parse("while (print 1;) {}"),
            vec![
                "[line 1] Error at \'print\': Expect expression.",
                "[line 1] Error at \')\': Expect expression."
            ]
        );
    }

    #[test]
    fn fors() {
        assert_eq!(parse("for (;;) {}"), vec!["(while true (do ))"]);
        assert_eq!(
            parse("for (var i=1;;) {}"),
            vec![
                "(do (def i 1) \
                     (while true (do )))"
            ]
        );
        assert_eq!(parse("for (;a<b;) {}"), vec!["(while (< a b) (do ))"]);
        assert_eq!(
            parse("for (;;c=c+1) {}"),
            vec![
                "(while true \
                        (do (do ) \
                            (expr (set! c (+ c 1)))))"
            ]
        );
        assert_eq!(
            parse("for (var i=0;i<10;i=i+1) print i;"),
            vec![
                "(do (def i 0) \
                     (while (< i 10) \
                            (do (print i) \
                                (expr (set! i (+ i 1))))))"
            ]
        );
        assert_eq!(
            parse("for () {}"),
            vec!["[line 1] Error at \')\': Expect expression."]
        );
        assert_eq!(
            parse("for (1) {}"),
            vec!["[line 1] Error at \')\': Expect \';\' after value."]
        );
        assert_eq!(
            parse("for (1;) {}"),
            vec!["[line 1] Error at \')\': Expect expression."]
        );
        assert_eq!(
            parse("for (1;2) {}"),
            vec!["[line 1] Error at \')\': Expect \';\' after loop condition."]
        );
        assert_eq!(
            parse("for (1;2;3;) {}"),
            vec![
                "[line 1] Error at \';\': Expect \')\' after for clauses.",
                "[line 1] Error at \')\': Expect expression."
            ]
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(parse("a();"), vec!["(expr (a ))"]);
        assert_eq!(parse("a(1,2,3,4);"), vec!["(expr (a 1 2 3 4))"]);
        assert_eq!(parse("print a(1,2);"), vec!["(print (a 1 2))"]);
        assert_eq!(parse("a(1)(2);"), vec!["(expr ((a 1) 2))"]);
        assert_eq!(parse("\"a\"(1);"), vec!["(expr (\"a\" 1))"]);
        assert_eq!(parse("1(a);"), vec!["(expr (1 a))"]);
        assert_eq!(parse("i=1(a);"), vec!["(expr (set! i (1 a)))"]);
        assert_eq!(parse("(i=1)(a);"), vec!["(expr ((group (set! i 1)) a))"]);
        assert_eq!(
            parse("a(1,);"),
            vec!["[line 1] Error at \')\': Expect expression."]
        );
        assert_eq!(
            parse("a(,1);"),
            vec!["[line 1] Error at \',\': Expect expression."]
        );
        assert_eq!(parse("a(
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1);"), vec!["(expr (a \
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \
        1 1 1 1 1))"]);
        assert_eq!(parse("a(
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1
            );"), vec!["[line 7] Error at \'1\': Cannot have more than 255 arguments."]);
    }

    #[test]
    fn function_declaration() {
        assert_eq!(parse("fun a(){}"), vec!["(defn a () )"]);
        assert_eq!(parse("fun a(a,b,c){}"), vec!["(defn a (a b c) )"]);
        assert_eq!(
            parse("fun a(){a; b;}"),
            vec!["(defn a () (expr a) (expr b))"]
        );
        assert_eq!(
            parse("fun a(1){}"),
            vec!["[line 1] Error at \'1\': Expect parameter name."]
        );
        assert_eq!(
            parse("fun a(a b){}"),
            vec!["[line 1] Error at \'b\': Expect \')\' after parameters."]
        );
        assert_eq!(
            parse("fun (){}"),
            vec!["[line 1] Error at \'(\': Expect function name."]
        );
        assert_eq!(
            parse("fun a{}"),
            vec!["[line 1] Error at \'{\': Expect \'(\' after function name."]
        );
        assert_eq!(
            parse("fun a();"),
            vec!["[line 1] Error at \';\': Expect \'{\' before function body."]
        );
        assert_eq!(parse("fun a(
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b){}"), vec!["(defn a (\
        b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b \
        b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b \
        b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b \
        b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b \
        b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b \
        b b b b b) )"]);
        assert_eq!(parse("fun a(
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,
        b,b,b,b,b,b){}"), vec!["[line 7] Error at \'b\': Cannot have more than 255 parameters."]);
    }

    #[test]
    fn return_statement() {
        assert_eq!(parse("return;"), vec!["(return)"]);
        assert_eq!(parse("return 1+1;"), vec!["(return (+ 1 1))"]);
        assert_eq!(
            parse("return"),
            vec!["[line 1] Error at end: Expect expression."]
        );
        assert_eq!(
            parse("return 1+1"),
            vec!["[line 1] Error at end: Expect \';\' after return value."]
        );
    }

    #[test]
    fn class() {
        assert_eq!(parse("class a {}"), vec!["(class a )"]);
        assert_eq!(
            parse("class a {f(){}g(){}}"),
            vec!["(class a (defn f () ) (defn g () ))"]
        );
        assert_eq!(
            parse(
                "class Breakfast {
                   cook() {
                     print \"Eggs a-fryin'!\";
                   }
                   serve(who) {
                     print \"Enjoy your breakfast, \" + who + \".\";
                   }
                 }"
            ),
            vec![
                "(class Breakfast \
                   (defn cook () \
                     (print \"Eggs a-fryin'!\")) \
                   (defn serve (who) \
                     (print (+ (+ \"Enjoy your breakfast, \" who) \".\"))))"
            ]
        );
        assert_eq!(
            parse("class a {var b = 1;}"),
            vec![
                "[line 1] Error at \'var\': Expect method name.",
                "[line 1] Error at \'}\': Expect expression."
            ]
        );
        assert_eq!(
            parse("class {}"),
            vec!["[line 1] Error at \'{\': Expect class name."]
        );
        assert_eq!(
            parse("class class {}"),
            vec!["[line 1] Error at \'class\': Expect class name."]
        );
        assert_eq!(
            parse("class a"),
            vec!["[line 1] Error at end: Expect \'{\' before class body."]
        );
    }

    #[test]
    fn property() {
        assert_eq!(
            parse("egg.scramble(3).with(cheddar);"),
            vec!["(expr ((get ((get egg :scramble) 3) :with) cheddar))"]
        );
        assert_eq!(
            parse("breakfast.omlette.filling.meat = ham;"),
            vec!["(expr (set (get (get breakfast :omlette) :filling) :meat ham))"]
        );
        assert_eq!(
            parse("a.;"),
            vec!["[line 1] Error at \';\': Expect property name after \'.\'."]
        );
    }

    #[test]
    fn this() {
        assert_eq!(
            parse("class A {m(){return this;}}"),
            vec!["(class A (defn m () (return this)))"]
        );
        assert_eq!(
            parse("this.a = 1;"),
            vec!["(expr (set this :a 1))"]
        );
        assert_eq!(
            parse("this = 1;"),
            vec!["[line 1] Error at \'=\': Invalid assignment target."]
        );
        assert_eq!(
            parse("var this = 1;"),
            vec!["[line 1] Error at \'this\': Expect variable name."]
        );
    }
}

use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::rc::Rc;
use std::result::Result;

pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Rc<Expr>),
    If(Rc<Expr>, Rc<Stmt>, Option<Rc<Stmt>>),
    Print(Rc<Expr>),
    Var(Token, Option<Rc<Expr>>),
}

pub enum Expr {
    Asign(Token, Rc<Expr>),
    Binary(Rc<Expr>, Token, Rc<Expr>),
    Grouping(Rc<Expr>),
    Literal(Literal),
    Unary(Token, Rc<Expr>),
    Variable(Token),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Stmt::*;
        match self {
            Block(statemets) => write!(f, "(do {})", join(statemets, " ")),
            Expression(expression) => write!(f, "(expr {})", expression),
            If(condition, then, None) => write!(f, "(if {} {})", condition, then),
            If(condition, then, Some(r#else)) => {
                write!(f, "(if {} {} {})", condition, then, r#else)
            }
            Print(expression) => write!(f, "(print {})", expression),
            Var(name, None) => write!(f, "(def {})", name.lexeme),
            Var(name, Some(initializer)) => write!(f, "(def {} {})", name.lexeme, initializer),
        }
    }
}

fn join<D: Display>(ds: &[D], separator: &str) -> String {
    ds.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(separator)
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Asign(name, value) => write!(f, "(set! {} {})", name.lexeme, value),
            Binary(left, operator, right) => write!(f, "({} {} {})", operator.lexeme, left, right),
            Grouping(expression) => write!(f, "(group {})", expression),
            Literal(value) => write!(f, "{}", value),
            Unary(operator, right) => write!(f, "({} {})", operator.lexeme, right),
            Variable(name) => write!(f, "{}", name.lexeme),
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn display_expr() {
        use crate::token::Literal::*;
        use crate::token::TokenType::{Identifier, Minus, Star};
        use Expr::*;

        let expression = Binary(
            Rc::new(Unary(
                Token::new(Minus, "-", Nil, 1),
                Rc::new(Variable(Token::new(Identifier, "varname", Nil, 1))),
            )),
            Token::new(Star, "*", Nil, 1),
            Rc::new(Grouping(Rc::new(Literal(Number(45.67))))),
        );

        assert_eq!(expression.to_string(), "(* (- varname) (group 45.67))");
    }

    #[test]
    fn display_expression() {
        use crate::token::Literal::*;
        use crate::token::TokenType::Minus;
        use Expr::*;
        use Stmt::*;

        let expression = Expression(Rc::new(Unary(
            Token::new(Minus, "-", Nil, 1),
            Rc::new(Literal(Number(123.))),
        )));

        assert_eq!(expression.to_string(), "(expr (- 123))");
    }

    #[test]
    fn display_print() {
        use crate::token::Literal::*;
        use crate::token::TokenType::Minus;
        use Expr::*;
        use Stmt::*;

        let expression = Print(Rc::new(Unary(
            Token::new(Minus, "-", Nil, 1),
            Rc::new(Literal(Number(123.))),
        )));

        assert_eq!(expression.to_string(), "(print (- 123))");
    }

    #[test]
    fn display_var() {
        use crate::token::Literal::*;
        use crate::token::TokenType::Identifier;
        use Expr::*;
        use Stmt::*;

        let expression = Var(
            Token::new(Identifier, "varname", Nil, 1),
            Some(Rc::new(Literal(Number(42.)))),
        );
        assert_eq!(expression.to_string(), "(def varname 42)");

        let expression = Var(Token::new(Identifier, "another", Nil, 1), None);
        assert_eq!(expression.to_string(), "(def another)");
    }

    #[test]
    fn display_assign() {
        use crate::token::Literal::*;
        use crate::token::TokenType::Identifier;
        use Expr::*;

        let expression = Asign(
            Token::new(Identifier, "varname", Nil, 1),
            Rc::new(Literal(Number(42.))),
        );

        assert_eq!(expression.to_string(), "(set! varname 42)");
    }

    #[test]
    fn display_block() {
        use crate::token::Literal::*;
        use crate::token::TokenType::Identifier;
        use Expr::*;
        use Stmt::*;

        let expression = Block(vec![
            Expression(Rc::new(Asign(
                Token::new(Identifier, "varname", Nil, 1),
                Rc::new(Literal(Number(42.))),
            ))),
            Expression(Rc::new(Literal(Bool(false)))),
        ]);

        assert_eq!(
            expression.to_string(),
            "(do (expr (set! varname 42)) (expr false))"
        );
    }

    #[test]
    fn if_statement() {
        use crate::token::Literal::*;
        use Expr::*;
        use Stmt::*;

        let expression = If(
            Rc::new(Literal(Bool(false))),
            Rc::new(Expression(Rc::new(Literal(Number(12.))))),
            Some(Rc::new(Print(Rc::new(Literal(Number(21.)))))),
        );
        assert_eq!(expression.to_string(), "(if false (expr 12) (print 21))");

        let expression = If(
            Rc::new(Literal(Bool(false))),
            Rc::new(Expression(Rc::new(Literal(Number(12.))))),
            None,
        );
        assert_eq!(expression.to_string(), "(if false (expr 12))");
    }
}

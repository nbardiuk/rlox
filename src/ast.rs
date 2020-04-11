use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::rc::Rc;
use std::result::Result;

pub enum Stmt {
    Expression(Rc<Expr>),
    Print(Rc<Expr>),
    Var(Token, Option<Rc<Expr>>),
}

pub enum Expr {
    Unary(Token, Rc<Expr>),
    Asign(Token, Rc<Expr>),
    Binary(Rc<Expr>, Token, Rc<Expr>),
    Grouping(Rc<Expr>),
    Literal(Literal),
    Variable(Token),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Stmt::*;
        match self {
            Expression(expression) => write!(f, "(expr {})", expression),
            Print(expression) => write!(f, "(print {})", expression),
            Var(name, Some(initializer)) => write!(f, "(def {} {})", name.lexeme, initializer),
            Var(name, None) => write!(f, "(def {})", name.lexeme),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Unary(operator, right) => write!(f, "({} {})", operator.lexeme, right),
            Asign(name, value) => write!(f, "(set! {} {})", name.lexeme, value),
            Binary(left, operator, right) => write!(f, "({} {} {})", operator.lexeme, left, right),
            Grouping(expression) => write!(f, "(group {})", expression),
            Literal(value) => write!(f, "{}", value),
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
}

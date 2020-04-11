use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::rc::Rc;
use std::result::Result;

#[derive(Clone)]
pub enum Expr {
    Unary(Token, Rc<Expr>),
    Binary(Rc<Expr>, Token, Rc<Expr>),
    Grouping(Rc<Expr>),
    Literal(Literal),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Unary(operator, right) => write!(f, "({} {})", operator.lexeme, right),
            Binary(left, operator, right) => write!(f, "({} {} {})", operator.lexeme, left, right),
            Grouping(expression) => write!(f, "(group {})", expression),
            Literal(value) => write!(f, "{}", value),
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn printing() {
        use crate::token::Literal::*;
        use crate::token::TokenType::{Minus, Star};
        use Expr::*;

        let expression = Binary(
            Rc::new(Unary(
                Token::new(Minus, "-", Nil, 1),
                Rc::new(Literal(Number(123.))),
            )),
            Token::new(Star, "*", Nil, 1),
            Rc::new(Grouping(Rc::new(Literal(Number(45.67))))),
        );

        assert_eq!(expression.to_string(), "(* (- 123) (group 45.67))");
    }
}

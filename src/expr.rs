use crate::token::Literal;
use crate::token::Token;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::result::Result;

enum Expr<'a> {
    Unary {
        operator: Token<'a>,
        right: &'a Expr<'a>,
    },
    Binary {
        left: &'a Expr<'a>,
        operator: Token<'a>,
        right: &'a Expr<'a>,
    },
    Grouping(&'a Expr<'a>),
    Literal(Literal<'a>),
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expr::*;
        match self {
            Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
            Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", operator.lexeme, left, right),
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

        let expression = Binary {
            left: &Unary {
                operator: Token::new(Minus, "-", Nil, 1),
                right: &Literal(Number(123.)),
            },
            operator: Token::new(Star, "*", Nil, 1),
            right: &Grouping(&Literal(Number(45.67))),
        };

        assert_eq!(expression.to_string(), "(* (- 123) (group 45.67))");
    }
}

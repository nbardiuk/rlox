use crate::expr::Expr::{self, *};
use crate::token::{self, Literal::*, Token, TokenType as t};
use std::result::Result::{Err, Ok};

fn evaluate<'a>(expr: &'a Expr) -> Result<token::Literal, RuntimeError> {
    match expr {
        Literal(value) => Ok(value.clone()),
        Grouping(expression) => evaluate(&expression),
        Unary(operator, right) => match (operator.typ, evaluate(right)) {
            (_, e @ Err(_)) => e,
            (t::Bang, Ok(r)) => Ok(Bool(!is_truthy(r))),
            (t::Minus, Ok(Number(d))) => Ok(Number(-d)),
            _ => nan(operator),
        },
        Binary(left, operator, right) => match (operator.typ, evaluate(left), evaluate(right)) {
            (_, e @ Err(_), _) => e,
            (_, _, e @ Err(_)) => e,
            (t::Minus, Ok(Number(a)), Ok(Number(b))) => Ok(Number(a - b)),
            (t::Slash, Ok(Number(a)), Ok(Number(b))) => Ok(Number(a / b)),
            (t::Star, Ok(Number(a)), Ok(Number(b))) => Ok(Number(a * b)),
            (t::Plus, Ok(Number(a)), Ok(Number(b))) => Ok(Number(a + b)),
            (t::Plus, Ok(String(a)), Ok(String(b))) => Ok(String(a + &b)),
            (t::Plus, _, _) => nans_ss(operator),
            (t::Greater, Ok(Number(a)), Ok(Number(b))) => Ok(Bool(a > b)),
            (t::GreaterEqual, Ok(Number(a)), Ok(Number(b))) => Ok(Bool(a >= b)),
            (t::Less, Ok(Number(a)), Ok(Number(b))) => Ok(Bool(a < b)),
            (t::LessEqual, Ok(Number(a)), Ok(Number(b))) => Ok(Bool(a <= b)),
            (t::BangEqual, Ok(a), Ok(b)) => Ok(Bool(a != b)),
            (t::EqualEqual, Ok(a), Ok(b)) => Ok(Bool(a == b)),
            _ => nans(operator),
        },
    }
}

fn nan<T>(operator: &Token) -> Result<T, RuntimeError> {
    RuntimeError::new(operator, "Operand must be a number")
}
fn nans<T>(operator: &Token) -> Result<T, RuntimeError> {
    RuntimeError::new(operator, "Operands must be numbers")
}
fn nans_ss<T>(operator: &Token) -> Result<T, RuntimeError> {
    RuntimeError::new(operator, "Operands must be two numbers or two strings")
}

#[derive(Debug, PartialEq)]
struct RuntimeError {
    token: Token,
    message: std::string::String,
}
impl RuntimeError {
    fn new<T>(token: &Token, message: &str) -> Result<T, Self> {
        Err(RuntimeError {
            token: token.clone(),
            message: message.to_string(),
        })
    }
}

fn is_truthy(l: token::Literal) -> bool {
    match l {
        Nil => false,
        Bool(b) => b,
        _ => true,
    }
}
#[cfg(test)]
mod spec {
    use super::*;
    use crate::lox::Lox;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::TokenType;

    fn nan<T>(typ: TokenType, s: &str) -> Result<T, RuntimeError> {
        RuntimeError::new(&Token::new(typ, s, Nil, 1), "Operand must be a number")
    }

    fn nans<T>(typ: TokenType, s: &str) -> Result<T, RuntimeError> {
        RuntimeError::new(&Token::new(typ, s, Nil, 1), "Operands must be numbers")
    }

    fn nans_ss<T>(typ: TokenType, s: &str) -> Result<T, RuntimeError> {
        RuntimeError::new(
            &Token::new(typ, s, Nil, 1),
            "Operands must be two numbers or two strings",
        )
    }

    fn eval<'a>(source: &'a str) -> Result<token::Literal, RuntimeError> {
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        let result = parser.parse().as_ref().map(evaluate).unwrap_or(Ok(Nil));
        result
    }

    #[test]
    fn literal() {
        assert_eq!(eval("nil"), Ok(Nil));
        assert_eq!(eval("1"), Ok(Number(1.)));
        assert_eq!(eval("\"str\""), Ok(String("str".to_string())));
        assert_eq!(eval("true"), Ok(Bool(true)));
    }

    #[test]
    fn unary_minus() {
        assert_eq!(eval("-1"), Ok(Number(-1.)));
        assert_eq!(eval("--1"), Ok(Number(1.)));
        assert_eq!(eval("-false"), nan(t::Minus, "-"));
        assert_eq!(eval("-\"\""), nan(t::Minus, "-"));
        assert_eq!(eval("-nil"), nan(t::Minus, "-"));
    }

    #[test]
    fn unary_bang() {
        assert_eq!(eval("!1"), Ok(Bool(false)));
        assert_eq!(eval("!0"), Ok(Bool(false)));
        assert_eq!(eval("!true"), Ok(Bool(false)));
        assert_eq!(eval("!false"), Ok(Bool(true)));
        assert_eq!(eval("!\"\""), Ok(Bool(false)));
        assert_eq!(eval("!\"non empty\""), Ok(Bool(false)));
        assert_eq!(eval("!nil"), Ok(Bool(true)));
        assert_eq!(eval("!!!!1"), Ok(Bool(true)));
    }

    #[test]
    fn binary_minus() {
        assert_eq!(eval("2-1"), Ok(Number(1.)));
        assert_eq!(eval("1-2"), Ok(Number(-1.)));
        assert_eq!(eval("true-1"), nans(t::Minus, "-"));
        assert_eq!(eval("nil-1"), nans(t::Minus, "-"));
        assert_eq!(eval("1-\"\""), nans(t::Minus, "-"));
    }

    #[test]
    fn binary_slash() {
        assert_eq!(eval("1/2"), Ok(Number(0.5)));
        assert_eq!(eval("3/-2"), Ok(Number(-1.5)));
        assert_eq!(eval("true/1"), nans(t::Slash, "/"));
        assert_eq!(eval("nil/1"), nans(t::Slash, "/"));
        assert_eq!(eval("1/\"\""), nans(t::Slash, "/"));
    }

    #[test]
    fn binary_star() {
        assert_eq!(eval("2*3"), Ok(Number(6.)));
        assert_eq!(eval("true*1"), nans(t::Star, "*"));
        assert_eq!(eval("nil*1"), nans(t::Star, "*"));
        assert_eq!(eval("1*\"\""), nans(t::Star, "*"));
    }

    #[test]
    fn binary_plus() {
        assert_eq!(eval("2+1"), Ok(Number(3.)));
        assert_eq!(eval("\"a\"+\"b\""), Ok(String("ab".to_string())));
        assert_eq!(eval("true+1"), nans_ss(t::Plus, "+"));
        assert_eq!(eval("nil+1"), nans_ss(t::Plus, "+"));
        assert_eq!(eval("1+\"\""), nans_ss(t::Plus, "+"));
    }

    #[test]
    fn binary_greater() {
        assert_eq!(eval("2>3"), Ok(Bool(false)));
        assert_eq!(eval("2>1"), Ok(Bool(true)));
        assert_eq!(eval("1>1"), Ok(Bool(false)));
        assert_eq!(eval("true>1"), nans(t::Greater, ">"));
        assert_eq!(eval("nil>1"), nans(t::Greater, ">"));
        assert_eq!(eval("1>\"\""), nans(t::Greater, ">"));
    }

    #[test]
    fn binary_greater_equal() {
        assert_eq!(eval("2>=3"), Ok(Bool(false)));
        assert_eq!(eval("2>=1"), Ok(Bool(true)));
        assert_eq!(eval("1>=1"), Ok(Bool(true)));
        assert_eq!(eval("true>=1"), nans(t::GreaterEqual, ">="));
        assert_eq!(eval("nil>=1"), nans(t::GreaterEqual, ">="));
        assert_eq!(eval("1>=\"\""), nans(t::GreaterEqual, ">="));
    }

    #[test]
    fn binary_less() {
        assert_eq!(eval("2<3"), Ok(Bool(true)));
        assert_eq!(eval("2<1"), Ok(Bool(false)));
        assert_eq!(eval("1<1"), Ok(Bool(false)));
        assert_eq!(eval("true<1"), nans(t::Less, "<"));
        assert_eq!(eval("nil<1"), nans(t::Less, "<"));
        assert_eq!(eval("1<\"\""), nans(t::Less, "<"));
    }

    #[test]
    fn binary_less_equal() {
        assert_eq!(eval("2<=3"), Ok(Bool(true)));
        assert_eq!(eval("2<=1"), Ok(Bool(false)));
        assert_eq!(eval("1<=1"), Ok(Bool(true)));
        assert_eq!(eval("true<=1"), nans(t::LessEqual, "<="));
        assert_eq!(eval("nil<=1"), nans(t::LessEqual, "<="));
        assert_eq!(eval("1<=\"\""), nans(t::LessEqual, "<="));
    }

    #[test]
    fn binary_equal_equal() {
        assert_eq!(eval("1==1"), Ok(Bool(true)));
        assert_eq!(eval("1==2"), Ok(Bool(false)));
        assert_eq!(eval("true==true"), Ok(Bool(true)));
        assert_eq!(eval("true==false"), Ok(Bool(false)));
        assert_eq!(eval("nil==nil"), Ok(Bool(true)));
        assert_eq!(eval("\"a\"==\"b\""), Ok(Bool(false)));
        assert_eq!(eval("\"a\"==\"a\""), Ok(Bool(true)));
        assert_eq!(eval("true==1"), Ok(Bool(false)));
        assert_eq!(eval("nil==0"), Ok(Bool(false)));
        assert_eq!(eval("nil==false"), Ok(Bool(false)));
        assert_eq!(eval("\"true\"==true"), Ok(Bool(false)));
        assert_eq!(eval("\"1\"==1"), Ok(Bool(false)));
    }

    #[test]
    fn binary_bang_equal() {
        assert_eq!(eval("1!=1"), Ok(Bool(false)));
        assert_eq!(eval("1!=2"), Ok(Bool(true)));
        assert_eq!(eval("true!=true"), Ok(Bool(false)));
        assert_eq!(eval("true!=false"), Ok(Bool(true)));
        assert_eq!(eval("nil!=nil"), Ok(Bool(false)));
        assert_eq!(eval("\"a\"!=\"b\""), Ok(Bool(true)));
        assert_eq!(eval("\"a\"!=\"a\""), Ok(Bool(false)));
        assert_eq!(eval("true!=1"), Ok(Bool(true)));
        assert_eq!(eval("nil!=0"), Ok(Bool(true)));
        assert_eq!(eval("nil!=false"), Ok(Bool(true)));
        assert_eq!(eval("\"true\"!=true"), Ok(Bool(true)));
        assert_eq!(eval("\"1\"!=1"), Ok(Bool(true)));
    }

    #[test]
    fn grouping() {
        assert_eq!(eval("3*(1+2)"), Ok(Number(9.)));
        assert_eq!(eval("!(1==2)"), Ok(Bool(true)));
        assert_eq!(eval("-(1+nil)"), nans_ss(t::Plus, "+"));
        assert_eq!(eval("2 * (3 / -\"muffin\")"), nan(t::Minus, "-"));
    }
}

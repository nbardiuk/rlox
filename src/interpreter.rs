use crate::expr::Expr::{self, *};
use crate::lox::Lox;
use crate::token::{self, Literal::*, Token, TokenType as t};
use std::io::{self, Write};
use std::result::Result::{Err, Ok};

pub fn interpret<W: Write>(lox: &mut Lox<W>, expr: Expr) -> io::Result<()> {
    match evaluate(&expr) {
        Ok(v) => lox.println(&v.to_string()),
        Err(e) => lox.runtime_error(e),
    }
}

fn evaluate(expr: &Expr) -> Result<token::Literal, RuntimeError> {
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
pub struct RuntimeError {
    pub token: Token,
    pub message: std::string::String,
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
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn run<'a>(source: &'a str) -> std::string::String {
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        if let Some(expr) = parser.parse() {
            interpret(&mut lox, expr).unwrap();
        }
        lox.output()
    }

    #[test]
    fn literal() {
        assert_eq!(run("nil"), "nil\n");
        assert_eq!(run("1"), "1\n");
        assert_eq!(run("\"str\""), "\"str\"\n");
        assert_eq!(run("true"), "true\n");
    }

    #[test]
    fn unary_minus() {
        assert_eq!(run("-1"), "-1\n");
        assert_eq!(run("--1"), "1\n");
        assert_eq!(run("-false"), "[line 1] Operand must be a number\n");
        assert_eq!(run("-\"\""), "[line 1] Operand must be a number\n");
        assert_eq!(run("-nil"), "[line 1] Operand must be a number\n");
    }

    #[test]
    fn unary_bang() {
        assert_eq!(run("!1"), "false\n");
        assert_eq!(run("!0"), "false\n");
        assert_eq!(run("!true"), "false\n");
        assert_eq!(run("!false"), "true\n");
        assert_eq!(run("!\"\""), "false\n");
        assert_eq!(run("!\"non empty\""), "false\n");
        assert_eq!(run("!nil"), "true\n");
        assert_eq!(run("!!!!1"), "true\n");
    }

    #[test]
    fn binary_minus() {
        assert_eq!(run("2-1"), "1\n");
        assert_eq!(run("1-2"), "-1\n");
        assert_eq!(run("true-1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil-1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1-\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_slash() {
        assert_eq!(run("1/2"), "0.5\n");
        assert_eq!(run("3/-2"), "-1.5\n");
        assert_eq!(run("true/1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil/1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1/\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_star() {
        assert_eq!(run("2*3"), "6\n");
        assert_eq!(run("true*1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil*1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1*\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_plus() {
        assert_eq!(run("2+1"), "3\n");
        assert_eq!(run("\"a\"+\"b\""), "\"ab\"\n");
        assert_eq!(
            run("true+1"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("nil+1"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("1+\"\""),
            "[line 1] Operands must be two numbers or two strings\n"
        );
    }

    #[test]
    fn binary_greater() {
        assert_eq!(run("2>3"), "false\n");
        assert_eq!(run("2>1"), "true\n");
        assert_eq!(run("1>1"), "false\n");
        assert_eq!(run("true>1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil>1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1>\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_greater_equal() {
        assert_eq!(run("2>=3"), "false\n");
        assert_eq!(run("2>=1"), "true\n");
        assert_eq!(run("1>=1"), "true\n");
        assert_eq!(run("true>=1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil>=1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1>=\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_less() {
        assert_eq!(run("2<3"), "true\n");
        assert_eq!(run("2<1"), "false\n");
        assert_eq!(run("1<1"), "false\n");
        assert_eq!(run("true<1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil<1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1<\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_less_equal() {
        assert_eq!(run("2<=3"), "true\n");
        assert_eq!(run("2<=1"), "false\n");
        assert_eq!(run("1<=1"), "true\n");
        assert_eq!(run("true<=1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("nil<=1"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("1<=\"\""), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_equal_equal() {
        assert_eq!(run("1==1"), "true\n");
        assert_eq!(run("1==2"), "false\n");
        assert_eq!(run("true==true"), "true\n");
        assert_eq!(run("true==false"), "false\n");
        assert_eq!(run("nil==nil"), "true\n");
        assert_eq!(run("\"a\"==\"b\""), "false\n");
        assert_eq!(run("\"a\"==\"a\""), "true\n");
        assert_eq!(run("true==1"), "false\n");
        assert_eq!(run("nil==0"), "false\n");
        assert_eq!(run("nil==false"), "false\n");
        assert_eq!(run("\"true\"==true"), "false\n");
        assert_eq!(run("\"1\"==1"), "false\n");
    }

    #[test]
    fn binary_bang_equal() {
        assert_eq!(run("1!=1"), "false\n");
        assert_eq!(run("1!=2"), "true\n");
        assert_eq!(run("true!=true"), "false\n");
        assert_eq!(run("true!=false"), "true\n");
        assert_eq!(run("nil!=nil"), "false\n");
        assert_eq!(run("\"a\"!=\"b\""), "true\n");
        assert_eq!(run("\"a\"!=\"a\""), "false\n");
        assert_eq!(run("true!=1"), "true\n");
        assert_eq!(run("nil!=0"), "true\n");
        assert_eq!(run("nil!=false"), "true\n");
        assert_eq!(run("\"true\"!=true"), "true\n");
        assert_eq!(run("\"1\"!=1"), "true\n");
    }

    #[test]
    fn grouping() {
        assert_eq!(run("3*(1+2)"), "9\n");
        assert_eq!(run("!(1==2)"), "true\n");
        assert_eq!(
            run("-(1+nil)"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("2 * (3 / -\"muffin\")"),
            "[line 1] Operand must be a number\n"
        );
    }
}

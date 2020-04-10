use crate::expr::Expr::{self, *};
use crate::token::{self, Literal::*, TokenType as t};

fn evaluate<'a>(expr: &'a Expr<'a>) -> token::Literal {
    match expr {
        Literal(value) => value.clone(),
        Grouping(expression) => evaluate(&expression),
        Unary(operator, right) => {
            match (operator.typ, evaluate(right)) {
                (t::Bang, r) => Bool(!is_truthy(r)),
                (t::Minus, Number(d)) => Number(-d),
                _ => Nil, // TODO report error
            }
        }
        Binary(left, operator, right) => {
            match (operator.typ, evaluate(left), evaluate(right)) {
                (t::Minus, Number(a), Number(b)) => Number(a - b),
                (t::Slash, Number(a), Number(b)) => Number(a / b),
                (t::Star, Number(a), Number(b)) => Number(a * b),
                (t::Plus, Number(a), Number(b)) => Number(a + b),
                (t::Plus, String(a), String(b)) => String(a + &b),
                (t::Greater, Number(a), Number(b)) => Bool(a > b),
                (t::GreaterEqual, Number(a), Number(b)) => Bool(a >= b),
                (t::Less, Number(a), Number(b)) => Bool(a < b),
                (t::LessEqual, Number(a), Number(b)) => Bool(a <= b),
                (t::BangEqual, a, b) => Bool(a != b),
                (t::EqualEqual, a, b) => Bool(a == b),
                _ => Nil, // TODO report error
            }
        }
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

    fn eval<'a>(source: &'a str) -> token::Literal {
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        let result = parser.parse().as_ref().map(evaluate).unwrap_or(Nil);
        print!("{}", lox.output());
        result
    }

    #[test]
    fn literal() {
        assert_eq!(eval("nil"), Nil);
        assert_eq!(eval("1"), Number(1.));
        assert_eq!(eval("\"str\""), String("str".to_string()));
        assert_eq!(eval("true"), Bool(true));
    }

    #[test]
    fn unary_minus() {
        assert_eq!(eval("-1"), Number(-1.));
        assert_eq!(eval("--1"), Number(1.));
        assert_eq!(eval("-false"), Nil); // TODO report error
        assert_eq!(eval("-\"\""), Nil); // TODO report error
        assert_eq!(eval("-nil"), Nil); // TODO report error
    }

    #[test]
    fn unary_bang() {
        assert_eq!(eval("!1"), Bool(false));
        assert_eq!(eval("!0"), Bool(false));
        assert_eq!(eval("!true"), Bool(false));
        assert_eq!(eval("!false"), Bool(true));
        assert_eq!(eval("!\"\""), Bool(false));
        assert_eq!(eval("!\"non empty\""), Bool(false));
        assert_eq!(eval("!nil"), Bool(true));
        assert_eq!(eval("!!!!1"), Bool(true));
    }

    #[test]
    fn binary_minus() {
        assert_eq!(eval("2-1"), Number(1.));
        assert_eq!(eval("1-2"), Number(-1.));
        assert_eq!(eval("true-1"), Nil); // TODO report error
        assert_eq!(eval("nil-1"), Nil); // TODO report error
        assert_eq!(eval("1-\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_slash() {
        assert_eq!(eval("1/2"), Number(0.5));
        assert_eq!(eval("3/-2"), Number(-1.5));
        assert_eq!(eval("true/1"), Nil); // TODO report error
        assert_eq!(eval("nil/1"), Nil); // TODO report error
        assert_eq!(eval("1/\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_star() {
        assert_eq!(eval("2*3"), Number(6.));
        assert_eq!(eval("true*1"), Nil); // TODO report error
        assert_eq!(eval("nil*1"), Nil); // TODO report error
        assert_eq!(eval("1*\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_plus() {
        assert_eq!(eval("2+1"), Number(3.));
        assert_eq!(eval("\"a\"+\"b\""), String("ab".to_string()));
        assert_eq!(eval("true+1"), Nil); // TODO report error
        assert_eq!(eval("nil+1"), Nil); // TODO report error
        assert_eq!(eval("1+\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_greater() {
        assert_eq!(eval("2>3"), Bool(false));
        assert_eq!(eval("2>1"), Bool(true));
        assert_eq!(eval("1>1"), Bool(false));
        assert_eq!(eval("true>1"), Nil); // TODO report error
        assert_eq!(eval("nil>1"), Nil); // TODO report error
        assert_eq!(eval("1>\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_greater_equal() {
        assert_eq!(eval("2>=3"), Bool(false));
        assert_eq!(eval("2>=1"), Bool(true));
        assert_eq!(eval("1>=1"), Bool(true));
        assert_eq!(eval("true>=1"), Nil); // TODO report error
        assert_eq!(eval("nil>=1"), Nil); // TODO report error
        assert_eq!(eval("1>=\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_less() {
        assert_eq!(eval("2<3"), Bool(true));
        assert_eq!(eval("2<1"), Bool(false));
        assert_eq!(eval("1<1"), Bool(false));
        assert_eq!(eval("true<1"), Nil); // TODO report error
        assert_eq!(eval("nil<1"), Nil); // TODO report error
        assert_eq!(eval("1<\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_less_equal() {
        assert_eq!(eval("2<=3"), Bool(true));
        assert_eq!(eval("2<=1"), Bool(false));
        assert_eq!(eval("1<=1"), Bool(true));
        assert_eq!(eval("true<=1"), Nil); // TODO report error
        assert_eq!(eval("nil<=1"), Nil); // TODO report error
        assert_eq!(eval("1<=\"\""), Nil); // TODO report error
    }

    #[test]
    fn binary_equal_equal() {
        assert_eq!(eval("1==1"), Bool(true));
        assert_eq!(eval("1==2"), Bool(false));
        assert_eq!(eval("true==true"), Bool(true));
        assert_eq!(eval("true==false"), Bool(false));
        assert_eq!(eval("nil==nil"), Bool(true));
        assert_eq!(eval("\"a\"==\"b\""), Bool(false));
        assert_eq!(eval("\"a\"==\"a\""), Bool(true));
        assert_eq!(eval("true==1"), Bool(false));
        assert_eq!(eval("nil==0"), Bool(false));
        assert_eq!(eval("nil==false"), Bool(false));
        assert_eq!(eval("\"true\"==true"), Bool(false));
        assert_eq!(eval("\"1\"==1"), Bool(false));
    }

    #[test]
    fn binary_bang_equal() {
        assert_eq!(eval("1!=1"), Bool(false));
        assert_eq!(eval("1!=2"), Bool(true));
        assert_eq!(eval("true!=true"), Bool(false));
        assert_eq!(eval("true!=false"), Bool(true));
        assert_eq!(eval("nil!=nil"), Bool(false));
        assert_eq!(eval("\"a\"!=\"b\""), Bool(true));
        assert_eq!(eval("\"a\"!=\"a\""), Bool(false));
        assert_eq!(eval("true!=1"), Bool(true));
        assert_eq!(eval("nil!=0"), Bool(true));
        assert_eq!(eval("nil!=false"), Bool(true));
        assert_eq!(eval("\"true\"!=true"), Bool(true));
        assert_eq!(eval("\"1\"!=1"), Bool(true));
    }

    #[test]
    fn grouping() {
        assert_eq!(eval("3*(1+2)"), Number(9.));
        assert_eq!(eval("!(1==2)"), Bool(true));
        assert_eq!(eval("-(1+nil)"), Nil); // TODO report error
    }
}

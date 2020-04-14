use crate::ast::Expr::{self, *};
use crate::ast::Stmt::{self, *};
use crate::environment::EnvRef;
use crate::environment::Environment;
use crate::lox::Lox;
use crate::token::{self, Literal::*, Token, TokenType as t};
use std::fmt;
use std::io::Write;
use std::rc::Rc;
use std::result::Result::{Err, Ok};
use std::time::Instant;
use Value::*;

type Result<T> = std::result::Result<T, RuntimeError>;

pub fn interpret<W: Write>(lox: &mut Lox<W>, env: EnvRef, statements: Vec<Stmt>) {
    let global = Environment::global(env.clone());
    global
        .borrow_mut()
        .define("clock", F(Rc::new(Clock::new())));
    for stmt in statements {
        if let Err(e) = execute(lox, env.clone(), &stmt) {
            lox.runtime_error(e);
            break;
        }
    }
}

fn execute_block<W: Write>(lox: &mut Lox<W>, env: EnvRef, statements: &[Stmt]) -> Result<()> {
    statements
        .iter()
        .try_for_each(|s| execute(lox, env.clone(), s))
}

fn execute<W: Write>(lox: &mut Lox<W>, env: EnvRef, stmt: &Stmt) -> Result<()> {
    match stmt {
        Block(statements) => {
            execute_block(lox, Environment::nested(env), &statements)?;
        }
        Expression(expression) => {
            evaluate(lox, env, &expression).map(|_| ())?;
        }
        Function(name, params, body) => env.borrow_mut().define(
            &name.lexeme,
            F(Rc::new(Function {
                name: name.clone(),
                params: params.clone(),
                body: body.to_vec(),
            })),
        ),
        If(condition, then, r#else) => {
            if is_truthy(&evaluate(lox, env.clone(), &condition)?) {
                execute(lox, env, &then)?;
            } else if let Some(els) = r#else {
                execute(lox, env, &els)?;
            }
        }
        Print(expression) => {
            let val = evaluate(lox, env, &expression)?;
            lox.println(&val.to_string());
        }
        Var(name, initializer) => match initializer {
            Some(i) => {
                let value = evaluate(lox, env.clone(), &i)?;
                env.borrow_mut().define(&name.lexeme, value)
            }
            _ => env.borrow_mut().define(&name.lexeme, V(Nil)),
        },
        While(condition, body) => {
            while is_truthy(&evaluate(lox, env.clone(), &condition)?) {
                execute(lox, env.clone(), &body)?
            }
        }
    }
    Ok(())
}

fn evaluate<W: Write>(lox: &mut Lox<W>, env: EnvRef, expr: &Expr) -> Result<Value> {
    match expr {
        Asign(name, value) => {
            let value = evaluate(lox, env.clone(), value)?;
            env.borrow_mut().assign(name, value)
        }
        Binary(left, op, right) => {
            match (
                op.typ,
                evaluate(lox, env.clone(), left)?,
                evaluate(lox, env, right)?,
            ) {
                (t::BangEqual, V(a), V(b)) => Ok(V(Bool(a != b))),
                (t::EqualEqual, V(a), V(b)) => Ok(V(Bool(a == b))),
                (t::Greater, V(Number(a)), V(Number(b))) => Ok(V(Bool(a > b))),
                (t::GreaterEqual, V(Number(a)), V(Number(b))) => Ok(V(Bool(a >= b))),
                (t::Less, V(Number(a)), V(Number(b))) => Ok(V(Bool(a < b))),
                (t::LessEqual, V(Number(a)), V(Number(b))) => Ok(V(Bool(a <= b))),
                (t::Minus, V(Number(a)), V(Number(b))) => Ok(V(Number(a - b))),
                (t::Plus, V(Number(a)), V(Number(b))) => Ok(V(Number(a + b))),
                (t::Plus, V(String(a)), V(String(b))) => Ok(V(String(a + &b))),
                (t::Plus, _, _) => err(op, "Operands must be two numbers or two strings"),
                (t::Slash, V(Number(a)), V(Number(b))) => Ok(V(Number(a / b))),
                (t::Star, V(Number(a)), V(Number(b))) => Ok(V(Number(a * b))),
                _ => err(op, "Operands must be numbers"),
            }
        }
        Call(callee, paren, args) => {
            if let F(callee) = evaluate(lox, env.clone(), callee)? {
                if args.len() != callee.arity() {
                    let message = format!(
                        "Expected {} arguments but got {}.",
                        callee.arity(),
                        args.len()
                    );
                    err(paren, &message)
                } else {
                    let mut ars = vec![];
                    for arg in args {
                        ars.push(evaluate(lox, env.clone(), arg)?);
                    }
                    callee.call(
                        &mut |env, body| execute_block(lox, env, body),
                        Environment::nested(Environment::global(env)),
                        paren,
                        &ars,
                    )
                }
            } else {
                err(paren, "Can only call functions and classes.")
            }
        }
        Grouping(expression) => evaluate(lox, env, &expression),
        Literal(value) => Ok(V(value.clone())),
        Logical(left, op, right) => {
            let left = evaluate(lox, env.clone(), left)?;
            match (op.typ, is_truthy(&left)) {
                (t::And, false) => Ok(left),
                (t::Or, true) => Ok(left),
                _ => evaluate(lox, env, right),
            }
        }
        Unary(op, right) => match (op.typ, evaluate(lox, env, right)?) {
            (t::Bang, r) => Ok(V(Bool(!is_truthy(&r)))),
            (t::Minus, V(Number(d))) => Ok(V(Number(-d))),
            _ => err(op, "Operand must be a number"),
        },
        Variable(name) => env.borrow().get(name),
    }
}

#[derive(Clone)]
pub enum Value {
    V(token::Literal),
    F(Rc<dyn Callable>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            V(l) => write!(f, "{}", l),
            F(c) => write!(f, "{}", c),
        }
    }
}

pub trait Callable: fmt::Display {
    fn call(
        &self,
        execute_block: &mut dyn FnMut(EnvRef, &[Stmt]) -> Result<()>,
        env: EnvRef,
        paren: &Token,
        args: &[Value],
    ) -> Result<Value>;

    fn arity(&self) -> usize;
}

struct Clock {
    start: Instant,
}
impl Clock {
    fn new() -> Self {
        Self {
            start: Instant::now(),
        }
    }
}
impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _: &mut dyn FnMut(EnvRef, &[Stmt]) -> Result<()>,
        _: EnvRef,
        _: &Token,
        _: &[Value],
    ) -> Result<Value> {
        let now = Instant::now();
        Ok(V(Number(now.duration_since(self.start).as_secs_f64())))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "clock")
    }
}

struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f#{}", self.name,)
    }
}

impl Callable for Function {
    fn call(
        &self,
        execute_block: &mut dyn FnMut(EnvRef, &[Stmt]) -> Result<()>,
        env: EnvRef,
        _: &Token,
        args: &[Value],
    ) -> Result<Value> {
        let defs = self.params.iter().zip(args.iter());
        defs.for_each(|(param, arg)| env.borrow_mut().define(&param.lexeme, arg.clone()));

        execute_block(env, &self.body)?;

        Ok(V(Nil))
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub token: Token,
    pub message: std::string::String,
}

impl RuntimeError {
    pub fn new(token: &Token, message: &str) -> RuntimeError {
        RuntimeError {
            token: token.clone(),
            message: message.to_string(),
        }
    }
}

pub fn err<T>(token: &Token, message: &str) -> Result<T> {
    Err(RuntimeError::new(token, message))
}

fn is_truthy(v: &Value) -> bool {
    match v {
        V(Nil) => false,
        V(Bool(b)) => *b,
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
        let statements = parser.parse();
        interpret(&mut lox, Environment::new(), statements);
        lox.output()
    }

    #[test]
    fn literal() {
        assert_eq!(run("print nil;"), "nil\n");
        assert_eq!(run("print 1;"), "1\n");
        assert_eq!(run("print \"str\";"), "\"str\"\n");
        assert_eq!(run("print true;"), "true\n");
        assert_eq!(run("print clock;"), "clock\n");
    }

    #[test]
    fn unary_minus() {
        assert_eq!(run("print -1;"), "-1\n");
        assert_eq!(run("print --1;"), "1\n");
        assert_eq!(run("print -false;"), "[line 1] Operand must be a number\n");
        assert_eq!(run("print -\"\";"), "[line 1] Operand must be a number\n");
        assert_eq!(run("print -nil;"), "[line 1] Operand must be a number\n");
        assert_eq!(run("print -clock;"), "[line 1] Operand must be a number\n");
    }

    #[test]
    fn unary_bang() {
        assert_eq!(run("print !1;"), "false\n");
        assert_eq!(run("print !0;"), "false\n");
        assert_eq!(run("print !true;"), "false\n");
        assert_eq!(run("print !false;"), "true\n");
        assert_eq!(run("print !\"\";"), "false\n");
        assert_eq!(run("print !\"non empty\";"), "false\n");
        assert_eq!(run("print !nil;"), "true\n");
        assert_eq!(run("print !!!!1;"), "true\n");
        assert_eq!(run("print !clock;"), "false\n");
    }

    #[test]
    fn binary_minus() {
        assert_eq!(run("print 2-1;"), "1\n");
        assert_eq!(run("print 1-2;"), "-1\n");
        assert_eq!(run("print true-1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil-1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1-\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1-clock;"), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_slash() {
        assert_eq!(run("print 1/2;"), "0.5\n");
        assert_eq!(run("print 3/-2;"), "-1.5\n");
        assert_eq!(run("print true/1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil/1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1/\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1/clock;"), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_star() {
        assert_eq!(run("print 2*3;"), "6\n");
        assert_eq!(run("print true*1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil*1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1*\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1*clock;"), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_plus() {
        assert_eq!(run("print 2+1;"), "3\n");
        assert_eq!(run("print \"a\"+\"b\";"), "\"ab\"\n");
        assert_eq!(
            run("print true+1;"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("print nil+1;"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("print 1+\"\";"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("print 1+clock;"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
    }

    #[test]
    fn binary_greater() {
        assert_eq!(run("print 2>3;"), "false\n");
        assert_eq!(run("print 2>1;"), "true\n");
        assert_eq!(run("print 1>1;"), "false\n");
        assert_eq!(run("print true>1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil>1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1>\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1>clock;"), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_greater_equal() {
        assert_eq!(run("print 2>=3;"), "false\n");
        assert_eq!(run("print 2>=1;"), "true\n");
        assert_eq!(run("print 1>=1;"), "true\n");
        assert_eq!(run("print true>=1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil>=1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1>=\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(
            run("print 1>=clock;"),
            "[line 1] Operands must be numbers\n"
        );
    }

    #[test]
    fn binary_less() {
        assert_eq!(run("print 2<3;"), "true\n");
        assert_eq!(run("print 2<1;"), "false\n");
        assert_eq!(run("print 1<1;"), "false\n");
        assert_eq!(run("print true<1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil<1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1<\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1<clock;"), "[line 1] Operands must be numbers\n");
    }

    #[test]
    fn binary_less_equal() {
        assert_eq!(run("print 2<=3;"), "true\n");
        assert_eq!(run("print 2<=1;"), "false\n");
        assert_eq!(run("print 1<=1;"), "true\n");
        assert_eq!(run("print true<=1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print nil<=1;"), "[line 1] Operands must be numbers\n");
        assert_eq!(run("print 1<=\"\";"), "[line 1] Operands must be numbers\n");
        assert_eq!(
            run("print 1<=clock;"),
            "[line 1] Operands must be numbers\n"
        );
    }

    #[test]
    fn binary_equal_equal() {
        assert_eq!(run("print 1==1;"), "true\n");
        assert_eq!(run("print 1==2;"), "false\n");
        assert_eq!(run("print true==true;"), "true\n");
        assert_eq!(run("print true==false;"), "false\n");
        assert_eq!(run("print nil==nil;"), "true\n");
        assert_eq!(run("print \"a\"==\"b\";"), "false\n");
        assert_eq!(run("print \"a\"==\"a\";"), "true\n");
        assert_eq!(run("print true==1;"), "false\n");
        assert_eq!(run("print nil==0;"), "false\n");
        assert_eq!(run("print nil==false;"), "false\n");
        assert_eq!(run("print \"true\"==true;"), "false\n");
        assert_eq!(run("print \"1\"==1;"), "false\n");
        assert_eq!(
            run("print clock==1;"),
            "[line 1] Operands must be numbers\n"
        ); // FIXME wrong message
    }

    #[test]
    fn binary_bang_equal() {
        assert_eq!(run("print 1!=1;"), "false\n");
        assert_eq!(run("print 1!=2;"), "true\n");
        assert_eq!(run("print true!=true;"), "false\n");
        assert_eq!(run("print true!=false;"), "true\n");
        assert_eq!(run("print nil!=nil;"), "false\n");
        assert_eq!(run("print \"a\"!=\"b\";"), "true\n");
        assert_eq!(run("print \"a\"!=\"a\";"), "false\n");
        assert_eq!(run("print true!=1;"), "true\n");
        assert_eq!(run("print nil!=0;"), "true\n");
        assert_eq!(run("print nil!=false;"), "true\n");
        assert_eq!(run("print \"true\"!=true;"), "true\n");
        assert_eq!(run("print \"1\"!=1;"), "true\n");
        assert_eq!(
            run("print clock!=1;"),
            "[line 1] Operands must be numbers\n"
        ); // FIXME wrong message
    }

    #[test]
    fn grouping() {
        assert_eq!(run("print 3*(1+2);"), "9\n");
        assert_eq!(run("print !(1==2);"), "true\n");
        assert_eq!(
            run("print -(1+nil);"),
            "[line 1] Operands must be two numbers or two strings\n"
        );
        assert_eq!(
            run("print 2 * (3 / -\"muffin\");"),
            "[line 1] Operand must be a number\n"
        );
    }

    #[test]
    fn statement_error() {
        assert_eq!(
            run("1"),
            "[line 1] Error at end: Expect \';\' after value.\n"
        );
        assert_eq!(
            run("print 1"),
            "[line 1] Error at end: Expect \';\' after value.\n"
        );
        assert_eq!(run("print"), "[line 1] Error at end: Expect expression.\n");
    }

    #[test]
    fn global_vars() {
        assert_eq!(
            run("var a = 1;
                var b = 2;
                print a + b;"),
            "3\n"
        );
        assert_eq!(
            run("var a;
                print a;"),
            "nil\n"
        );
        assert_eq!(
            run("print a;
                 var a = \"too late!\";"),
            "[line 1] Undefined variable \'a\'.\n"
        );
        assert_eq!(
            run("var a = 1;
                 print a; // 1.
                 var a = true;
                 print a; // true."),
            "1\n\
             true\n"
        );
        assert_eq!(run("var a = 1; print a = 2;"), "2\n");
        assert_eq!(run("a = 1;"), "[line 1] Undefined variable \'a\'.\n");
        assert_eq!(
            run("var a;
                 var b;
                 var c;
                 print a;
                 print b;
                 print c;
                 a = b = c = 1;
                 print a;
                 print b;
                 print c;
                 "),
            "nil\n\
             nil\n\
             nil\n\
             1\n\
             1\n\
             1\n"
        );
        assert_eq!(
            run("var a = b = c = 1;"),
            "[line 1] Undefined variable \'c\'.\n"
        );
    }

    #[test]
    fn blocks() {
        assert_eq!(
            run("var a = \"global a\";
                 var b = \"global b\";
                 var c = \"global c\";
                 {
                   var a = \"outer a\";
                   var b = \"outer b\";
                   {
                     var a = \"inner a\";
                     print a;
                     print b;
                     print c;
                   }
                   print a;
                   print b;
                   print c;
                 }
                 print a;
                 print b;
                 print c;"),
            "\"inner a\"\n\
             \"outer b\"\n\
             \"global c\"\n\
             \"outer a\"\n\
             \"outer b\"\n\
             \"global c\"\n\
             \"global a\"\n\
             \"global b\"\n\
             \"global c\"\n"
        );
        assert_eq!(
            run("var a = 1;
                 {
                   var a = a + 2;
                   print a;
                 }"),
            "3\n"
        );
        assert_eq!(
            run("{
                   var a = 1;
                 }
                 {
                   a = 2;
                 }"),
            "[line 5] Undefined variable \'a\'.\n"
        );
        assert_eq!(
            run("{
                   var a = 1;
                 }
                 a = 2;
                 "),
            "[line 4] Undefined variable \'a\'.\n"
        );
    }

    #[test]
    fn ifs() {
        assert_eq!(
            run("var t = true;
                 var f = false;
                 if (t) print t; else print f;
                 if (f) print t; else print f;"),
            "true\n\
             false\n"
        );
        assert_eq!(
            run("if (true) print 1; else fail;
                 if (false) fail; else print 2;"),
            "1\n\
             2\n"
        );
    }

    #[test]
    fn logical() {
        assert_eq!(
            run("print 1 or 2 or FAIL;
                 print 1 and 2;
                 print 1 and false and FAIL;"),
            "1\n\
             2\n\
             false\n"
        );
        assert_eq!(
            run("print nil or FAIL;"),
            "[line 1] Undefined variable 'FAIL'.\n"
        );
        assert_eq!(
            run("print 1 and FAIL;"),
            "[line 1] Undefined variable 'FAIL'.\n"
        );
    }

    #[test]
    fn whiles() {
        assert_eq!(
            run("var a = 0;
                 var b = 1;
                 var i = 0;
                 while (i < 17) {
                   i = i + 1;
                   var t = b;
                   b = a + b;
                   a = t;
                 }
                 print b;"),
            "2584\n"
        );
        assert_eq!(run("while (false) { FAIL; } print 1;"), "1\n");
        assert_eq!(
            run("while (true) { FAIL; } print 1;"),
            "[line 1] Undefined variable \'FAIL\'.\n"
        );
        assert_eq!(
            run("while (FAIL) { print 1; } print 2;"),
            "[line 1] Undefined variable \'FAIL\'.\n"
        );
    }

    #[test]
    fn fors() {
        assert_eq!(
            run("for(var i=0;i<10;i=i+1)print i;"),
            "0\n\
             1\n\
             2\n\
             3\n\
             4\n\
             5\n\
             6\n\
             7\n\
             8\n\
             9\n"
        );
        assert_eq!(run("for(;false;) FAIL; print 1;"), "1\n");
        assert_eq!(
            run("for(;true;FAIL) BREAK; print 1;"),
            "[line 1] Undefined variable \'BREAK\'.\n"
        );
        assert_eq!(
            run("for(;true;FAIL) print 0; print 1;"),
            "0\n\
            [line 1] Undefined variable \'FAIL\'.\n"
        );
        assert_eq!(
            run("for(FAIL;true;) print 0; print 1;"),
            "[line 1] Undefined variable \'FAIL\'.\n"
        );
        assert_eq!(run("var i=10;for(i=0;i<3;i=i+1){}print i;"), "3\n");
        assert_eq!(run("var i=true;for(var i=0;i<3;i=i+1){}print i;"), "true\n");
        assert_eq!(
            run("for(var i=0;i<3;i=i+1){}print i;"),
            "[line 1] Undefined variable \'i\'.\n"
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(run("print clock() > 0 and clock() < 0.1;"), "true\n");
        assert_eq!(run("var f = clock; print f() < 0.1;"), "true\n");
        assert_eq!(run("A(B,C);"), "[line 1] Undefined variable \'A\'.\n");
        assert_eq!(
            run("clock(B,C);"),
            "[line 1] Expected 0 arguments but got 2.\n"
        );
        assert_eq!(
            run("1();"),
            "[line 1] Can only call functions and classes.\n"
        );
        assert_eq!(
            run("true();"),
            "[line 1] Can only call functions and classes.\n"
        );
        assert_eq!(
            run("nil();"),
            "[line 1] Can only call functions and classes.\n"
        );
        assert_eq!(
            run("\"not a fun\"();"),
            "[line 1] Can only call functions and classes.\n"
        );
    }

    #[test]
    fn function() {
        assert_eq!(
            run("fun add() {}
                 print add;"),
            "f#add\n"
        );
        assert_eq!(
            run("fun add(a, b, c) {
                   print a + b + c;
                 }
                 add(1, 2, 3);"),
            "6\n"
        );
        assert_eq!(
            run("fun count(n) {
                   if (n > 1) count(n - 1);
                   print n;
                 }
                 count(3);"),
            "1\n\
             2\n\
             3\n"
        );
        assert_eq!(
            run("fun a() {
                   var c = 0;
                 }
                 print c;"),
            "[line 4] Undefined variable \'c\'.\n"
        );
        assert_eq!(
            run("fun a() {
                   print c;
                 }
                 {
                   var c = 0;
                   a();
                 }"),
            "[line 2] Undefined variable \'c\'.\n"
        );
        assert_eq!(
            run("var c = 0;
                 fun f() { print c = c + 1; }
                 f();
                 f();
                 f();"),
            "1\n\
             2\n\
             3\n"
        );
    }
}

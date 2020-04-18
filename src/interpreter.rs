use crate::ast::Expr::{self, *};
use crate::ast::Stmt::{self, *};
use crate::environment::EnvRef;
use crate::environment::Environment;
use crate::lox::Lox;
use crate::token::{self, Literal::*, Token, TokenType as t};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::rc::Rc;
use std::result::Result::{Err, Ok};
use std::time::Instant;
use Value::*;

pub type Result<T> = std::result::Result<T, RuntimeException>;

pub struct Interpreter<'a, W: Write> {
    lox: &'a mut Lox<W>,
    locals: &'a HashMap<Expr, usize>,
}

impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(lox: &'a mut Lox<W>, locals: &'a HashMap<Expr, usize>) -> Self {
        Self { lox, locals }
    }

    pub fn interpret(&mut self, env: EnvRef, statements: Vec<Stmt>) {
        let global = Environment::global(env.clone());
        global
            .borrow_mut()
            .define("clock", F(Rc::new(Clock::new())));
        for stmt in statements {
            if let Err(e) = self.execute(env.clone(), &stmt) {
                self.lox.runtime_error(e);
                break;
            }
        }
    }

    fn execute_block(&mut self, env: EnvRef, statements: &[Stmt]) -> Result<()> {
        statements
            .iter()
            .try_for_each(|s| self.execute(env.clone(), s))
    }

    fn execute(&mut self, env: EnvRef, stmt: &Stmt) -> Result<()> {
        match stmt {
            Block(statements) => {
                self.execute_block(Environment::nested(env), &statements)?;
            }
            Class(name, methods) => {
                env.borrow_mut().define(&name.lexeme, V(Nil));
                let mut ms = HashMap::default();
                for method in methods {
                    if let Function(name, params, body) = method {
                        ms.insert(
                            name.lexeme.clone(),
                            Function {
                                name: name.clone(),
                                params: params.clone(),
                                body: body.to_vec(),
                                closure: env.clone(),
                            },
                        );
                    }
                }

                env.borrow_mut().assign(
                    name,
                    F(Rc::new(Class {
                        name: name.clone(),
                        methods: ms,
                    })),
                )?;
            }
            Expression(expression) => {
                self.evaluate(env, &expression).map(|_| ())?;
            }
            Function(name, params, body) => env.borrow_mut().define(
                &name.lexeme,
                F(Rc::new(Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.to_vec(),
                    closure: env.clone(),
                })),
            ),
            If(condition, then, r#else) => {
                if is_truthy(&self.evaluate(env.clone(), &condition)?) {
                    self.execute(env, &then)?;
                } else if let Some(els) = r#else {
                    self.execute(env, &els)?;
                }
            }
            Print(expression) => {
                let val = self.evaluate(env, &expression)?;
                self.lox.println(&val.to_string());
            }
            Return(_keyword, value) => {
                let value = match value {
                    Some(value) => self.evaluate(env, value)?,
                    None => V(Nil),
                };
                return Err(RuntimeException::Return(value));
            }
            Var(name, initializer) => match initializer {
                Some(i) => {
                    let value = self.evaluate(env.clone(), &i)?;
                    env.borrow_mut().define(&name.lexeme, value)
                }
                _ => env.borrow_mut().define(&name.lexeme, V(Nil)),
            },
            While(condition, body) => {
                while is_truthy(&self.evaluate(env.clone(), &condition)?) {
                    self.execute(env.clone(), &body)?
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, env: EnvRef, expr: &Expr) -> Result<Value> {
        match expr {
            Asign(name, value) => {
                let value = self.evaluate(env.clone(), value)?;
                if let Some(distance) = self.locals.get(expr) {
                    Environment::assign_at(env, *distance, name, value)
                } else {
                    Environment::global(env).borrow_mut().assign(name, value)
                }
            }
            Binary(left, op, right) => {
                match (
                    op.typ,
                    self.evaluate(env.clone(), left)?,
                    self.evaluate(env, right)?,
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
                if let F(callee) = self.evaluate(env.clone(), callee)? {
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
                            ars.push(self.evaluate(env.clone(), arg)?);
                        }
                        callee.call(&mut |env, body| self.execute_block(env, body), paren, &ars)
                    }
                } else {
                    err(paren, "Can only call functions and classes.")
                }
            }
            Get(object, name) => {
                if let I(i) = self.evaluate(env, &object)? {
                    i.get(name)
                } else {
                    err(&name, "Only instances have properties.")
                }
            }
            Grouping(expression) => self.evaluate(env, &expression),
            Literal(value) => Ok(V(value.clone())),
            Logical(left, op, right) => {
                let left = self.evaluate(env.clone(), left)?;
                match (op.typ, is_truthy(&left)) {
                    (t::And, false) => Ok(left),
                    (t::Or, true) => Ok(left),
                    _ => self.evaluate(env, right),
                }
            }
            Unary(op, right) => match (op.typ, self.evaluate(env, right)?) {
                (t::Bang, r) => Ok(V(Bool(!is_truthy(&r)))),
                (t::Minus, V(Number(d))) => Ok(V(Number(-d))),
                _ => err(op, "Operand must be a number"),
            },
            Set(object, name, value) => {
                if let I(i) = self.evaluate(env.clone(), &object)? {
                    let v = self.evaluate(env, &value)?;
                    i.set(name, v.clone());
                    Ok(v)
                } else {
                    err(&name, "Only instances have fields.")
                }
            }
            This(keyword) => self.lookup_variable(env, keyword, expr),
            Variable(name) => self.lookup_variable(env, name, expr),
        }
    }

    fn lookup_variable(&self, env: EnvRef, name: &Token, expr: &Expr) -> Result<Value> {
        if let Some(distance) = self.locals.get(expr) {
            Environment::get_at(env, *distance, name)
        } else {
            Environment::global(env).borrow().get(name)
        }
    }
}

#[derive(Clone)]
pub enum Value {
    V(token::Literal),
    F(Rc<dyn Callable>),
    I(Instance),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            V(l) => write!(f, "{}", l),
            F(c) => write!(f, "{}", c),
            I(i) => write!(f, "{}", i),
        }
    }
}

pub trait Callable: fmt::Display {
    fn call(
        &self,
        execute_block: &mut dyn FnMut(EnvRef, &[Stmt]) -> Result<()>,
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

#[derive(Clone)]
struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: EnvRef,
}
impl Function {
    fn bind(&self, instance: Instance) -> Self {
        let closure = Environment::nested(self.closure.clone());
        closure.borrow_mut().define("this", I(instance));
        Function {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            closure,
        }
    }
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
        _: &Token,
        args: &[Value],
    ) -> Result<Value> {
        let env = Environment::nested(self.closure.clone());

        let defs = self.params.iter().zip(args.iter());
        defs.for_each(|(param, arg)| env.borrow_mut().define(&param.lexeme, arg.clone()));

        match execute_block(env, &self.body) {
            Err(RuntimeException::Return(v)) => Ok(v),
            Ok(_) => Ok(V(Nil)),
            Err(e) => Err(e),
        }
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Clone)]
struct Class {
    name: Token,
    methods: HashMap<std::string::String, Function>,
}
impl Class {
    fn find_method(&self, name: &str) -> Option<Function> {
        self.methods.get(name).cloned()
    }
}
impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name,)
    }
}
impl Callable for Class {
    fn call(
        &self,
        execute_block: &mut dyn FnMut(EnvRef, &[Stmt]) -> Result<()>,
        paren: &Token,
        args: &[Value],
    ) -> Result<Value> {
        let this = Instance::new(self);
        if let Some(init) = self.find_method("init") {
            init.bind(this.clone()).call(execute_block, paren, args)?;
        }
        Ok(I(this))
    }

    fn arity(&self) -> usize {
        self.find_method("init").map(|i| i.arity()).unwrap_or(0)
    }
}

#[derive(Clone)]
pub struct Instance {
    class: Class,
    fields: Rc<RefCell<HashMap<std::string::String, Value>>>,
}
impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class.name,)
    }
}
impl Instance {
    fn new(class: &Class) -> Self {
        Self {
            class: class.clone(),
            fields: Rc::new(RefCell::new(HashMap::default())),
        }
    }

    fn set(&self, name: &Token, value: Value) {
        self.fields.borrow_mut().insert(name.lexeme.clone(), value);
    }

    fn get(&self, name: &Token) -> Result<Value> {
        if let Some(v) = self.fields.borrow().get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(f) = self.class.find_method(&name.lexeme) {
            Ok(F(Rc::new(f.bind(self.clone()))))
        } else {
            err(name, &format!("Undefined property '{}'.", name))
        }
    }
}

pub enum RuntimeException {
    Error(Token, std::string::String),
    Return(Value),
}

impl RuntimeException {
    pub fn new(token: &Token, message: &str) -> RuntimeException {
        RuntimeException::Error(token.clone(), message.to_string())
    }
}

pub fn err<T>(token: &Token, message: &str) -> Result<T> {
    Err(RuntimeException::new(token, message))
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
    use crate::resolver::Resolver;
    use crate::scanner::Scanner;

    fn run<'a>(source: &'a str) -> std::string::String {
        let mut lox = Lox::<Vec<u8>>::new();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        let statements = parser.parse();
        if lox.has_error {
            return lox.output();
        }
        let locals = Resolver::new(&mut lox).resolve(&statements).locals;
        if lox.has_error {
            return lox.output();
        }
        Interpreter::new(&mut lox, &locals).interpret(Environment::new(), statements);
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
            "[line 3] Error at \'a\': Cannot read local variable in its own initializer.\n"
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
        assert_eq!(run("fun add(){} print add;"), "f#add\n");
        assert_eq!(run("fun add(){} print add();"), "nil\n");
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

    #[test]
    fn returns() {
        assert_eq!(
            run("fun fibonacci(n) {
                   if (n <= 1) return n;
                   return fibonacci(n - 2) + fibonacci(n - 1);
                 }
                 for (var i = 0; i < 20; i = i + 1) {
                   print fibonacci(i);
                 }"),
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n"
        );
        assert_eq!(
            run("return 123; print 2;"),
            "[line 1] Error at \'return\': Cannot return from top-level code\n"
        );
    }

    #[test]
    fn local_functions() {
        assert_eq!(
            run("fun makeCounter() {
                   var i = 0;
                   fun count() {
                     i = i + 1;
                     print i;
                   }
                   return count;
                 }
                 var counter = makeCounter();
                 counter();
                 counter();"),
            "1\n2\n"
        );
        assert_eq!(
            run("var a = \"global\";
                 {
                   fun showA() {
                     print a;
                   }
                   showA();
                   var a = \"block\";
                   showA();
                 }"),
            "\"global\"\n\"global\"\n"
        );
    }

    #[test]
    fn class() {
        assert_eq!(
            run("class DevonshireCream {
                   serveOn() {
                     return \"Scones\";
                   }
                 }
                 print DevonshireCream;"),
            "DevonshireCream\n"
        );
    }

    #[test]
    fn instance() {
        assert_eq!(
            run("class Bagel {}
                 var bagel = Bagel();
                 print bagel;"),
            "Bagel instance\n"
        );
    }

    #[test]
    fn fields() {
        assert_eq!(
            run("class A {}
                 var a = A();
                 a.c = 1;
                 print a.c;"),
            "1\n"
        );
        assert_eq!(
            run("class A {}
                 var a = A();
                 a.b = a;
                 a.c = 1;
                 print a.c;
                 print a.b.c;"),
            "1\n1\n"
        );
        assert_eq!(
            run("class A {}
                 var a = A();
                 print a.b;"),
            "[line 3] Undefined property \'b\'.\n"
        );
        assert_eq!(
            run("print 1.a;"),
            "[line 1] Only instances have properties.\n"
        );
    }

    #[test]
    fn method() {
        assert_eq!(
            run("class Bacon {
                   eat() {
                     print \"Crunch crunch crunch!\";
                   }
                 }

                 Bacon().eat();"),
            "\"Crunch crunch crunch!\"\n"
        );
        assert_eq!(
            run("fun notMethod(argument) {
                   print \"called function with \" + argument;
                 }
                 class Box {}
                 var box = Box();
                 box.function = notMethod;
                 box.function(\"argument\");"),
            "\"called function with argument\"\n"
        );
    }

    #[test]
    fn this() {
        assert_eq!(
            run("class Cake {
                   taste() {
                     var adjective = \"delicious\";
                     print \"The \" + this.flavor + \" cake is \" + adjective + \"!\";
                   }
                 }
                 var cake = Cake();
                 cake.flavor = \"German chocolate\";
                 cake.taste();"),
            "\"The German chocolate cake is delicious!\"\n"
        );
        assert_eq!(
            run("class Thing {
                   getCallback() {
                     fun localFunction() {
                       print this;
                     }
                     return localFunction;
                   }
                 }
                 var callback = Thing().getCallback();
                 callback();"),
            "Thing instance\n"
        );
        assert_eq!(
            run("print this;"),
            "[line 1] Error at \'this\': Cannot use \'this\' outside of a class\n"
        );
        assert_eq!(
            run("fun notAMethod(){ print this; }
                notAMethod();"),
            "[line 1] Error at \'this\': Cannot use \'this\' outside of a class\n"
        );
    }
    #[test]
    fn constructor() {
        assert_eq!(
            run("class A {init(){this.c = 1;} }
                 var a = A();
                 print a.c;"),
            "1\n"
        );
        assert_eq!(
            run("class A {init(b){this.b = b;} }
                 var a = A(1);
                 print a.b;"),
            "1\n"
        );
        assert_eq!(
            run("class A {init(){print this;} }
                 var a = A();
                 print a.init();"),
            "A instance\nA instance\nnil\n"
        );
        assert_eq!(
            run("class A {init(b){this.b = b;} }
                 A();"),
            "[line 2] Expected 1 arguments but got 0.\n"
        );
    }
}

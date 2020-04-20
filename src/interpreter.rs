use crate::ast::Expr::{self, *};
use crate::ast::Stmt::{self, *};
use crate::environment::EnvRef;
use crate::environment::Environment;
use crate::lox::Lox;
use crate::token::Literal as L;
use crate::token::Token;
use crate::token::TokenType as T;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::result::Result::{Err, Ok};
use std::time::Instant;
use Value::*;

pub type Result<T> = std::result::Result<T, RuntimeException>;

pub struct Interpreter<'a> {
    lox: &'a mut Lox,
    locals: &'a HashMap<Token, usize>,
    env: EnvRef,
    global: EnvRef,
}

impl<'a> Interpreter<'a> {
    pub fn new(lox: &'a mut Lox, locals: &'a HashMap<Token, usize>, env: EnvRef) -> Self {
        let global = env.clone();
        global
            .borrow_mut()
            .define("clock", F(Rc::new(Clock::new())));
        Self {
            lox,
            locals,
            env,
            global,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) {
        for stmt in statements {
            if let Err(e) = self.execute(&stmt) {
                self.lox.runtime_error(e);
                break;
            }
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<()> {
        statements.iter().try_for_each(|s| self.execute(s))
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Block(statements) => {
                let env = Environment::nested(self.env.clone());
                let old_env = self.env.clone();
                self.env = env;
                self.execute_block(&statements)?;
                self.env = old_env;
            }
            Class(name, superclass, methods) => {
                let superclass = if let Some(n) = superclass {
                    if let C(s) = self.evaluate(&Variable(n.clone()))? {
                        Some(s)
                    } else {
                        return err(n, "Superclass must be a class.");
                    }
                } else {
                    None
                };

                self.env.borrow_mut().define(&name.lexeme, V(L::Nil));

                let env = if let Some(s) = &superclass {
                    let e = Environment::nested(self.env.clone());
                    e.borrow_mut().define("super", C(s.clone()));
                    e
                } else {
                    self.env.clone()
                };

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
                                is_initializer: name.lexeme == "init",
                            },
                        );
                    }
                }

                let env = if superclass.is_some() {
                    Environment::unnested(env)
                } else {
                    env
                };

                env.borrow_mut().assign(
                    name,
                    C(Rc::new(Class {
                        name: name.clone(),
                        superclass,
                        methods: ms,
                    })),
                )?;
            }
            Expression(expression) => {
                self.evaluate(&expression).map(|_| ())?;
            }
            Function(name, params, body) => self.env.borrow_mut().define(
                &name.lexeme,
                F(Rc::new(Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.to_vec(),
                    closure: self.env.clone(),
                    is_initializer: false,
                })),
            ),
            If(condition, then, r#else) => {
                if is_truthy(&self.evaluate(&condition)?) {
                    self.execute(&then)?;
                } else if let Some(els) = r#else {
                    self.execute(&els)?;
                }
            }
            Print(expression) => {
                let val = self.evaluate(&expression)?;
                self.lox.println(&val.to_string());
            }
            Return(_keyword, value) => {
                let value = match value {
                    Some(value) => self.evaluate(value)?,
                    None => V(L::Nil),
                };
                return Err(RuntimeException::Return(value));
            }
            Var(name, initializer) => match initializer {
                Some(i) => {
                    let value = self.evaluate(&i)?;
                    self.env.borrow_mut().define(&name.lexeme, value)
                }
                _ => self.env.borrow_mut().define(&name.lexeme, V(L::Nil)),
            },
            While(condition, body) => {
                while is_truthy(&self.evaluate(&condition)?) {
                    self.execute(&body)?
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Asign(name, value) => {
                let value = self.evaluate(value)?;
                if let Some(distance) = self.locals.get(name) {
                    Environment::assign_at(self.env.clone(), *distance, name, value)
                } else {
                    self.global.borrow_mut().assign(name, value)
                }
            }
            Binary(left, op, right) => {
                match (op.typ, self.evaluate(left)?, self.evaluate(right)?) {
                    (T::BangEqual, V(a), V(b)) => Ok(V(L::Bool(a != b))),
                    (T::EqualEqual, V(a), V(b)) => Ok(V(L::Bool(a == b))),
                    (T::Greater, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Bool(a > b))),
                    (T::GreaterEqual, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Bool(a >= b))),
                    (T::Less, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Bool(a < b))),
                    (T::LessEqual, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Bool(a <= b))),
                    (T::Minus, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Number(a - b))),
                    (T::Plus, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Number(a + b))),
                    (T::Plus, V(L::String(a)), V(L::String(b))) => Ok(V(L::String(a + &b))),
                    (T::Plus, _, _) => err(op, "Operands must be two numbers or two strings"),
                    (T::Slash, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Number(a / b))),
                    (T::Star, V(L::Number(a)), V(L::Number(b))) => Ok(V(L::Number(a * b))),
                    _ => err(op, "Operands must be numbers"),
                }
            }
            Call(callee, paren, args) => match self.evaluate(callee)? {
                C(callee) => self.call(callee, args, paren),
                F(callee) => self.call(callee, args, paren),
                _ => err(paren, "Can only call functions and classes."),
            },
            Get(object, name) => {
                if let I(i) = self.evaluate(&object)? {
                    i.get(name)
                } else {
                    err(&name, "Only instances have properties.")
                }
            }
            Grouping(expression) => self.evaluate(&expression),
            Literal(value) => Ok(V(*value.clone())),
            Logical(left, op, right) => {
                let left = self.evaluate(left)?;
                match (op.typ, is_truthy(&left)) {
                    (T::And, false) => Ok(left),
                    (T::Or, true) => Ok(left),
                    _ => self.evaluate(right),
                }
            }
            Unary(op, right) => match (op.typ, self.evaluate(right)?) {
                (T::Bang, r) => Ok(V(L::Bool(!is_truthy(&r)))),
                (T::Minus, V(L::Number(d))) => Ok(V(L::Number(-d))),
                _ => err(op, "Operand must be a number"),
            },
            Set(object, name, value) => {
                if let I(i) = self.evaluate(&object)? {
                    let v = self.evaluate(&value)?;
                    i.set(name, v.clone());
                    Ok(v)
                } else {
                    err(&name, "Only instances have fields.")
                }
            }
            Super(keyword, method) => {
                if let Some(distance) = self.locals.get(keyword) {
                    if let C(superclass) =
                        Environment::get_at(self.env.clone(), *distance, keyword)?
                    {
                        let mut this = keyword.clone();
                        this.lexeme = "this".to_string();
                        if let I(object) =
                            Environment::get_at(self.env.clone(), *distance - 1, &this)?
                        {
                            return if let Some(method) = superclass.find_method(&method.lexeme) {
                                Ok(F(Rc::new(method.bind(object))))
                            } else {
                                err(&method, &format!("Undefined property '{}'.", method.lexeme))
                            };
                        }
                    }
                }
                err(&keyword, "Should be a resolver error")
            }
            This(keyword) => self.lookup_variable(keyword),
            Variable(name) => self.lookup_variable(name),
        }
    }

    fn lookup_variable(&self, name: &Token) -> Result<Value> {
        if let Some(distance) = self.locals.get(name) {
            Environment::get_at(self.env.clone(), *distance, name)
        } else {
            self.global.borrow().get(name)
        }
    }

    fn call(&mut self, callee: Rc<dyn Callable>, args: &[Expr], paren: &Token) -> Result<Value> {
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
                ars.push(self.evaluate(arg)?);
            }
            callee.call(self, paren, &ars)
        }
    }
}

#[derive(Clone)]
pub enum Value {
    V(L),
    F(Rc<dyn Callable>),
    C(Rc<Class>),
    I(Instance),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            V(l) => write!(f, "{}", l),
            F(c) => write!(f, "{}", c),
            I(i) => write!(f, "{}", i),
            C(c) => write!(f, "{}", c),
        }
    }
}

pub trait Callable: fmt::Display {
    fn call(&self, interpreter: &mut Interpreter, paren: &Token, args: &[Value]) -> Result<Value>;
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

    fn call(&self, _: &mut Interpreter, _: &Token, _: &[Value]) -> Result<Value> {
        let now = Instant::now();
        Ok(V(L::Number(now.duration_since(self.start).as_secs_f64())))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "clock")
    }
}

#[derive(Clone)]
struct Function {
    name: Box<Token>,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: EnvRef,
    is_initializer: bool,
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
            is_initializer: self.is_initializer,
        }
    }

    fn this(&self) -> Result<Value> {
        let mut this = self.name.clone();
        this.lexeme = "this".to_string();
        Environment::get_at(self.closure.clone(), 0, &this)
    }
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f#{}", self.name,)
    }
}
impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, _: &Token, args: &[Value]) -> Result<Value> {
        let env = Environment::nested(self.closure.clone());
        let old_env = interpreter.env.clone();
        interpreter.env = env;

        let defs = self.params.iter().zip(args.iter());
        defs.for_each(|(param, arg)| {
            interpreter
                .env
                .borrow_mut()
                .define(&param.lexeme, arg.clone())
        });

        let r = match interpreter.execute_block(&self.body) {
            Err(RuntimeException::Return(v)) => {
                if self.is_initializer {
                    self.this()
                } else {
                    Ok(v)
                }
            }
            Ok(_) => {
                if self.is_initializer {
                    self.this()
                } else {
                    Ok(V(L::Nil))
                }
            }
            Err(e) => Err(e),
        };

        interpreter.env = old_env;
        r
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Clone)]
pub struct Class {
    name: Box<Token>,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Function>,
}
impl Class {
    fn find_method(&self, name: &str) -> Option<Function> {
        if self.methods.contains_key(name) {
            self.methods.get(name).cloned()
        } else if let Some(s) = &self.superclass {
            s.find_method(name)
        } else {
            None
        }
    }
}
impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name,)
    }
}
impl Callable for Class {
    fn call(&self, interpreter: &mut Interpreter, paren: &Token, args: &[Value]) -> Result<Value> {
        let this = Instance::new(self);
        if let Some(init) = self.find_method("init") {
            init.bind(this.clone()).call(interpreter, paren, args)?;
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
    fields: Rc<RefCell<HashMap<String, Value>>>,
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
    Error(Token, String),
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
        V(L::Nil) => false,
        V(L::Bool(b)) => *b,
        _ => true,
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::scanner::Scanner;

    fn run<'a>(source: &'a str) -> String {
        let out = Rc::new(RefCell::new(vec![]));
        let mut lox = Lox::new_t(out.clone());
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(&mut lox);
        let mut parser = Parser::new(&mut lox, tokens);
        let statements = parser.parse();
        if lox.has_error {
            let v = out.borrow().to_vec();
            return String::from_utf8(v).unwrap();
        }
        let locals = Resolver::new(&mut lox).resolve(&statements).locals;
        if lox.has_error {
            let v = out.borrow().to_vec();
            return String::from_utf8(v).unwrap();
        }
        Interpreter::new(&mut lox, &locals, Environment::new()).interpret(statements);
        let v = out.borrow().to_vec();
        return String::from_utf8(v).unwrap();
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
        assert_eq!(run("1"), "[line 1] Error at end: Expect ';' after value.\n");
        assert_eq!(
            run("print 1"),
            "[line 1] Error at end: Expect ';' after value.\n"
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
            "[line 1] Undefined variable 'a'.\n"
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
        assert_eq!(run("a = 1;"), "[line 1] Undefined variable 'a'.\n");
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
            "[line 1] Undefined variable 'c'.\n"
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
            "[line 3] Error at 'a': Cannot read local variable in its own initializer.\n"
        );
        assert_eq!(
            run("{
                   var a = 1;
                 }
                 {
                   a = 2;
                 }"),
            "[line 5] Undefined variable 'a'.\n"
        );
        assert_eq!(
            run("{
                   var a = 1;
                 }
                 a = 2;
                 "),
            "[line 4] Undefined variable 'a'.\n"
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
            "[line 1] Undefined variable 'FAIL'.\n"
        );
        assert_eq!(
            run("while (FAIL) { print 1; } print 2;"),
            "[line 1] Undefined variable 'FAIL'.\n"
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
            "[line 1] Undefined variable 'BREAK'.\n"
        );
        assert_eq!(
            run("for(;true;FAIL) print 0; print 1;"),
            "0\n\
            [line 1] Undefined variable 'FAIL'.\n"
        );
        assert_eq!(
            run("for(FAIL;true;) print 0; print 1;"),
            "[line 1] Undefined variable 'FAIL'.\n"
        );
        assert_eq!(run("var i=10;for(i=0;i<3;i=i+1){}print i;"), "3\n");
        assert_eq!(run("var i=true;for(var i=0;i<3;i=i+1){}print i;"), "true\n");
        assert_eq!(
            run("for(var i=0;i<3;i=i+1){}print i;"),
            "[line 1] Undefined variable 'i'.\n"
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(run("print clock() > 0 and clock() < 0.1;"), "true\n");
        assert_eq!(run("var f = clock; print f() < 0.1;"), "true\n");
        assert_eq!(run("A(B,C);"), "[line 1] Undefined variable 'A'.\n");
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
            "[line 4] Undefined variable 'c'.\n"
        );
        assert_eq!(
            run("fun a() {
                   print c;
                 }
                 {
                   var c = 0;
                   a();
                 }"),
            "[line 2] Undefined variable 'c'.\n"
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
            "[line 1] Error at 'return': Cannot return from top-level code\n"
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
            "[line 3] Undefined property 'b'.\n"
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
            "[line 1] Error at 'this': Cannot use 'this' outside of a class\n"
        );
        assert_eq!(
            run("fun notAMethod(){ print this; }
                notAMethod();"),
            "[line 1] Error at 'this': Cannot use 'this' outside of a class\n"
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
            "A instance\nA instance\nA instance\n"
        );
        assert_eq!(
            run("class A {init(){return;} }
                 var a = A();
                 print a.init();"),
            "A instance\n"
        );
        assert_eq!(
            run("class A {init(){this.b=1;return;this.b=2;} }
                 var a = A();
                 print a.b;"),
            "1\n"
        );
        assert_eq!(
            run("class A {init(){return 1;} }
                 A();"),
            "[line 1] Error at 'return': Cannot return a value from an initializer\n"
        );
        assert_eq!(
            run("class A {init(b){this.b = b;} }
                 A();"),
            "[line 2] Expected 1 arguments but got 0.\n"
        );
    }

    #[test]
    fn superclass() {
        assert_eq!(
            run("class A { }
                 class B < A {}
                 print B;"),
            "B\n"
        );
        assert_eq!(
            run("class A < A { }"),
            "[line 1] Error at 'A': A class cannot inherit from itself.\n"
        );
        assert_eq!(
            run("fun A() { }
                 class B < A {}"),
            "[line 2] Superclass must be a class.\n"
        );
    }

    #[test]
    fn inhertinace() {
        assert_eq!(
            run("class Doughnut {
                   cook() {
                     print \"Fry until golden brown.\";
                   }
                 }
                 class BostonCream < Doughnut {}
                 BostonCream().cook();"),
            "\"Fry until golden brown.\"\n"
        );
    }

    #[test]
    fn super_() {
        assert_eq!(
            run("class A {
                   method() { print \"A method\"; }
                 }
                 class B < A {
                   method() { print \"B method\"; }
                   test() { super.method(); }
                 }
                 class C < B {}
                 C().test();"),
            "\"A method\"\n"
        );
        assert_eq!(
            run("super.m();"),
            "[line 1] Error at 'super': Cannot use 'super' outside of a class\n"
        );
        assert_eq!(
            run("class A { m(){super.cook();} }
                 A().m();"),
            "[line 1] Error at 'super': Cannot use 'super' in a class with not usperclass\n"
        );
    }
}

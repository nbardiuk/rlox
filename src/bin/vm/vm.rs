use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::chunks::OpCode as Op;
use crate::compiler::Compiler;
use crate::out::Out;
use crate::table::Table;
use crate::value::Value;
use Value as V;

const STACK_MAX: usize = 255;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    pub stack: Vec<Value>,
    globals: Table,
    out: Out,
}

macro_rules! binary_number {
    ($self:ident, $t:ident, $f:expr) => {{
        match ($self.pop(), $self.pop()) {
            (Some(V::Number(b)), Some(V::Number(a))) => {
                $self.push(V::$t($f(a, b)));
            }
            _ => {
                $self.runtime_error(format_args!("Operands must be numbers."));
                return InterpretRuntimeError;
            }
        }
    }};
}

impl Vm {
    pub fn new(out: Out) -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX), // FIXME this is not really max
            out,
            globals: Table::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        if let Some(chunk) = Compiler::new(self.out.clone()).compile(source) {
            self.chunk = chunk;
            self.ip = 0;
            self.run()
        } else {
            InterpretCompileError
        }
    }

    fn peek(&self) -> Option<&Value> {
        self.stack.last()
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            #[cfg(feature = "debug-trace")]
            {
                print!("{}", self.debug_stack());
                print!("{}", self.chunk.disasemble_instruction(self.ip));
            }

            let instruction = self.read_byte();
            match instruction {
                Op::Add => match (self.pop(), self.pop()) {
                    (Some(V::Str(b)), Some(V::Str(a))) => {
                        self.push(V::Str(a + b));
                    }
                    (Some(V::Number(b)), Some(V::Number(a))) => {
                        self.push(V::Number(a + b));
                    }
                    _ => {
                        self.runtime_error(format_args!(
                            "Operands must be two numbers or two strings."
                        ));
                        return InterpretRuntimeError;
                    }
                },
                Op::Constant(i) => {
                    let i = *i;
                    self.push(self.read_constant(i));
                }
                Op::DefineGlobal(i) => {
                    if let V::Str(name) = self.read_constant(*i) {
                        if let Some(value) = self.pop() {
                            self.globals.set(name, value);
                        }
                    }
                }
                Op::GetGlobal(i) => {
                    if let V::Str(name) = self.read_constant(*i) {
                        match self.globals.get(&name).cloned() {
                            Some(value) => {
                                self.push(value);
                            }
                            None => {
                                self.runtime_error(format_args!("Undefined variable '{}'.", name));
                                return InterpretRuntimeError;
                            }
                        }
                    }
                }
                Op::SetGlobal(i) => {
                    if let V::Str(name) = self.read_constant(*i) {
                        if let Some(value) = self.peek().cloned() {
                            if self.globals.set(name.clone(), value.clone()) {
                                self.globals.delete(&name);
                                self.runtime_error(format_args!("Undefined variable '{}'.", name));
                                return InterpretRuntimeError;
                            }
                        }
                    }
                }
                Op::SetLocal(i) => {
                    let i = *i;
                    self.stack[i] = self.peek().unwrap().clone()
                }
                Op::GetLocal(i) => {
                    let i = *i;
                    self.push(self.stack[i].clone());
                }
                Op::Divide => binary_number!(self, Number, |a, b| a / b),
                Op::Equal => {
                    let (b, a) = (self.pop(), self.pop());
                    self.push(V::Bool(a == b));
                }
                Op::False => self.push(V::Bool(false)),
                Op::Greater => binary_number!(self, Bool, |a, b| a > b),
                Op::Less => binary_number!(self, Bool, |a, b| a < b),
                Op::Multiply => binary_number!(self, Number, |a, b| a * b),
                Op::Negate => {
                    match self.peek() {
                        Some(V::Number(_)) => {}
                        _ => {
                            self.runtime_error(format_args!("Operand must be a number."));
                            return InterpretRuntimeError;
                        }
                    }
                    if let Some(V::Number(constant)) = self.pop() {
                        self.push(V::Number(-constant));
                    }
                }
                Op::Nil => self.push(V::Nil),
                Op::Not => {
                    if let Some(v) = self.pop() {
                        self.push(V::Bool(is_falsey(&v)));
                    }
                }
                Op::Print => {
                    if let Some(v) = self.pop() {
                        self.out.println(format_args!("{}", v));
                    }
                }
                Op::Pop => {
                    self.pop();
                }
                Op::Return => {
                    return InterpretOk;
                }
                Op::Substract => binary_number!(self, Number, |a, b| a - b),
                Op::True => self.push(V::Bool(true)),
                Op::JumpIfFalse(offset) => {
                    if let Some(v) = self.peek() {
                        if is_falsey(v) {
                            self.ip += *offset;
                        }
                    }
                }
                Op::Jump(offset) => {
                    self.ip += *offset;
                }
            }
            self.ip += 1;
        }
    }

    fn read_constant(&self, i: usize) -> Value {
        self.chunk.constants[i].clone() // FIXME can I use reference here?
    }

    fn read_byte(&self) -> &OpCode {
        &self.chunk.code[self.ip]
    }

    fn runtime_error(&mut self, args: std::fmt::Arguments) {
        self.out.println(format_args!(
            "[line {}] {}",
            self.chunk.lines[self.ip], args
        ));

        self.stack.clear();
    }
}

fn is_falsey(v: &Value) -> bool {
    match v {
        V::Bool(false) | V::Nil => true,
        _ => false,
    }
}

use InterpretResult::*;
pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

#[cfg(test)]
mod spec {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn run(source: &str) -> String {
        let out = Rc::new(RefCell::new(vec![]));
        let mut vm = Vm::new(Out::new(out.clone()));
        vm.interpret(source);
        let v = out.borrow().to_vec();
        String::from_utf8(v).unwrap()
    }

    #[test]
    fn literal() {
        assert_eq!(run("print nil;"), "nil\n");
        assert_eq!(run("print 1;"), "1\n");
        assert_eq!(run("print \"str\";"), "\"str\"\n");
        assert_eq!(run("print true;"), "true\n");
    }

    #[test]
    fn unary_minus() {
        assert_eq!(run("print -1;"), "-1\n");
        assert_eq!(run("print --1;"), "1\n");
        assert_eq!(run("print -false;"), "[line 1] Operand must be a number.\n");
        assert_eq!(run("print -\"\";"), "[line 1] Operand must be a number.\n");
        assert_eq!(run("print -nil;"), "[line 1] Operand must be a number.\n");
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
    }

    #[test]
    fn binary_minus() {
        assert_eq!(run("print 2-1;"), "1\n");
        assert_eq!(run("print 1-2;"), "-1\n");
        assert_eq!(run("print true-1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print nil-1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print 1-\"\";"), "[line 1] Operands must be numbers.\n");
    }

    #[test]
    fn binary_slash() {
        assert_eq!(run("print 1/2;"), "0.5\n");
        assert_eq!(run("print 3/-2;"), "-1.5\n");
        assert_eq!(run("print true/1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print nil/1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print 1/\"\";"), "[line 1] Operands must be numbers.\n");
    }

    #[test]
    fn binary_star() {
        assert_eq!(run("print 2*3;"), "6\n");
        assert_eq!(run("print true*1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print nil*1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print 1*\"\";"), "[line 1] Operands must be numbers.\n");
    }

    #[test]
    fn binary_plus() {
        assert_eq!(run("print 2+1;"), "3\n");
        assert_eq!(run("print \"a\"+\"b\";"), "\"ab\"\n");
        assert_eq!(
            run("print true+1;"),
            "[line 1] Operands must be two numbers or two strings.\n"
        );
        assert_eq!(
            run("print nil+1;"),
            "[line 1] Operands must be two numbers or two strings.\n"
        );
        assert_eq!(
            run("print 1+\"\";"),
            "[line 1] Operands must be two numbers or two strings.\n"
        );
    }

    #[test]
    fn binary_greater() {
        assert_eq!(run("print 2>3;"), "false\n");
        assert_eq!(run("print 2>1;"), "true\n");
        assert_eq!(run("print 1>1;"), "false\n");
        assert_eq!(run("print true>1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print nil>1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print 1>\"\";"), "[line 1] Operands must be numbers.\n");
    }

    #[test]
    fn binary_greater_equal() {
        assert_eq!(run("print 2>=3;"), "false\n");
        assert_eq!(run("print 2>=1;"), "true\n");
        assert_eq!(run("print 1>=1;"), "true\n");
        assert_eq!(
            run("print true>=1;"),
            "[line 1] Operands must be numbers.\n"
        );
        assert_eq!(run("print nil>=1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(
            run("print 1>=\"\";"),
            "[line 1] Operands must be numbers.\n"
        );
    }

    #[test]
    fn binary_less() {
        assert_eq!(run("print 2<3;"), "true\n");
        assert_eq!(run("print 2<1;"), "false\n");
        assert_eq!(run("print 1<1;"), "false\n");
        assert_eq!(run("print true<1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print nil<1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(run("print 1<\"\";"), "[line 1] Operands must be numbers.\n");
    }

    #[test]
    fn binary_less_equal() {
        assert_eq!(run("print 2<=3;"), "true\n");
        assert_eq!(run("print 2<=1;"), "false\n");
        assert_eq!(run("print 1<=1;"), "true\n");
        assert_eq!(
            run("print true<=1;"),
            "[line 1] Operands must be numbers.\n"
        );
        assert_eq!(run("print nil<=1;"), "[line 1] Operands must be numbers.\n");
        assert_eq!(
            run("print 1<=\"\";"),
            "[line 1] Operands must be numbers.\n"
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
    }

    #[test]
    fn grouping() {
        assert_eq!(run("print 3*(1+2);"), "9\n");
        assert_eq!(run("print !(1==2);"), "true\n");
        assert_eq!(
            run("print -(1+nil);"),
            "[line 1] Operands must be two numbers or two strings.\n"
        );
        assert_eq!(
            run("print 2 * (3 / -\"muffin\");"),
            "[line 1] Operand must be a number.\n"
        );
    }

    #[test]
    fn statement_error() {
        assert_eq!(
            run("1"),
            "[line 1] Error at end: Expect ';' after expression.\n"
        );
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
        assert_eq!(
            run("a * b = c + d;"),
            "[line 1] Error at '=': Invalid assignment target.\n"
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
}

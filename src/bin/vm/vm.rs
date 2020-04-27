use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::chunks::OpCode as Op;
use crate::compiler::Compiler;
use crate::value::Value;
use Value as V;

const STACK_MAX: usize = 255;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    pub stack: Vec<Value>,
}

macro_rules! binary_number {
    ($self:ident, $t:ident, $f:expr) => {{
        match ($self.stack.pop(), $self.stack.pop()) {
            (Some(V::Number(b)), Some(V::Number(a))) => {
                $self.stack.push(V::$t($f(a, b)));
            }
            _ => {
                $self.runtime_error("Operands must be numbers.");
                return InterpretRuntimeError;
            }
        }
    }};
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX), // FIXME this is not really max
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        if let Some(chunk) = Compiler::new().compile(source) {
            self.chunk = chunk;
            self.ip = 0;
            self.run()
        } else {
            InterpretCompileError
        }
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
                Op::Add => match (self.stack.pop(), self.stack.pop()) {
                    (Some(V::Str(b)), Some(V::Str(a))) => {
                        self.stack.push(V::Str(a + &b));
                    }
                    (Some(V::Number(b)), Some(V::Number(a))) => {
                        self.stack.push(V::Number(a + b));
                    }
                    _ => {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return InterpretRuntimeError;
                    }
                },
                Op::Constant(i) => {
                    self.stack.push(self.read_constant(*i));
                }
                Op::Divide => binary_number!(self, Number, |a, b| a / b),
                Op::Equal => {
                    let (b, a) = (self.stack.pop(), self.stack.pop());
                    self.stack.push(V::Bool(a == b));
                }
                Op::False => self.stack.push(V::Bool(false)),
                Op::Greater => binary_number!(self, Bool, |a, b| a > b),
                Op::Less => binary_number!(self, Bool, |a, b| a < b),
                Op::Multiply => binary_number!(self, Number, |a, b| a * b),
                Op::Negate => {
                    match self.stack.last() {
                        Some(V::Number(_)) => {}
                        _ => {
                            self.runtime_error("Operand must be a number.");
                            return InterpretRuntimeError;
                        }
                    }
                    if let Some(V::Number(constant)) = self.stack.pop() {
                        self.stack.push(V::Number(-constant));
                    }
                }
                Op::Nil => self.stack.push(V::Nil),
                Op::Not => {
                    if let Some(v) = self.stack.pop() {
                        self.stack.push(V::Bool(is_falsey(v)));
                    }
                }
                Op::Return => {
                    if let Some(constant) = self.stack.pop() {
                        println!("{}", constant);
                    };
                    return InterpretOk;
                }
                Op::Substract => binary_number!(self, Number, |a, b| a - b),
                Op::True => self.stack.push(V::Bool(true)),
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

    // TODO use format args ???
    fn runtime_error(&mut self, message: &str) {
        eprintln!("{}", message);
        eprintln!("[line {}] in script", self.chunk.lines[self.ip]);

        self.stack.clear();
    }
}

fn is_falsey(v: Value) -> bool {
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

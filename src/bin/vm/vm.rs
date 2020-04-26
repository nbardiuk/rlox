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
        match ($self.stack.last(), $self.stack.last()) {
            (Some(V::Number(_)), Some(V::Number(_))) => {}
            _ => {
                $self.runtime_error("Operands must be numbers.");
                return InterpretRuntimeError;
            }
        }

        if let (Some(V::Number(b)), Some(V::Number(a))) = ($self.stack.pop(), $self.stack.pop()) {
            $self.stack.push(V::$t($f(a, b)));
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
                Op::Add => binary_number!(self, Number, |a, b| a + b),
                Op::Constant(i) => {
                    self.stack.push(self.read_constant(*i));
                }
                Op::Nil => self.stack.push(V::Nil),
                Op::True => self.stack.push(V::Bool(true)),
                Op::False => self.stack.push(V::Bool(false)),
                Op::Divide => binary_number!(self, Number, |a, b| a / b),
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
                Op::Return => {
                    if let Some(constant) = self.stack.pop() {
                        println!("{}", constant);
                    };
                    return InterpretOk;
                }
                Op::Substract => binary_number!(self, Number, |a, b| a - b),
            }
            self.ip += 1;
        }
    }

    fn read_constant(&self, i: usize) -> Value {
        self.chunk.constants[i]
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

use InterpretResult::*;
pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

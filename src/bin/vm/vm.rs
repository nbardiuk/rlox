use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::chunks::OpCode::*;
use crate::value::Value;

const STACK_MAX: usize = 255;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    pub stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX), // FIXME this is not really max
        }
    }
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
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
                OpAdd => self.binary(|a, b| a + b),
                OpConstant(i) => {
                    self.stack.push(self.read_constant(*i));
                }
                OpDivide => self.binary(|a, b| a / b),
                OpMultiply => self.binary(|a, b| a * b),
                OpNegate => {
                    if let Some(constant) = self.stack.pop() {
                        self.stack.push(-constant);
                    }
                }
                OpReturn => {
                    if let Some(constant) = self.stack.pop() {
                        println!("{}", constant);
                    };
                    return InterpretResult::InterpretOk;
                }
                OpSubstract => self.binary(|a, b| a - b),
            }
            self.ip += 1;
        }
    }

    fn binary(&mut self, f: fn(Value, Value) -> Value) {
        if let (Some(b), Some(a)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(f(a, b));
        }
    }

    fn read_constant(&self, i: usize) -> Value {
        self.chunk.constants[i]
    }

    fn read_byte(&self) -> &OpCode {
        &self.chunk.code[self.ip]
    }
}

pub fn interpret(source: &str) -> InterpretResult {
    InterpretOk
}

use InterpretResult::*;
pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

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
                OpConstant(i) => {
                    let constant = self.read_constant(*i);
                    self.stack.push(*constant);
                }
                OpReturn => {
                    if let Some(constant) = self.stack.pop() {
                        println!("{}", constant);
                    };
                    return InterpretResult::InterpretOk;
                }
            }
            self.ip += 1;
        }
    }

    fn read_constant(&self, i: usize) -> &Value {
        &self.chunk.constants[i]
    }

    fn read_byte(&self) -> &OpCode {
        &self.chunk.code[self.ip]
    }
}

pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

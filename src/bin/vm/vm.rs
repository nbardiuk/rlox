use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::chunks::OpCode::*;
use crate::value::Value;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
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
            self.chunk.disasemble_instruction(self.ip);

            let instruction = self.read_byte();
            match instruction {
                OpConstant(i) => {
                    let constant = self.read_constant(*i);
                    println!("{}", constant);
                }
                OpReturn => return InterpretResult::InterpretOk,
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

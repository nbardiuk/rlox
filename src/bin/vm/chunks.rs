use crate::value::Value;

#[derive(Clone, Copy)]
pub enum OpCode {
    Add,
    Constant(usize),
    DefineGlobal(usize),
    Divide,
    Equal,
    False,
    GetGlobal(usize),
    Greater,
    Less,
    Multiply,
    Negate,
    Nil,
    Not,
    Print,
    Pop,
    Return,
    Substract,
    True,
}

pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, code: OpCode, line: usize) {
        self.code.push(code);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

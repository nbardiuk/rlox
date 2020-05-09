use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::vm::Vm;
use OpCode as Op;

impl Vm {
    pub fn debug_stack(&self) -> String {
        let mut f = String::from("          ");
        for v in self.stack.iter() {
            f.push_str(&format!("[ {} ]", v));
        }
        f.push('\n');
        f
    }
}

impl Chunk {
    pub fn disasemble_instruction(&self, offset: usize) -> String {
        let instruction = &self.code[offset];
        let line = self.lines[offset];
        let pref_line = if offset > 0 {
            Some(self.lines[offset - 1])
        } else {
            None
        };
        self.di(offset, line, pref_line, instruction)
    }

    pub fn disasemble(&self, name: &str) -> String {
        let mut f = String::new();
        f.push_str(&format!("== {} ==\n", name));
        let mut prev_line = None;
        for (offset, (instruction, line)) in self.code.iter().zip(self.lines.iter()).enumerate() {
            f.push_str(&self.di(offset, *line, prev_line, instruction));
            prev_line = Some(*line);
        }
        f
    }

    fn di(
        &self,
        offset: usize,
        line: usize,
        prev_line: Option<usize>,
        instruction: &OpCode,
    ) -> String {
        let mut f = String::new();

        // Offset
        f.push_str(&format!("{:04} ", offset));

        // Line
        if prev_line == Some(line) {
            f.push_str("   | ");
        } else {
            f.push_str(&format!("{:4} ", line));
        }

        // Instruction
        match instruction {
            Op::Add => f.push_str("OP_ADD"),
            Op::Constant(i) => f.push_str(&self.constant("OP_CONSTANT", *i)),
            Op::DefineGlobal(i) => f.push_str(&self.constant("OP_DEFINE_GLOBAL", *i)),
            Op::Divide => f.push_str("OP_DIVIDE"),
            Op::Equal => f.push_str("OP_EQUAL"),
            Op::False => f.push_str("OP_FALSE"),
            Op::GetGlobal(i) => f.push_str(&self.constant("OP_GET_GLOBAL", *i)),
            Op::GetLocal(i) => f.push_str(&self.byte("OP_GET_LOCAL", *i)),
            Op::Greater => f.push_str("OP_GREATER"),
            Op::Less => f.push_str("OP_LESS"),
            Op::Multiply => f.push_str("OP_MULTIPLY"),
            Op::Negate => f.push_str("OP_NEGATE"),
            Op::Nil => f.push_str("OP_NIL"),
            Op::Not => f.push_str("OP_NOT"),
            Op::Pop => f.push_str("OP_POP"),
            Op::Print => f.push_str("OP_PRINT"),
            Op::Return => f.push_str("OP_RETURN"),
            Op::SetGlobal(i) => f.push_str(&self.constant("OP_SET_GLOBAL", *i)),
            Op::SetLocal(i) => f.push_str(&self.byte("OP_SET_LOCAL", *i)),
            Op::Substract => f.push_str("OP_SUBSTRACT"),
            Op::True => f.push_str("OP_TRUE"),
        };
        f.push('\n');
        f
    }

    fn constant(&self, name: &str, i: usize) -> String {
        format!("{:16} {:04} '{}'", name, i, self.constants[i])
    }

    fn byte(&self, name: &str, i: usize) -> String {
        format!("{:16} {:04}", name, i)
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    use crate::out::Out;
    use crate::value::Value as V;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn stack() {
        let out = Out::new(Rc::new(RefCell::new(vec![])));
        let mut vm = Vm::new(out);
        vm.stack.push(V::Number(1.));
        vm.stack.push(V::Number(2.2));
        vm.stack.push(V::Number(3.1));

        assert_eq!(vm.debug_stack(), "          [ 1 ][ 2.2 ][ 3.1 ]\n")
    }

    #[test]
    fn chunk() {
        let mut c = Chunk::new();
        let i = c.add_constant(V::Number(1.2));
        c.write(Op::Constant(i), 123);
        c.write(Op::Return, 123);

        assert_eq!(
            c.disasemble("test chunk"),
            "== test chunk ==\n\
             0000  123 OP_CONSTANT      0000 '1.2'\n\
             0001    | OP_RETURN\n"
        )
    }
}

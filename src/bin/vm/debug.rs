use crate::chunks::Chunk;
use crate::chunks::OpCode;
use crate::vm::Vm;
use OpCode::*;

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
            OpAdd => f.push_str("OP_ADD"),
            OpConstant(i) => {
                f.push_str(&format!(
                    "{:16} {:04} '{}'",
                    "OP_CONSTANT", i, self.constants[*i]
                ));
            }
            OpDivide => f.push_str("OP_DIVIDE"),
            OpMultiply => f.push_str("OP_MULTIPLY"),
            OpNegate => f.push_str("OP_NEGATE"),
            OpReturn => f.push_str("OP_RETURN"),
            OpSubstract => f.push_str("OP_SUBSTRACT"),
        };
        f.push('\n');
        f
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn stack() {
        let mut vm = Vm::new();
        vm.stack.push(1.);
        vm.stack.push(2.2);
        vm.stack.push(3.1);

        assert_eq!(vm.debug_stack(), "          [ 1 ][ 2.2 ][ 3.1 ]\n")
    }

    #[test]
    fn chunk() {
        let mut c = Chunk::new();
        let i = c.add_constant(1.2);
        c.write(OpConstant(i), 123);
        c.write(OpReturn, 123);

        assert_eq!(
            c.disasemble("test chunk"),
            "== test chunk ==\n\
             0000  123 OP_CONSTANT      0000 '1.2'\n\
             0001    | OP_RETURN\n"
        )
    }
}

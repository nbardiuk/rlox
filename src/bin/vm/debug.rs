use crate::chunks::Chunk;
use crate::chunks::OpCode;
use OpCode::*;

impl Chunk {
    pub fn disasemble_instruction(&self, offset: usize) {
        let instruction = &self.code[offset];
        let line = self.lines[offset];
        let pref_line = if offset > 0 {
            Some(self.lines[offset - 1])
        } else {
            None
        };
        print!("{}", self.di(offset, line, pref_line, instruction));
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
            OpReturn => f.push_str("OP_RETURN"),
            OpConstant(i) => {
                f.push_str(&format!(
                    "{:16} {:04} '{}'",
                    "OP_CONSTANT", i, self.constants[*i]
                ));
            }
        };
        f.push('\n');
        f
    }
}

#[cfg(test)]
mod spec {
    use super::*;

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

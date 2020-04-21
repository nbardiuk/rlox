use crate::chunks::Chunk;
use crate::chunks::OpCode;
use OpCode::*;

impl Chunk {
    pub fn disasemble(&self, name: &str) -> String {
        let mut f = String::new();
        f.push_str(&format!("== {} ==\n", name));
        let mut prev_line = 0;
        for (offset, (code, line)) in self.code.iter().zip(self.lines.iter()).enumerate() {
            // Offset
            f.push_str(&format!("{:04} ", offset));

            // Line
            if offset > 0 && &prev_line == line {
                f.push_str("   | ");
            } else {
                f.push_str(&format!("{:4} ", line));
            }
            prev_line = *line;

            // Code
            match code {
                OpReturn => f.push_str("OP_RETURN"),
                OpConstant(i) => {
                    f.push_str(&format!(
                        "{:16} {:04} '{}'",
                        "OP_CONSTANT", i, self.constants[*i]
                    ));
                }
            };
            f.push('\n');
        }
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

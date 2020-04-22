use crate::chunks::Chunk;
use crate::chunks::OpCode::*;
use crate::vm::Vm;
use std::io;
use std::process;

mod chunks;
mod debug;
mod value;
mod vm;

fn main() -> io::Result<()> {
    let mut c = Chunk::new();
    let i = c.add_constant(1.2);
    c.write(OpConstant(i), 123);
    c.write(OpReturn, 123);

    let mut vm = Vm::new();
    vm.interpret(c);

    process::exit(0)
}

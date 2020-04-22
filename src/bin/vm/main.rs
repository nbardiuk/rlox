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

    let i = c.add_constant(3.4);
    c.write(OpConstant(i), 123);

    c.write(OpAdd, 123);

    let i = c.add_constant(5.6);
    c.write(OpConstant(i), 123);

    c.write(OpDivide, 123);

    c.write(OpNegate, 123);

    c.write(OpReturn, 123);

    let mut vm = Vm::new();
    vm.interpret(c);

    process::exit(0)
}

use crate::vm::InterpretResult::*;
use crate::vm::Vm;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;
mod chunks;
mod compiler;
mod debug;
mod scanner;
mod table;
mod value;
mod vm;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args().skip(1).collect();
    match args.len() {
        0 => repl(),
        1 => run_file(&args[0]),
        _ => {
            println!("Usage: vm [script]");
            process::exit(64)
        }
    }
}

fn repl() -> io::Result<()> {
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        Vm::new().interpret(&line);
    }
}

fn run_file(path: &str) -> io::Result<()> {
    let source = &fs::read_to_string(path)?;

    let result = Vm::new().interpret(source);

    match result {
        InterpretCompileError => process::exit(65),
        InterpretRuntimeError => process::exit(70),
        InterpretOk => Ok(()),
    }
}

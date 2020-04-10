mod expr;
mod lox;
mod parser;
mod scanner;
mod token;

use crate::lox::Lox;
use std::env;
use std::io;
use std::process;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args().skip(1).collect();
    let mut lox = Lox::<io::Stdout>::new();
    match args.len() {
        0 => lox.run_prompt(),
        1 => lox.run_file(&args[0]),
        _ => {
            println!("Usage: rlox [script]");
            process::exit(64)
        }
    }
}

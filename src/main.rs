use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args().skip(1).collect();
    match args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            println!("Usage: rlox [script]");
            process::exit(64)
        }
    }
}

fn run_file(path: &str) -> io::Result<()> {
    run(&fs::read_to_string(path)?);
    io::Result::Ok(())
}

fn run_prompt() -> io::Result<()> {
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        run(&input)
    }
}

fn run(source: &str) {
    let tokens = scan_tokens(source);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn scan_tokens(source: &str) -> Vec<Token> {
    vec![]
}

#[derive(Debug)]
struct Token {}

use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args().skip(1).collect();
    let mut lox = Lox { has_error: false };
    match args.len() {
        0 => lox.run_prompt(),
        1 => lox.run_file(&args[0]),
        _ => {
            println!("Usage: rlox [script]");
            process::exit(64)
        }
    }
}

fn scan_tokens(source: &str) -> Vec<Token> {
    vec![]
}

#[derive(Debug)]
struct Token {}

struct Lox {
    has_error: bool,
}

impl Lox {
    fn run(&self, source: &str) {
        let tokens = scan_tokens(source);
        for token in tokens {
            println!("{:?}", token);
        }
    }
    fn run_file(&self, path: &str) -> io::Result<()> {
        self.run(&fs::read_to_string(path)?);
        if self.has_error {
            process::exit(65)
        };
        io::Result::Ok(())
    }

    fn run_prompt(&mut self) -> io::Result<()> {
        loop {
            print!("> ");
            io::stdout().flush()?;

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            self.run(&input);
            self.has_error = false;
        }
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }
    fn report(&mut self, line: usize, location: &str, message: &str) {
        println!("[line {}] Error{}: {}", line, location, message);
        self.has_error = true;
    }
}

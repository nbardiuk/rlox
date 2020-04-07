use std::fs;
use std::io;
use std::io::Write;
use std::process;

use crate::scanner::scan_tokens;

pub struct Lox {
    has_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { has_error: false }
    }

    fn run(&self, source: &str) {
        let tokens = scan_tokens(source);
        for token in tokens {
            println!("{}", token);
        }
    }

    pub fn run_file(&self, path: &str) -> io::Result<()> {
        self.run(&fs::read_to_string(path)?);
        if self.has_error {
            process::exit(65)
        };
        io::Result::Ok(())
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
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

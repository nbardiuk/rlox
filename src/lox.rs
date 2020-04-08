use std::fs;
use std::io;
use std::io::Write;
use std::process;

use crate::scanner::Scanner;

pub struct Lox<W: Write> {
    pub has_error: bool,
    out: W,
}

impl Lox<io::Stdout> {
    pub fn new() -> Self {
        Self {
            has_error: false,
            out: io::stdout(),
        }
    }
}

impl Lox<Vec<u8>> {
    pub fn new() -> Self {
        Self {
            has_error: false,
            out: vec![],
        }
    }

    pub fn output(&self) -> String {
        String::from_utf8(self.out.clone()).unwrap()
    }
}

impl<W: Write> Lox<W> {
    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(self);
        for token in tokens {
            println!("{}", token);
        }
    }

    pub fn run_file(&mut self, path: &str) -> io::Result<()> {
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

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, location: &str, message: &str) {
        self.out
            .write_fmt(format_args!(
                "[line {}] Error{}: {}\n",
                line, location, message
            ))
            .unwrap();
        self.has_error = true;
    }
}

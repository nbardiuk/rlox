use crate::interpreter::interpret;
use crate::interpreter::RuntimeError;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::Token;
use crate::token::TokenType;
use std::fs;
use std::io::{self, Write};
use std::process;

pub struct Lox<W: Write> {
    pub has_error: bool,
    pub has_runtime_error: bool,
    out: W,
}

impl Lox<io::Stdout> {
    pub fn new() -> Self {
        Self {
            has_error: false,
            has_runtime_error: false,
            out: io::stdout(),
        }
    }
}

impl Lox<Vec<u8>> {
    pub fn new() -> Self {
        Self {
            has_error: false,
            has_runtime_error: false,
            out: vec![],
        }
    }

    pub fn output(&self) -> String {
        String::from_utf8(self.out.clone()).unwrap()
    }
}

impl<W: Write> Lox<W> {
    fn run(&mut self, source: &str) -> io::Result<()> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(self);
        let mut parser = Parser::new(self, tokens);
        if let Some(expr) = parser.parse() {
            interpret(self, expr)
        } else {
            Result::Ok(())
        }
    }

    pub fn run_file(&mut self, path: &str) -> io::Result<()> {
        self.run(&fs::read_to_string(path)?)?;
        if self.has_error {
            process::exit(65)
        };
        if self.has_runtime_error {
            process::exit(70)
        };
        io::Result::Ok(())
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
        loop {
            self.print("> ")?;
            self.out.flush()?;

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            self.run(&input)?;
            self.has_error = false;
        }
    }

    pub fn print(&mut self, message: &str) -> io::Result<()> {
        self.out.write_fmt(format_args!("{}", message))
    }

    pub fn println(&mut self, message: &str) -> io::Result<()> {
        self.out.write_fmt(format_args!("{}\n", message))
    }

    pub fn runtime_error(&mut self, e: RuntimeError) -> io::Result<()> {
        self.has_runtime_error = true;
        self.println(&format!("[line {}] {}", e.token.line, e.message))
    }

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message).unwrap()
    }

    pub fn error_token(&mut self, token: Token, message: &str) {
        match token.typ {
            TokenType::EOF => self.report(token.line, " at end", message),
            _ => self.report(token.line, &format!(" at '{}'", token.lexeme), message),
        }
        .unwrap()
    }

    fn report(&mut self, line: usize, location: &str, message: &str) -> io::Result<()> {
        self.has_error = true;
        self.println(&format!("[line {}] Error{}: {}", line, location, message))
    }
}
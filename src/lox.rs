use crate::environment::EnvRef;
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeException;
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use crate::token::Token;
use crate::token::TokenType;
use std::cell::RefCell;
use std::fs;
use std::io::{self, Write};
use std::process;
use std::rc::Rc;

pub struct Lox {
    pub has_error: bool,
    pub has_runtime_error: bool,
    out: Rc<RefCell<dyn Write>>,
}

impl Lox {
    pub fn new() -> Self {
        Lox::new_t(Rc::new(RefCell::new(io::stdout())))
    }

    pub fn new_t(out: Rc<RefCell<dyn Write>>) -> Self {
        Self {
            has_error: false,
            has_runtime_error: false,
            out,
        }
    }
}

impl Lox {
    fn run(&mut self, env: EnvRef, source: &str) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(self);
        if self.has_error {
            return;
        }

        let mut parser = Parser::new(self, tokens);
        let statements = parser.parse();
        if self.has_error {
            return;
        }

        let locals = Resolver::new(self).resolve(&statements).locals;

        Interpreter::new(self, &locals).interpret(env, statements);
    }

    pub fn run_file(&mut self, path: &str) -> io::Result<()> {
        let env = Environment::new();
        self.run(env, &fs::read_to_string(path)?);
        if self.has_error {
            process::exit(65)
        };
        if self.has_runtime_error {
            process::exit(70)
        };
        Ok(())
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
        let env = Environment::new();
        loop {
            self.print("> ")?;
            self.out.borrow_mut().flush()?;

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            self.run(env.clone(), &input);
            self.has_error = false;
        }
    }

    pub fn print(&mut self, message: &str) -> io::Result<()> {
        self.out.borrow_mut().write_fmt(format_args!("{}", message))
    }

    pub fn println(&mut self, message: &str) {
        self.out
            .borrow_mut()
            .write_fmt(format_args!("{}\n", message))
            .unwrap()
    }

    pub fn runtime_error(&mut self, e: RuntimeException) {
        self.has_runtime_error = true;
        use RuntimeException::*;
        match e {
            Error(token, message) => self.println(&format!("[line {}] {}", token.line, message)),
            Return(_) => {}
        }
    }

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message)
    }

    pub fn error_token(&mut self, token: &Token, message: &str) {
        match token.typ {
            TokenType::EOF => self.report(token.line, " at end", message),
            _ => self.report(token.line, &format!(" at '{}'", token.lexeme), message),
        }
    }

    fn report(&mut self, line: usize, location: &str, message: &str) {
        self.has_error = true;
        self.println(&format!("[line {}] Error{}: {}", line, location, message))
    }
}

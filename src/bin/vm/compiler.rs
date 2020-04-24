use crate::scanner::Scanner;
use crate::scanner::TokenType::*;

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!("{:?} '{}'", token.typ, token.lexeme);

        if token.typ == Eof {
            break;
        }
    }
}

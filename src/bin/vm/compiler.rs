use crate::chunks::Chunk;
use crate::chunks::OpCode as Op;
use crate::chunks::OpCode;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType as T;
use crate::value::ObjString;
use crate::value::Value;
use Precedence as Prec;
use Value as V;

pub struct Compiler<'s> {
    scanner: Scanner<'s>,
    previous: Option<Token<'s>>,
    current: Option<Token<'s>>,
    has_error: bool,
    panic_mode: bool,
    compiling_chunk: Chunk,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Self {
        Self {
            compiling_chunk: Chunk::new(),
            scanner: Scanner::new(""),
            previous: None,
            current: None,
            has_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&mut self, source: &'s str) -> Option<Chunk> {
        self.scanner = Scanner::new(source);

        self.advance();
        self.expression();
        self.consume(T::Eof, "Expected end of expressoin.");
        self.end_compiler();

        if self.has_error {
            None
        } else {
            let mut result = Chunk::new();
            std::mem::swap(&mut self.compiling_chunk, &mut result);
            Some(result)
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::Assignment);
    }

    fn number(&mut self) {
        let value = self
            .previous
            .as_ref()
            .map(|t| t.lexeme)
            .unwrap_or_default()
            .parse()
            .unwrap_or_default();
        self.emit_constant(V::Number(value));
    }

    fn literal(&mut self) {
        match self.previous_type() {
            Some(T::False) => self.emit_code(Op::False),
            Some(T::Nil) => self.emit_code(Op::Nil),
            Some(T::True) => self.emit_code(Op::True),
            _ => panic!("Unreachable."),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(T::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.previous_type().unwrap_or(T::Error);

        // compile the operand
        self.parse_precedence(Prec::Unary);

        // emit the operator instruction
        match operator_type {
            T::Bang => self.emit_code(Op::Not),
            T::Minus => self.emit_code(Op::Negate),
            _ => panic!("Unreachable."),
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous_type().unwrap_or(T::Error);

        // right operand
        let precedence = Precedence::for_type(operator_type);
        self.parse_precedence(precedence.next());

        // emit the operator instruction
        match operator_type {
            T::Plus => self.emit_code(Op::Add),
            T::Minus => self.emit_code(Op::Substract),
            T::Star => self.emit_code(Op::Multiply),
            T::Slash => self.emit_code(Op::Divide),
            T::EqualEqual => self.emit_code(Op::Equal),
            T::BangEqual => self.emit_codes(&[Op::Equal, Op::Not]),
            T::Greater => self.emit_code(Op::Greater),
            T::GreaterEqual => self.emit_codes(&[Op::Greater, Op::Not]),
            T::Less => self.emit_code(Op::Less),
            T::LessEqual => self.emit_codes(&[Op::Less, Op::Not]),
            _ => panic!("Unreachable."),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let operator_type = self.previous_type().unwrap_or(T::Error);
        let ok = self.prefix(operator_type, "Expect expression.");
        if !ok {
            return;
        }

        while precedence <= Precedence::for_type(self.current_type().unwrap_or(T::Error)) {
            self.advance();
            self.infix(self.previous_type().unwrap_or(T::Error));
        }
    }

    fn string(&mut self) {
        let lexeme = self.previous.as_ref().map(|t| t.lexeme).unwrap_or_default();
        self.emit_constant(V::Str(ObjString::new(&lexeme[1..lexeme.len() - 1])));
    }

    fn previous_type(&self) -> Option<TokenType> {
        self.previous.as_ref().map(|t| t.typ)
    }

    fn current_type(&self) -> Option<TokenType> {
        self.current.as_ref().map(|t| t.typ)
    }

    fn prefix(&mut self, t: TokenType, message: &str) -> bool {
        match t {
            T::LeftParen => self.grouping(),
            T::Minus | T::Bang => self.unary(),
            T::Number => self.number(),
            T::False | T::True | T::Nil => self.literal(),
            T::String => self.string(),
            _ => {
                self.error(message);
                return false;
            }
        }
        true
    }

    fn infix(&mut self, t: TokenType) {
        match t {
            T::BangEqual
            | T::EqualEqual
            | T::Greater
            | T::GreaterEqual
            | T::Less
            | T::LessEqual
            | T::Minus
            | T::Plus
            | T::Slash
            | T::Star => self.binary(),
            _ => {}
        }
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);
        loop {
            self.current = Some(self.scanner.scan_token());
            if self.current_type() != Some(T::Error) {
                break;
            } else {
                let message = self.current.as_ref().map(|t| t.lexeme).unwrap_or_default();
                self.error_at_current(message);
            };
        }
    }

    fn consume(&mut self, typ: TokenType, message: &str) {
        if Some(typ) == self.current_type() {
            self.advance();
            return;
        }
        self.error_at_current(message)
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        #[cfg(feature = "debug-trace")]
        {
            if !self.has_error {
                print!("{}", self.current_chunk().disasemble("code"))
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_code(Op::Return)
    }

    fn emit_code(&mut self, code: OpCode) {
        let line = self.previous.as_ref().map(|t| t.line).unwrap_or_default();
        self.current_chunk().write(code, line)
    }

    fn emit_codes(&mut self, codes: &[OpCode]) {
        for code in codes {
            self.emit_code(*code);
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let i = self.current_chunk().add_constant(value);
        self.emit_code(Op::Constant(i));
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiling_chunk
    }

    fn error(&mut self, message: &str) {
        if let Some(token) = self.previous.as_ref() {
            self.error_at(token.typ, token.lexeme, token.line, message);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        if let Some(token) = self.current.as_ref() {
            self.error_at(token.typ, token.lexeme, token.line, message);
        }
    }

    fn error_at(&mut self, typ: TokenType, lexeme: &str, line: usize, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        let position = match typ {
            T::Eof => " at end".to_string(),
            T::Error => "".to_string(),
            _ => format!(" at '{}'", lexeme),
        };
        println!("[line {}] Error{}: {}", line, position, message);
        self.has_error = true;
    }
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // > >= < <=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => None,
        }
    }

    fn for_type(t: TokenType) -> Precedence {
        use Precedence as P;
        match t {
            T::Minus | T::Plus => P::Term,
            T::Slash | T::Star => P::Factor,
            T::BangEqual | T::EqualEqual => P::Equality,
            T::Greater | T::GreaterEqual | T::Less | T::LessEqual => P::Comparison,
            _ => P::None,
        }
    }
}

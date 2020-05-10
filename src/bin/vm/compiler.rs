use crate::chunks::Chunk;
use crate::chunks::OpCode as Op;
use crate::chunks::OpCode;
use crate::out::Out;
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
    out: Out,
    locals: Vec<Local<'s>>,
    scope_depth: usize,
}

impl<'s> Compiler<'s> {
    pub fn new(out: Out) -> Self {
        Self {
            compiling_chunk: Chunk::new(),
            scanner: Scanner::new(""),
            previous: None,
            current: None,
            has_error: false,
            panic_mode: false,
            out,
            locals: vec![],
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self, source: &'s str) -> Option<Chunk> {
        self.scanner = Scanner::new(source);

        self.advance();
        while !self.matches(T::Eof) {
            self.declaration();
        }
        self.end_compiler();

        if self.has_error {
            None
        } else {
            let mut result = Chunk::new();
            std::mem::swap(&mut self.compiling_chunk, &mut result);
            Some(result)
        }
    }

    fn declaration(&mut self) {
        if self.matches(T::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(T::Equal) {
            self.expression();
        } else {
            self.emit_code(Op::Nil);
        }

        self.consume(T::Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(T::Identifier, error_message);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }

        let name = self.previous.as_ref().map(|n| n.lexeme).unwrap_or_default();
        let name = V::Str(ObjString::new(name));
        self.current_chunk().add_constant(name)
    }

    fn define_variable(&mut self, global: usize) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_code(Op::DefineGlobal(global))
    }

    fn mark_initialized(&mut self) {
        if let Some(l) = self.locals.last_mut() {
            l.depth.replace(self.scope_depth);
        }
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        let name = self.previous.as_ref().map(|n| n.lexeme).unwrap_or_default();
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.depth.filter(|d| d < &self.scope_depth).is_some() {
                break;
            }
            if name == local.name {
                self.error("Variable with this name already declared in this scope.");
                break;
            }
        }
        self.add_local(name);
    }

    fn add_local(&mut self, name: &'s str) {
        self.locals.push(Local { name, depth: None });
    }

    fn variable(&mut self, can_assign: bool) {
        let name = self.previous.as_ref().map(|n| n.lexeme).unwrap_or_default();

        let get_op;
        let set_op;
        if let Some(i) = self.resolve_local(&name) {
            get_op = Op::GetLocal(i);
            set_op = Op::SetLocal(i);
        } else {
            let name = V::Str(ObjString::new(name));
            let i = self.current_chunk().add_constant(name);
            get_op = Op::GetGlobal(i);
            set_op = Op::SetGlobal(i);
        }

        if can_assign && self.matches(T::Equal) {
            self.expression();
            self.emit_code(set_op)
        } else {
            self.emit_code(get_op)
        }
    }

    fn resolve_local(&mut self, name: &'s str) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if name == local.name {
                if local.depth.is_none() {
                    self.error("Cannot read local variable in its own initializer.");
                }
                return Some(i);
            }
        }
        None
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current_type() != Some(T::Eof) {
            if self.previous_type() == Some(T::Semicolon) {
                return;
            }
            match self.current_type() {
                Some(T::Class) | Some(T::Fun) | Some(T::Var) | Some(T::For) | Some(T::If)
                | Some(T::While) | Some(T::Print) | Some(T::Return) => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn statement(&mut self) {
        if self.matches(T::Print) {
            self.print_statement();
        } else if self.matches(T::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while let Some(last) = self.locals.last() {
            if last.depth.unwrap_or(0) > self.scope_depth {
                self.locals.pop();
                self.emit_code(Op::Pop);
            } else {
                break;
            }
        }
    }

    fn block(&mut self) {
        while !self.check(T::RightBrace) && !self.check(T::Eof) {
            self.declaration();
        }
        self.consume(T::RightBrace, "Expect '}' after block.");
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(T::Semicolon, "Expect ';' after value.");
        self.emit_code(Op::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(T::Semicolon, "Expect ';' after expression.");
        self.emit_code(Op::Pop);
    }

    fn matches(&mut self, t: TokenType) -> bool {
        if !self.check(t) {
            return false;
        };
        self.advance();
        true
    }

    fn check(&self, t: TokenType) -> bool {
        self.current_type() == Some(t)
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::Assignment);
    }

    fn number(&mut self, _can_assign: bool) {
        let value = self
            .previous
            .as_ref()
            .map(|t| t.lexeme)
            .unwrap_or_default()
            .parse()
            .unwrap_or_default();
        self.emit_constant(V::Number(value));
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous_type() {
            Some(T::False) => self.emit_code(Op::False),
            Some(T::Nil) => self.emit_code(Op::Nil),
            Some(T::True) => self.emit_code(Op::True),
            _ => panic!("Unreachable."),
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(T::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _can_assign: bool) {
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

    fn binary(&mut self, _can_assign: bool) {
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
            T::GreaterEqual => self.emit_codes(&[Op::Less, Op::Not]),
            T::Less => self.emit_code(Op::Less),
            T::LessEqual => self.emit_codes(&[Op::Greater, Op::Not]),
            _ => panic!("Unreachable."),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign = precedence <= Precedence::Assignment;
        let operator_type = self.previous_type().unwrap_or(T::Error);
        let ok = self.prefix(operator_type, can_assign, "Expect expression.");
        if !ok {
            return;
        }

        while precedence <= Precedence::for_type(self.current_type().unwrap_or(T::Error)) {
            self.advance();
            self.infix(self.previous_type().unwrap_or(T::Error), can_assign);
        }

        if can_assign && self.matches(T::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn string(&mut self, _can_assign: bool) {
        let lexeme = self.previous.as_ref().map(|t| t.lexeme).unwrap_or_default();
        self.emit_constant(V::Str(ObjString::new(&lexeme[1..lexeme.len() - 1])));
    }

    fn previous_type(&self) -> Option<TokenType> {
        self.previous.as_ref().map(|t| t.typ)
    }

    fn current_type(&self) -> Option<TokenType> {
        self.current.as_ref().map(|t| t.typ)
    }

    fn prefix(&mut self, t: TokenType, can_assign: bool, message: &str) -> bool {
        match t {
            T::LeftParen => self.grouping(can_assign),
            T::Minus | T::Bang => self.unary(can_assign),
            T::Number => self.number(can_assign),
            T::False | T::True | T::Nil => self.literal(can_assign),
            T::String => self.string(can_assign),
            T::Identifier => self.variable(can_assign),
            _ => {
                self.error(message);
                return false;
            }
        }
        true
    }

    fn infix(&mut self, t: TokenType, can_assign: bool) {
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
            | T::Star => self.binary(can_assign),
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
            let typ = token.typ;
            let lexeme = token.lexeme;
            let line = token.line;
            self.error_at(typ, lexeme, line, message);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        if let Some(token) = self.current.as_ref() {
            let typ = token.typ;
            let lexeme = token.lexeme;
            let line = token.line;
            self.error_at(typ, lexeme, line, message);
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
        self.out.println(format_args!(
            "[line {}] Error{}: {}",
            line, position, message
        ));
        self.has_error = true;
    }
}

struct Local<'s> {
    name: &'s str,
    depth: Option<usize>,
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

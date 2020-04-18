use crate::ast::Expr::{self, *};
use crate::ast::Stmt::{self, *};
use crate::lox::Lox;
use crate::token::Token;
use std::collections::HashMap;
use std::io::Write;

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    pub locals: HashMap<Expr, usize>,
    function: FunctionType,
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            locals: HashMap::default(),
            function: FunctionType::None,
        }
    }

    pub fn resolve_stmts<W: Write>(&mut self, lox: &mut Lox<W>, statements: &[Stmt]) {
        for statement in statements {
            self.resolve_stmt(lox, statement);
        }
    }

    fn resolve_stmt<W: Write>(&mut self, lox: &mut Lox<W>, statement: &Stmt) {
        match statement {
            Block(statements) => {
                self.begin_scope();
                self.resolve_stmts(lox, statements);
                self.end_scope();
            }
            Class(name, _methods) => {
                self.declare(lox, name);
                self.define(name);
            }
            Expression(expression) => {
                self.resolve_expr(lox, expression);
            }
            Function(name, params, body) => {
                self.declare(lox, name);
                self.define(name);
                self.resolve_function(lox, params, body, FunctionType::Function)
            }
            If(condition, then, r#else) => {
                self.resolve_expr(lox, condition);
                self.resolve_stmt(lox, then);
                if let Some(e) = r#else {
                    self.resolve_stmt(lox, e);
                }
            }
            Var(name, initializer) => {
                self.declare(lox, name);
                if let Some(i) = initializer {
                    self.resolve_expr(lox, &i);
                }
                self.define(name);
            }
            Print(expression) => {
                self.resolve_expr(lox, expression);
            }
            Return(keyword, value) => {
                if self.function == FunctionType::None {
                    lox.error_token(keyword, "Cannot return from top-level code");
                }
                if let Some(value) = value {
                    self.resolve_expr(lox, value);
                }
            }
            While(condition, body) => {
                self.resolve_expr(lox, condition);
                self.resolve_stmt(lox, body);
            }
        }
    }

    fn resolve_expr<W: Write>(&mut self, lox: &mut Lox<W>, expr: &Expr) {
        match expr {
            Asign(name, value) => {
                self.resolve_expr(lox, value);
                self.resolve_local(name, expr);
            }
            Binary(left, _op, right) => {
                self.resolve_expr(lox, left);
                self.resolve_expr(lox, right);
            }
            Call(callee, _paren, args) => {
                self.resolve_expr(lox, callee);
                for arg in args {
                    self.resolve_expr(lox, arg);
                }
            }
            Get(object, _name) => {
                self.resolve_expr(lox, object);
            }
            Grouping(expression) => {
                self.resolve_expr(lox, expression);
            }
            Literal(_value) => {}
            Logical(left, _op, right) => {
                self.resolve_expr(lox, left);
                self.resolve_expr(lox, right);
            }
            Variable(name) => {
                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(false) = scope.get(&name.lexeme) {
                        lox.error_token(name, "Cannot read local variable in its own initializer.");
                    }
                }
                self.resolve_local(name, expr);
            }
            Set(object, _name, value) => {
                self.resolve_expr(lox, value);
                self.resolve_expr(lox, object);
            }
            Unary(_op, right) => {
                self.resolve_expr(lox, right);
            }
        }
    }

    fn resolve_function<W: Write>(
        &mut self,
        lox: &mut Lox<W>,
        params: &[Token],
        body: &[Stmt],
        function: FunctionType,
    ) {
        let enclosing_functoin = self.function;
        self.function = function;
        self.begin_scope();
        for param in params {
            self.declare(lox, param);
            self.define(param);
        }
        self.resolve_stmts(lox, body);
        self.end_scope();
        self.function = enclosing_functoin;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_local(&mut self, name: &Token, expr: &Expr) {
        let mut distance = 0;
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(&name.lexeme) {
                self.locals.insert(expr.clone(), distance);
                return;
            }
            distance += 1;
        }
    }

    fn declare<W: Write>(&mut self, lox: &mut Lox<W>, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                lox.error_token(
                    name,
                    "Variable with this name already declared in this scope.",
                );
                return;
            }
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }
}

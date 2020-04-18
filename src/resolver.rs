use crate::ast::Expr::{self, *};
use crate::ast::Stmt::{self, *};
use crate::lox::Lox;
use crate::token::Token;
use std::collections::HashMap;
use std::io::Write;

pub struct Resolver<'a, W: Write> {
    scopes: Vec<HashMap<String, bool>>,
    pub locals: HashMap<Expr, usize>,
    function: FunctionType,
    class: ClassType,
    lox: &'a mut Lox<W>,
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    Method,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
}

impl<'a, W: Write> Resolver<'a, W> {
    pub fn new(lox: &'a mut Lox<W>) -> Self {
        Self {
            scopes: vec![],
            locals: HashMap::default(),
            function: FunctionType::None,
            class: ClassType::None,
            lox,
        }
    }

    pub fn resolve(mut self, statements: &[Stmt]) -> Self {
        for statement in statements {
            self.resolve_stmt(statement);
        }
        self
    }

    fn resolve_stmts(&mut self, statements: &[Stmt]) {
        for statement in statements {
            self.resolve_stmt(statement);
        }
    }

    fn resolve_stmt(&mut self, statement: &Stmt) {
        match statement {
            Block(statements) => {
                self.begin_scope();
                {
                    self.resolve_stmts(statements);
                }
                self.end_scope();
            }
            Class(name, methods) => {
                let enclosing_class = self.class;
                self.class = ClassType::Class;
                {
                    self.declare(name);
                    self.define(name);

                    self.begin_scope();
                    {
                        self.define_s("this");
                        for method in methods {
                            if let Function(_name, params, body) = method {
                                self.resolve_function(params, body, FunctionType::Method)
                            }
                        }
                    }
                    self.end_scope();
                }
                self.class = enclosing_class;
            }
            Expression(expression) => {
                self.resolve_expr(expression);
            }
            Function(name, params, body) => {
                self.declare(name);
                self.define(name);
                self.resolve_function(params, body, FunctionType::Function)
            }
            If(condition, then, r#else) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then);
                if let Some(e) = r#else {
                    self.resolve_stmt(e);
                }
            }
            Var(name, initializer) => {
                self.declare(name);
                if let Some(i) = initializer {
                    self.resolve_expr(&i);
                }
                self.define(name);
            }
            Print(expression) => {
                self.resolve_expr(expression);
            }
            Return(keyword, value) => {
                if self.function == FunctionType::None {
                    self.lox
                        .error_token(keyword, "Cannot return from top-level code");
                }
                if let Some(value) = value {
                    self.resolve_expr(value);
                }
            }
            While(condition, body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Asign(name, value) => {
                self.resolve_expr(value);
                self.resolve_local(name, expr);
            }
            Binary(left, _op, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Call(callee, _paren, args) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Get(object, _name) => {
                self.resolve_expr(object);
            }
            Grouping(expression) => {
                self.resolve_expr(expression);
            }
            Literal(_value) => {}
            Logical(left, _op, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Variable(name) => {
                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(false) = scope.get(&name.lexeme) {
                        self.lox.error_token(
                            name,
                            "Cannot read local variable in its own initializer.",
                        );
                    }
                }
                self.resolve_local(name, expr);
            }
            Set(object, _name, value) => {
                self.resolve_expr(value);
                self.resolve_expr(object);
            }
            This(keyword) => {
                if self.class == ClassType::None {
                    self.lox
                        .error_token(keyword, "Cannot use 'this' outside of a class");
                }
                self.resolve_local(keyword, expr);
            }
            Unary(_op, right) => {
                self.resolve_expr(right);
            }
        }
    }

    fn resolve_function(&mut self, params: &[Token], body: &[Stmt], function: FunctionType) {
        let enclosing_functoin = self.function;
        self.function = function;
        {
            self.begin_scope();
            {
                for param in params {
                    self.declare(param);
                    self.define(param);
                }
                self.resolve_stmts(body);
            }
            self.end_scope();
        }
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

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                self.lox.error_token(
                    name,
                    "Variable with this name already declared in this scope.",
                );
                return;
            }
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        self.define_s(&name.lexeme);
    }

    fn define_s(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), true);
        }
    }
}

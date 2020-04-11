use crate::interpreter::err;
use crate::interpreter::RuntimeError;
use crate::token::Literal;
use crate::token::Token;
use std::boxed::Box;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(enclosing: Environment) -> Self {
        let mut s = Self::default();
        s.enclosing = Some(Box::new(enclosing));
        s
    }

    pub fn define(&mut self, token: &Token, value: Literal) {
        let var = &token.lexeme;
        self.values.insert(var.clone(), value);
    }

    pub fn assign(&mut self, token: &Token, value: Literal) -> Result<Literal, RuntimeError> {
        let var = &token.lexeme;
        if self.values.contains_key(var) {
            self.values.insert(var.clone(), value.clone());
            Ok(value)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.as_mut().assign(token, value)
        } else {
            err(&token, &format!("Undefined variable '{}'.", var))
        }
    }

    pub fn get(&self, token: &Token) -> Result<Literal, RuntimeError> {
        let var = &token.lexeme;
        if let Some(v) = self.values.get(var) {
            Ok(v.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(token)
        } else {
            err(&token, &format!("Undefined variable '{}'.", var))
        }
    }
}

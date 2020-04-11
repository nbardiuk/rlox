use crate::interpreter::err;
use crate::interpreter::RuntimeError;
use crate::token::Literal;
use crate::token::Token;
use std::collections::HashMap;

pub struct Environment {
    scopes: Vec<HashMap<String, Literal>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
        }
    }

    pub fn nest(&mut self) {
        self.scopes.push(HashMap::default());
    }

    pub fn unnest(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, token: &Token, value: Literal) {
        let var = &token.lexeme;
        if let Some(values) = self.scopes.last_mut() {
            values.insert(var.clone(), value);
        }
    }

    pub fn assign(&mut self, token: &Token, value: Literal) -> Result<Literal, RuntimeError> {
        let var = &token.lexeme;
        for values in self.scopes.iter_mut().rev() {
            if values.contains_key(var) {
                values.insert(var.clone(), value.clone());
                return Ok(value);
            }
        }
        err(&token, &format!("Undefined variable '{}'.", var))
    }

    pub fn get(&self, token: &Token) -> Result<Literal, RuntimeError> {
        let var = &token.lexeme;
        for values in self.scopes.iter().rev() {
            if let Some(value) = values.get(var) {
                return Ok(value.clone());
            }
        }
        err(&token, &format!("Undefined variable '{}'.", var))
    }
}

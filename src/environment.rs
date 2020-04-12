use crate::interpreter::err;
use crate::interpreter::RuntimeError;
use crate::interpreter::Value;
use crate::token::Token;
use std::collections::HashMap;

pub struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
        }
    }

    pub fn from_global(&self) -> Self {
        Self {
            scopes: vec![self.scopes[0].clone(), HashMap::default()],
        }
    }

    pub fn nest(&mut self) {
        self.scopes.push(HashMap::default());
    }

    pub fn unnest(&mut self) {
        self.scopes.pop();
    }

    pub fn define_global(&mut self, var: &str, value: Value) {
        if let Some(values) = self.scopes.first_mut() {
            values.insert(var.to_string(), value);
        }
    }

    pub fn define(&mut self, var: &str, value: Value) {
        if let Some(values) = self.scopes.last_mut() {
            values.insert(var.to_string(), value);
        }
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> Result<Value, RuntimeError> {
        let var = &token.lexeme;
        for values in self.scopes.iter_mut().rev() {
            if values.contains_key(var) {
                values.insert(var.clone(), value.clone());
                return Ok(value);
            }
        }
        err(&token, &format!("Undefined variable '{}'.", var))
    }

    pub fn get(&self, token: &Token) -> Result<Value, RuntimeError> {
        let var = &token.lexeme;
        for values in self.scopes.iter().rev() {
            if let Some(value) = values.get(var) {
                return Ok(value.clone());
            }
        }
        err(&token, &format!("Undefined variable '{}'.", var))
    }
}

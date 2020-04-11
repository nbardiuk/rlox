use crate::interpreter::RuntimeError;
use crate::token::Literal;
use crate::token::Token;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn define(&mut self, token: &Token, value: Literal) {
        let var = &token.lexeme;
        self.values.insert(var.clone(), value);
    }

    pub fn get(&self, token: &Token) -> Result<Literal, RuntimeError> {
        let var = &token.lexeme;
        let value = self.values.get(var).cloned();
        value.ok_or_else(|| RuntimeError::new(&token, &format!("Undefined variable '{}'.", var)))
    }
}

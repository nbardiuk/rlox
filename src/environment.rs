use crate::interpreter::err;
use crate::interpreter::Result;
use crate::interpreter::Value;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<Env>;

pub struct Env {
    enclosing: Option<EnvRef>,
    values: RefCell<HashMap<String, Value>>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(Self {
            enclosing: None,
            values: RefCell::new(HashMap::default()),
        })
    }

    pub fn nested(enclosing: EnvRef) -> EnvRef {
        Rc::new(Self {
            enclosing: Some(enclosing),
            values: RefCell::new(HashMap::default()),
        })
    }

    pub fn unnested(e: EnvRef) -> EnvRef {
        if let Some(e) = e.enclosing.clone() {
            e
        } else {
            e
        }
    }

    pub fn define(&self, var: &str, value: Value) {
        self.values.borrow_mut().insert(var.to_string(), value);
    }

    pub fn assign(&self, token: &Token, value: Value) -> Result<Value> {
        let var = &token.lexeme;
        if self.values.borrow().contains_key(var) {
            self.values.borrow_mut().insert(var.clone(), value.clone());
            return Ok(value);
        }
        match &self.enclosing {
            Some(p) => p.assign(token, value),
            _ => err(&token, &format!("Undefined variable '{}'.", var)),
        }
    }

    fn ancestor(env: EnvRef, distance: usize) -> EnvRef {
        if distance == 0 {
            env
        } else {
            Self::ancestor(env.enclosing.clone().unwrap(), distance - 1)
        }
    }

    pub fn get_at(env: EnvRef, distance: usize, token: &Token) -> Result<Value> {
        let var = &token.lexeme;
        match Self::ancestor(env, distance).values.borrow().get(var) {
            Some(value) => Ok(value.clone()),
            None => err(&token, &format!("Undefined variable '{}'.", var)),
        }
    }

    pub fn assign_at(env: EnvRef, distance: usize, token: &Token, value: Value) -> Result<Value> {
        let var = &token.lexeme;
        Self::ancestor(env, distance)
            .values
            .borrow_mut()
            .insert(var.clone(), value.clone());
        Ok(value)
    }

    pub fn get(&self, token: &Token) -> Result<Value> {
        let var = &token.lexeme;
        match self.values.borrow().get(var) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(p) => p.get(token),
                _ => err(&token, &format!("Undefined variable '{}'.", var)),
            },
        }
    }
}

use crate::interpreter::err;
use crate::interpreter::Result;
use crate::interpreter::Value;
use crate::token::Token;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type EnvRef = Rc<Env>;

pub struct Env {
    enclosing: Option<EnvRef>,
    values: RefCell<FxHashMap<String, Value>>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(Self {
            enclosing: None,
            values: RefCell::new(FxHashMap::default()),
        })
    }

    pub fn nested(enclosing: EnvRef) -> EnvRef {
        Rc::new(Self {
            enclosing: Some(enclosing),
            values: RefCell::new(FxHashMap::default()),
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

    pub fn get_at(&self, distance: usize, token: &Token) -> Result<Value> {
        let var = &token.lexeme;
        if distance == 0 {
            match self.values.borrow().get(var) {
                Some(value) => Ok(value.clone()),
                None => err(&token, &format!("Undefined variable '{}'.", var)),
            }
        } else {
            self.enclosing.clone().unwrap().get_at(distance - 1, token)
        }
    }

    pub fn assign_at(&self, distance: usize, token: &Token, value: Value) -> Result<Value> {
        let var = &token.lexeme;
        if distance == 0 {
            self.values.borrow_mut().insert(var.clone(), value.clone());
            Ok(value)
        } else {
            self.enclosing
                .clone()
                .unwrap()
                .assign_at(distance - 1, token, value)
        }
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

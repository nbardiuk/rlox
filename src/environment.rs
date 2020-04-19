use crate::interpreter::err;
use crate::interpreter::Result;
use crate::interpreter::Value;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Environment>>;

pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            enclosing: None,
            values: HashMap::default(),
        }))
    }

    pub fn global(env: EnvRef) -> EnvRef {
        match &env.clone().borrow().enclosing {
            Some(g) => Environment::global(g.clone()),
            None => env,
        }
    }

    pub fn nested(enclosing: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            enclosing: Some(enclosing),
            values: HashMap::default(),
        }))
    }

    pub fn unnested(e: EnvRef) -> EnvRef {
        if let Some(e) = e.borrow().enclosing.clone() {
            e
        } else {
            e.clone()
        }
    }

    pub fn define(&mut self, var: &str, value: Value) {
        self.values.insert(var.to_string(), value);
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> Result<Value> {
        let var = &token.lexeme;
        if self.values.contains_key(var) {
            self.values.insert(var.clone(), value.clone());
            return Ok(value);
        }
        match &self.enclosing {
            Some(p) => p.borrow_mut().assign(token, value),
            _ => err(&token, &format!("Undefined variable '{}'.", var)),
        }
    }

    fn ancestor(env: EnvRef, distance: usize) -> EnvRef {
        if distance == 0 {
            env
        } else {
            let p = env
                .borrow()
                .enclosing
                .clone()
                .expect(&format!("Reached globals at distance {}", distance));
            Self::ancestor(p, distance - 1)
        }
    }

    pub fn get_at(env: EnvRef, distance: usize, token: &Token) -> Result<Value> {
        let var = &token.lexeme;
        match Self::ancestor(env, distance).borrow().values.get(var) {
            Some(value) => Ok(value.clone()),
            None => err(&token, &format!("Undefined variable '{}'.", var)),
        }
    }

    pub fn assign_at(env: EnvRef, distance: usize, token: &Token, value: Value) -> Result<Value> {
        let var = &token.lexeme;
        Self::ancestor(env, distance)
            .borrow_mut()
            .values
            .insert(var.clone(), value.clone());
        Ok(value)
    }

    pub fn get(&self, token: &Token) -> Result<Value> {
        let var = &token.lexeme;
        match self.values.get(var) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(p) => p.borrow_mut().get(token),
                _ => err(&token, &format!("Undefined variable '{}'.", var)),
            },
        }
    }
}

use crate::interpreter::err;
use crate::interpreter::RuntimeError;
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

    pub fn define(&mut self, var: &str, value: Value) {
        self.values.insert(var.to_string(), value);
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> Result<Value, RuntimeError> {
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

    pub fn get(&self, token: &Token) -> Result<Value, RuntimeError> {
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

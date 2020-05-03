use std::fmt::{Display, Error, Formatter};

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Str(ObjString),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Value::*;
        match self {
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Number(n) => write!(f, "{}", n),
            Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ObjString {
    chars: String,
    pub hash: u32,
}

impl ObjString {
    pub fn new(s: &str) -> Self {
        Self {
            chars: String::from(s),
            hash: hash(s),
        }
    }
}

impl Display for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.chars)
    }
}

// FNV-1a
fn hash(s: &str) -> u32 {
    let mut hash = 2_166_136_261;
    for c in s.chars() {
        hash ^= c as u32;
        hash = hash.wrapping_mul(16_777_619);
    }
    hash
}

impl std::ops::Add for ObjString {
    type Output = ObjString;
    fn add(self, r: Self) -> Self {
        ObjString::new(&(self.chars + &r.chars))
    }
}

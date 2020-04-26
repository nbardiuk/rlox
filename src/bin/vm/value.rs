use std::fmt::{Display, Error, Formatter};

#[derive(Copy, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Value::*;
        match self {
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Number(n) => write!(f, "{}", n),
        }
    }
}

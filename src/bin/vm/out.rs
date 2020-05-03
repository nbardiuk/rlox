use std::cell::RefCell;
use std::fmt::Arguments;
use std::io::{self, Write};
use std::rc::Rc;

pub struct Out {
    out: Rc<RefCell<dyn Write>>,
}

impl Out {
    pub fn std() -> Self {
        Out::new(Rc::new(RefCell::new(io::stdout())))
    }

    pub fn new(out: Rc<RefCell<dyn Write>>) -> Self {
        Self { out }
    }

    pub fn print(&mut self, args: Arguments) {
        self.out.borrow_mut().write_fmt(args).unwrap()
    }

    pub fn println(&mut self, args: Arguments) {
        self.out.borrow_mut().write_fmt(args).unwrap();
        self.out.borrow_mut().write_fmt(format_args!("\n")).unwrap();
    }
}

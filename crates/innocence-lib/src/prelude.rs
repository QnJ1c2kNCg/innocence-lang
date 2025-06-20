use std::rc::Rc;

use crate::environment::{self, Environment};

mod io;

#[derive(Clone, Debug)]
pub(crate) enum NativeFunction {
    StringVoid { function: fn(String) -> () },
}

/// Populates an environment with all prelude functions.
pub(crate) fn populate_environment(environment: &Rc<Environment>) {
    io::populate_environment(environment);
}

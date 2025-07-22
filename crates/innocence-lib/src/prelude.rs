use std::rc::Rc;

use crate::environment::Environment;

mod asserts;
mod io;

#[derive(Clone, Debug)]
pub(crate) enum NativeFunction {
    StringVoid { function: fn(String) -> () },
    BoolVoid { function: fn(bool) -> () },
}

/// Populates an environment with all prelude functions.
pub(crate) fn populate_environment(environment: &Rc<Environment>) {
    io::populate_environment(environment);
    asserts::populate_environment(environment);
}

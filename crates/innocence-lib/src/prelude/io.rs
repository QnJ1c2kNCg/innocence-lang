use std::rc::Rc;

use crate::{environment::Environment, prelude::NativeFunction, tokens::Identifier};

/// Populates an environment with I/O related prelude functions.
pub(super) fn populate_environment(environment: &Rc<Environment>) {
    environment.define_native_function(Identifier::new("print".to_owned()), native_print());
}

/// Native I/O print to STDOUT.
fn native_print() -> NativeFunction {
    let function = |arg: String| println!("{arg}");

    NativeFunction::StringVoid { function }
}

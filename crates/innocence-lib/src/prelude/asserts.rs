use std::rc::Rc;

use crate::{environment::Environment, prelude::NativeFunction, tokens::Identifier};

/// Populates an environment with asserts related prelude functions.
pub(super) fn populate_environment(environment: &Rc<Environment>) {
    environment.define_native_function(Identifier::new("assert".to_owned()), native_assert());
}

/// Native assert, takes a bool.
fn native_assert() -> NativeFunction {
    let function = |arg: bool| assert!(arg);

    NativeFunction::BoolVoid { function }
}

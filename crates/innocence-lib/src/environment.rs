use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{interpreter::Value, prelude::NativeFunction, tokens::Identifier};

#[derive(Debug)]
pub(crate) enum EnvironmentError {
    UndefinedVariable(Identifier),
}

/// The Environment stores the mapping of [`Identifier`] to [`Value`].
/// It contains everything that lives in memory when an innocence program executes.
pub(crate) struct Environment {
    /// Stores the variables of the innocence program as a mapping of [`Identifier`] to [`Value`].
    values: RefCell<HashMap<Identifier, Value>>,
    /// This is a backpointer to the parent environment, this is needed to handle
    /// blocks (potentially nested) and variable shadowing.
    /// (We are implementing a sort of parent pointer tree).
    parent: Option<Rc<Environment>>,
}

impl Environment {
    /// Returns the [`Environment`] used for the _global_ variables. This is
    /// the root of all environments (it has no parents). Children environments will
    /// be created to store local variables.
    pub(crate) fn new_global() -> Self {
        Self {
            values: RefCell::default(),
            parent: None,
        }
    }

    /// Constructor that takes a reference to the enclosing parent environment.
    pub(crate) fn new(enclosing: Rc<Environment>) -> Self {
        Self {
            values: RefCell::default(),
            parent: Some(enclosing),
        }
    }

    /// Define a new variable.
    /// NOTE: For now this shadows previous definitions.
    pub(crate) fn define(&self, id: Identifier, value: Value) {
        self.values.borrow_mut().insert(id, value);
    }

    /// Use to define a native function. Those functions usually belong to the prelude
    /// (functions provided by the interpreter).
    pub(crate) fn define_native_function(&self, id: Identifier, native_fn: NativeFunction) {
        assert!(
            self.values
                .borrow_mut()
                .insert(id, native_fn.into())
                .is_none(),
            "Native function should be defined only once."
        )
    }

    /// Assign an existing variable, a [`EnvironmentError::UndefinedVariable`] error
    /// will be returned if the [`Identifier`] has not been defined yet.
    pub(crate) fn assign(&self, id: Identifier, value: Value) -> Result<(), EnvironmentError> {
        match self.values.borrow_mut().entry(id) {
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.insert(value);
                Ok(())
            }
            // if we cannot find the identifier in the current environment we check in the parent environment
            Entry::Vacant(vacant_entry) => match &self.parent {
                Some(enclosing) => enclosing.assign(vacant_entry.into_key(), value),
                None => Err(EnvironmentError::UndefinedVariable(vacant_entry.into_key())),
            },
        }
    }

    /// Get the value assigned to an [`Identifier`], if it is defined.
    pub(crate) fn get(&self, id: &Identifier) -> Option<Value> {
        match self.values.borrow().get(id) {
            // TODO: not a fan of the clone here, should return a Ref
            Some(value) => Some(value.clone()),
            None => self.parent.as_ref().and_then(|enclosing| enclosing.get(id)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define_assign_get_happy_case() {
        let env = Environment::new_global();
        let id = Identifier::new("foo".to_owned());

        assert!(env.get(&id).is_none());
        env.define(id.clone(), Value::Number(42f64));
        assert!(matches!(env.get(&id).unwrap(), Value::Number(42f64)));

        env.assign(id.clone(), Value::Number(420f64)).unwrap();
        assert!(matches!(env.get(&id).unwrap(), Value::Number(420f64)));
    }

    #[test]
    fn outer_inner_env_lookup() {
        let outer = Rc::new(Environment::new_global());
        let id_outer = Identifier::new("outer".to_owned());
        outer.define(id_outer.clone(), Value::Number(42f64));

        let inner = Environment::new(outer.clone());
        let id_inner = Identifier::new("inner".to_owned());
        inner.define(id_inner.clone(), Value::Number(420f64));

        // the inner environment has access to both variable
        assert!(matches!(
            inner.get(&id_outer).unwrap(),
            Value::Number(42f64)
        ));
        assert!(matches!(
            inner.get(&id_inner).unwrap(),
            Value::Number(420f64)
        ));

        // the outer environment only has access to the outer variables
        assert!(matches!(
            outer.get(&id_outer).unwrap(),
            Value::Number(42f64)
        ));
        assert!(outer.get(&id_inner).is_none());
    }

    #[test]
    fn assign_undefined_variable() {
        let env = Environment::new_global();
        let id = Identifier::new("outer".to_owned());

        // assigning a variable that was not defined
        let err = env.assign(id.clone(), Value::Number(420f64)).unwrap_err();

        assert!(matches!(err, EnvironmentError::UndefinedVariable(_)));
    }
}

use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{interpreter::Value, tokens::Identifier};

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
            // TODO: not a fan of the clone here, should return a ref
            Some(value) => Some(value.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|enclosing| enclosing.get(id)),
        }
    }
}

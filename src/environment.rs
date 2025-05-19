use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{interpreter::Value, tokens::Identifier};

pub(crate) enum EnvironmentError {
    UndefinedVariable(Identifier),
}

pub(crate) struct Environment {
    values: RefCell<HashMap<Identifier, Value>>,
    /// This is a backpointer to the parent environment
    /// We are implementing a sort of parent pointer tree
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    pub(crate) fn new_global() -> Self {
        Self {
            values: RefCell::default(),
            enclosing: None,
        }
    }

    pub(crate) fn new(enclosing: Rc<Environment>) -> Self {
        Self {
            values: RefCell::default(),
            enclosing: Some(enclosing),
        }
    }

    pub(crate) fn define(&self, id: Identifier, value: Value) {
        self.values.borrow_mut().insert(id, value);
    }

    pub(crate) fn assign(&self, id: Identifier, value: Value) -> Result<(), EnvironmentError> {
        match self.values.borrow_mut().entry(id) {
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.insert(value);
                Ok(())
            }
            Entry::Vacant(vacant_entry) => match &self.enclosing {
                Some(enclosing) => enclosing.assign(vacant_entry.into_key(), value),
                None => Err(EnvironmentError::UndefinedVariable(vacant_entry.into_key())),
            },
        }
    }

    pub(crate) fn get(&self, id: &Identifier) -> Option<Value> {
        match self.values.borrow().get(id) {
            // TODO: Not a fan of the clone here, should return a ref
            Some(value) => Some(value.clone()),
            None => self
                .enclosing
                .as_ref()
                .map(|enclosing| enclosing.get(id))
                .flatten(),
        }
    }
}

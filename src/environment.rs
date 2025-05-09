use std::collections::{HashMap, hash_map::Entry};

use crate::{interpreter::PartiallyInterpretedExpression, tokens::Identifier};

pub(crate) enum EnvironmentError {
    UndefinedVariable(Identifier),
}

#[derive(Default)]
pub(crate) struct Environment {
    values: HashMap<Identifier, PartiallyInterpretedExpression>,
}

impl Environment {
    pub(crate) fn define(&mut self, id: Identifier, value: PartiallyInterpretedExpression) {
        self.values.insert(id, value);
    }

    pub(crate) fn assign(
        &mut self,
        id: Identifier,
        value: PartiallyInterpretedExpression,
    ) -> Result<(), EnvironmentError> {
        match self.values.entry(id) {
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.insert(value);
                Ok(())
            }
            Entry::Vacant(vacant_entry) => {
                Err(EnvironmentError::UndefinedVariable(vacant_entry.into_key()))
            }
        }
    }

    pub(crate) fn get(&self, id: &Identifier) -> Option<&PartiallyInterpretedExpression> {
        self.values.get(id)
    }
}

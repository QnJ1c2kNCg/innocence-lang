use std::collections::HashMap;

use crate::{interpreter::PartiallyInterpretedExpression, tokens::Identifier};

#[derive(Default)]
pub(crate) struct Environment {
    values: HashMap<Identifier, PartiallyInterpretedExpression>,
}

impl Environment {
    pub(crate) fn bind(&mut self, id: Identifier, value: PartiallyInterpretedExpression) {
        self.values.insert(id, value);
    }

    pub(crate) fn get(&self, id: &Identifier) -> Option<&PartiallyInterpretedExpression> {
        self.values.get(id)
    }
}

use crate::tokens::{Identifier, Token};

pub(crate) trait ExpressionVisitor<T> {
    fn visit_binary(&mut self, expr: &Expression) -> T;
    fn visit_unary(&mut self, expr: &Expression) -> T;
    fn visit_grouping(&mut self, expr: &Expression) -> T;
    fn visit_literal(&mut self, expr: &Expression) -> T;
    fn visit_variable(&mut self, expr: &Expression) -> T;
    fn visit_assign(&mut self, expr: &Expression) -> T;
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Binary {
        left: Box<Expression>,
        operation: Token,
        right: Box<Expression>,
    },
    Unary {
        operation: Token,
        right: Box<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Literal {
        literal: Token,
    },
    Variable {
        id: Identifier,
    },
    // TODO: If the language does support assignment (which I'm not sure I like),
    // I would prefer it be a statement rather than an expression
    Assign {
        id: Identifier,
        value: Box<Expression>,
    },
}

impl Expression {
    pub(crate) fn accept<T>(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T {
        match self {
            Expression::Binary { .. } => visitor.visit_binary(self),
            Expression::Unary { .. } => visitor.visit_unary(self),
            Expression::Grouping { .. } => visitor.visit_grouping(self),
            Expression::Literal { .. } => visitor.visit_literal(self),
            Expression::Variable { .. } => visitor.visit_variable(self),
            Expression::Assign { .. } => visitor.visit_assign(self),
        }
    }
}

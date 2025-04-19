use crate::tokens::Token;

pub(crate) trait ExpressionVisitor<T> {
    fn visit_binary(&mut self, e: &Expression) -> T;
    fn visit_unary(&mut self, e: &Expression) -> T;
    fn visit_grouping(&mut self, e: &Expression) -> T;
    fn visit_literal(&mut self, e: &Expression) -> T;
}

#[derive(Debug)]
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
}

impl Expression {
    pub(crate) fn accept<T>(&self, visitor: &mut dyn ExpressionVisitor<T>) -> T {
        match self {
            Expression::Binary { .. } => visitor.visit_binary(self),
            Expression::Unary { .. } => visitor.visit_unary(self),
            Expression::Grouping { .. } => visitor.visit_grouping(self),
            Expression::Literal { .. } => visitor.visit_literal(self),
        }
    }
}

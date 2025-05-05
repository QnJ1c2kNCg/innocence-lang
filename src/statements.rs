use crate::{expressions::Expression, tokens::Identifier};

pub(crate) trait StatementVisitor<T> {
    fn visit_expression_stmt(&mut self, stmt: &Statement) -> T;
    fn visit_print_stmt(&mut self, stmt: &Statement) -> T;
    fn visit_let_stmt(&mut self, stmt: &Statement) -> T;
}

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Print(Expression),
    Let {
        name: Identifier,
        initializer: Expression,
    },
}

impl Statement {
    pub(crate) fn accept<T>(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
        match self {
            Statement::Expression(_) => visitor.visit_expression_stmt(self),
            Statement::Print(_) => visitor.visit_print_stmt(self),
            Statement::Let { .. } => visitor.visit_let_stmt(self),
        }
    }
}

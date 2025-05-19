use std::rc::Rc;

use crate::{environment::Environment, expressions::Expression, tokens::Identifier};

pub(crate) trait StatementVisitor<T> {
    fn visit_expression_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_print_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_let_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_block_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_if_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
}

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Print(Expression),
    Let {
        name: Identifier,
        initializer: Expression,
    },
    Block(Vec<Statement>),
    If {
        condition: Expression,
        if_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
}

impl Statement {
    pub(crate) fn accept<T>(
        &self,
        visitor: &mut dyn StatementVisitor<T>,
        environment: &Rc<Environment>,
    ) -> T {
        match self {
            Statement::Expression(_) => visitor.visit_expression_stmt(self, environment),
            Statement::Print(_) => visitor.visit_print_stmt(self, environment),
            Statement::Let { .. } => visitor.visit_let_stmt(self, environment),
            Statement::Block(_) => visitor.visit_block_stmt(self, environment),
            Statement::If { .. } => visitor.visit_if_stmt(self, environment),
        }
    }
}

use std::rc::Rc;

use crate::{environment::Environment, expressions::Expression, tokens::Identifier};

/// All types of supported statements for the innocence language.
/// Usually statements do not evaluate to a value, but have side effects
/// (this rule is only loosely followed at the moment).
#[derive(Debug)]
pub(crate) enum Statement {
    /// This represent an [`Expression`] where a statement is expected.
    /// For example, a function call that ends with a `;`.
    Expression(Expression),
    /// Standard print to console
    // TODO: Move to standard library function
    Print(Expression),
    /// Binding a new variable. Variables in innocence needs to be
    /// explicitly initialized.
    Let {
        name: Identifier,
        initializer: Expression,
    },
    /// Represent a block (list) of statements, this could be a function body
    /// or a nested block (scoping).
    Block(Vec<Statement>),
    /// if/else statement, this is the conditional construct of innocence
    If {
        condition: Expression,
        if_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
}

/// Visitor pattern trait for something that can visit a statement. This is implemented by
/// the [`Interpreter`].
pub(crate) trait StatementVisitor<T> {
    fn visit_expression_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_print_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_let_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_block_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
    fn visit_if_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> T;
}

impl Statement {
    /// Machinery for the Visitor pattern that is used to traverse the AST.
    /// This function takes a reference to the visitor and calls the correct
    /// visit_* function based on the type of statement.
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

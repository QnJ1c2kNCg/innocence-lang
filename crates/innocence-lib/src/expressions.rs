use std::rc::Rc;

use crate::{
    environment::Environment,
    tokens::{Identifier, Token},
};

/// All types of supported expression for the innocence language.
/// An expression can be recursively evaluated by the [`Interpreter`]
/// to produce a [`Value`].
#[derive(Debug, Clone)]
pub(crate) enum Expression {
    /// Binary expression of the form: <left expression> <operand> <right expression>.
    /// For example: 1 + 2.
    Binary {
        left: Box<Expression>,
        operation: Token,
        right: Box<Expression>,
    },
    /// Logical `and` and `or`
    Logical {
        left: Box<Expression>,
        operation: Token,
        right: Box<Expression>,
    },
    /// Unary expression of the form: <operand> <right expression>.
    /// For example: -1, to negate a number, or !false, to flip a bool.
    Unary {
        operation: Token,
        right: Box<Expression>,
    },
    /// Grouping to affect precedence, the `Grouping` contains the expression that
    /// is contained between the parenthesis pair.
    Grouping { expr: Box<Expression> },
    /// Literal are hardcoded strings, numbers or bools in the source code.
    /// For example: "hello, world" is a string literal, 42 is a number literal.
    Literal { literal: Token },
    /// A variable, identified using an [`Identifier`]. Variables are stored in [`Environment`].
    Variable { id: Identifier },
    /// Assignment of already declared variable.
    // TODO: If the language does support assignment (which I'm not sure I like),
    // I would prefer it be a statement rather than an expression
    Assign {
        id: Identifier,
        value: Box<Expression>,
    },
    /// Initialization of a struct, during a variable declaration
    StructInit {
        struct_type: Identifier,
        fields: Vec<(Identifier, Expression)>,
    },
    /// Accessor for a struct field
    StructAccessor {
        instance_name: Box<Expression>,
        field_name: Identifier,
    },
    /// Setter for a struct field
    StructSetter {
        instance_name: Box<Expression>,
        field_name: Identifier,
        value: Box<Expression>,
    },
    /// A function call, the `callee` will be the [`Identifier`] for the function,
    /// and arguments are what is inside the parenthesis.
    FunctionCall {
        callee: Box<Expression>,
        // Store the closing paren for better error reporting.
        paren: Token,
        arguments: Vec<Expression>,
    },
}

/// Visitor pattern trait for something that can visit an expression. This is implemented by
/// the [`Interpreter`].
pub(crate) trait ExpressionVisitor<T> {
    fn visit_binary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_logical(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_unary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_grouping(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_literal(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_variable(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_assign(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_function_call(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_struct_initialization(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> T;
    fn visit_struct_accessor(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
    fn visit_struct_setter(&mut self, expr: &Expression, environment: &Rc<Environment>) -> T;
}

impl Expression {
    /// Machinery for the Visitor pattern that is used to traverse the AST.
    /// This function takes a reference to the visitor and calls the correct
    /// visit_* function based on the type of expression.
    pub(crate) fn accept<T>(
        &self,
        visitor: &mut dyn ExpressionVisitor<T>,
        environment: &Rc<Environment>,
    ) -> T {
        match self {
            Expression::Binary { .. } => visitor.visit_binary(self, environment),
            Expression::Logical { .. } => visitor.visit_logical(self, environment),
            Expression::Unary { .. } => visitor.visit_unary(self, environment),
            Expression::Grouping { .. } => visitor.visit_grouping(self, environment),
            Expression::Literal { .. } => visitor.visit_literal(self, environment),
            Expression::Variable { .. } => visitor.visit_variable(self, environment),
            Expression::Assign { .. } => visitor.visit_assign(self, environment),
            Expression::FunctionCall { .. } => visitor.visit_function_call(self, environment),
            Expression::StructInit { .. } => visitor.visit_struct_initialization(self, environment),
            Expression::StructAccessor { .. } => visitor.visit_struct_accessor(self, environment),
            Expression::StructSetter { .. } => visitor.visit_struct_setter(self, environment),
        }
    }
}

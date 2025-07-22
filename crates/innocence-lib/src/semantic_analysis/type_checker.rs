/// The [`TypeChecker`] is part of the semantic analysis (between the parser and the interpreting).
/// It traverses the AST, adding type information, and then validating type constraints.

// TODO: Maybe not true VVV
/// For now, I'm thinking of implementing this in two passes. The first pass builds a symbol table that
/// maps [`Identifier`] to [`TypeInfo`]. The second pass does the type constraint validation.
use std::{collections::HashMap, rc::Rc};

use crate::{
    environment::Environment,
    expressions::{Expression, ExpressionVisitor},
    statements::{Statement, StatementVisitor},
    tokens::{Identifier, TokenType},
};

type Result<T> = std::result::Result<T, TypeCheckerError>;

pub(crate) enum TypeCheckerError {
    TypeMismatch(TypeInfo, TypeInfo),
    ExpectedTypeButGot(TypeInfo, TypeInfo),
}

impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Print source location
        // TODO: Add more context to the user
        match self {
            TypeCheckerError::TypeMismatch(lhs, rhs) => {
                write!(f, "Type mismatch between: {} and {}", lhs, rhs)
            }

            TypeCheckerError::ExpectedTypeButGot(lhs, rhs) => {
                write!(f, "Expected type {} but got {}", lhs, rhs)
            }
        }
    }
}

/// Represents the different types in innocence
/// TODO: Should we have a type for function?
/// TODO: Cleanup the project, I feel like there is type info in a few different places
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum TypeInfo {
    Number,
    String,
    Bool,
    UserDefined(Identifier),
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Number => write!(f, "Number"),
            TypeInfo::String => write!(f, "String"),
            TypeInfo::Bool => write!(f, "Bool"),
            TypeInfo::UserDefined(identifier) => write!(f, "{}", identifier.0),
        }
    }
}

impl From<Identifier> for TypeInfo {
    fn from(value: Identifier) -> Self {
        match value.0.as_str() {
            "Number" => TypeInfo::Number,
            "Bool" => TypeInfo::Bool,
            "String" => TypeInfo::String,
            _ => TypeInfo::UserDefined(value),
        }
    }
}

pub(crate) struct TypeChecker {
    symbol_table: HashMap<Identifier, TypeInfo>,
}

impl TypeChecker {
    pub(crate) fn new() -> Self {
        TypeChecker {
            symbol_table: HashMap::default(),
        }
    }

    pub(crate) fn check(&mut self, statements: &Vec<Statement>) -> Result<()> {
        for statement in statements {
            // TODO: The environment is not used for the type checker... should probably refactor the signatures
            self.execute(statement, &Rc::new(Environment::new_global()))?
        }
        Ok(())
    }

    // TODO: Rename
    fn execute(&mut self, statement: &Statement, environment: &Rc<Environment>) -> Result<()> {
        statement.accept(self, environment)
    }

    /// Evaluate an [`Expression`] to produce a [`TypeInfo`]. Note that this can/will recurse, for instance
    /// calling `evaluate` on a [`Expression::Grouping`] expression, will return in a subsequent call to
    /// `evaluate` to what is _contained_ in the grouping, etc.
    fn evaluate(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<TypeInfo> {
        expr.accept(self, environment)
    }
}

impl ExpressionVisitor<Result<TypeInfo>> for TypeChecker {
    fn visit_binary(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Binary {
                left,
                operation,
                right,
            } => {
                let left = self.evaluate(left, environment)?;
                let right = self.evaluate(right, environment)?;

                // We expect both operands to be of the same type
                if left != right {
                    return Err(TypeCheckerError::TypeMismatch(left, right));
                }

                match operation.token_type {
                    TokenType::Minus | TokenType::Slash | TokenType::Star => {
                        if left != TypeInfo::Number {
                            Err(TypeCheckerError::ExpectedTypeButGot(TypeInfo::Number, left))
                        } else {
                            Ok(left)
                        }
                    }
                    TokenType::Plus => {
                        if left == TypeInfo::Number || left == TypeInfo::String {
                            Ok(left)
                        } else {
                            // This is not the most accurate, since String would also be valid
                            Err(TypeCheckerError::ExpectedTypeButGot(TypeInfo::Number, left))
                        }
                    }
                    // These operation always result in a Bool, even if the operands are not initially bools
                    TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::BangEqual
                    | TokenType::EqualEqual => Ok(TypeInfo::Bool),

                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_logical(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Logical {
                left,
                operation,
                right,
            } => {
                let left = self.evaluate(left, environment)?;
                let right = self.evaluate(right, environment)?;
                match (&left, &right) {
                    (TypeInfo::Bool, TypeInfo::Bool) => Ok(left),
                    _ => Err(TypeCheckerError::TypeMismatch(left, right)),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_unary(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Unary { operation, right } => {
                let right = self.evaluate(right, environment)?;
                match operation.token_type {
                    TokenType::Minus => {
                        if right == TypeInfo::Number {
                            Ok(right)
                        } else {
                            Err(TypeCheckerError::ExpectedTypeButGot(
                                TypeInfo::Number,
                                right,
                            ))
                        }
                    }
                    TokenType::Bang => {
                        if right == TypeInfo::Bool {
                            Ok(right)
                        } else {
                            Err(TypeCheckerError::ExpectedTypeButGot(TypeInfo::Bool, right))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_grouping(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Grouping { expr } => self.evaluate(expr, environment),
            _ => unreachable!(),
        }
    }

    fn visit_literal(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Literal { literal } => match &literal.token_type {
                TokenType::Number(_) => Ok(TypeInfo::Number),
                TokenType::String(_) => Ok(TypeInfo::String),
                TokenType::True | TokenType::False => Ok(TypeInfo::Bool),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn visit_variable(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        match expr {
            Expression::Variable { id } => self
                .symbol_table
                .get(id)
                .cloned()
                // .ok_or(InterpreterError::UnknownVariable(id.clone())),
                .ok_or_else(|| panic!()),
            _ => unreachable!(),
        }
    }

    fn visit_assign(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        todo!()
    }

    fn visit_function_call(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        // TODO
        Ok(TypeInfo::Bool)
    }

    fn visit_struct_initialization(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        todo!()
    }

    fn visit_struct_accessor(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        todo!()
    }

    fn visit_struct_setter(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<TypeInfo> {
        todo!()
    }
}

impl StatementVisitor<Result<()>> for TypeChecker {
    fn visit_expression_stmt(
        &mut self,
        stmt: &Statement,
        environment: &Rc<Environment>,
    ) -> Result<()> {
        match stmt {
            Statement::Expression(expression) => self.evaluate(expression, environment).map(|_| ()),
            _ => unreachable!(),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::Let {
                name,
                type_info,
                initializer,
            } => {
                let evaluated_type_info = self.evaluate(initializer, environment)?;
                if let Some(type_info) = type_info {
                    if evaluated_type_info != *type_info {
                        return Err(TypeCheckerError::TypeMismatch(
                            evaluated_type_info,
                            type_info.clone(),
                        ));
                    }
                }
                if self
                    .symbol_table
                    .insert(name.clone(), evaluated_type_info)
                    .is_some()
                {
                    // TODO: Handle this gracefully
                    panic!("can this happen?")
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        todo!()
    }

    fn visit_function_stmt(
        &mut self,
        stmt: &Statement,
        environment: &Rc<Environment>,
    ) -> Result<()> {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        todo!()
    }

    fn visit_if_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        todo!()
    }

    fn visit_while_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        todo!()
    }

    fn visit_return_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        todo!()
    }
}

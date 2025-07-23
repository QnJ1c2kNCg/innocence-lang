/// The [`TypeChecker`] is part of the semantic analysis (between the parser and the interpreting).
/// It traverses the AST, adding type information, and then validating type constraints.

// TODO: Maybe not true VVV
/// For now, I'm thinking of implementing this in two passes. The first pass builds a symbol table that
/// maps [`Identifier`] to [`TypeInfo`]. The second pass does the type constraint validation.
use std::{collections::HashMap, hash::Hash, rc::Rc};

use crate::{
    environment::Environment,
    expressions::{Expression, ExpressionVisitor},
    statements::{Parameter, Statement, StatementVisitor},
    tokens::{Identifier, TokenType},
};

type Result<T> = std::result::Result<T, TypeCheckerError>;

pub(crate) enum TypeCheckerError {
    TypeMismatch(TypeInfo, TypeInfo),
    ExpectedTypeButGot(TypeInfo, TypeInfo),
    InvalidNumberOfParameters {
        struct_type: Identifier,
        expected_num_params: usize,
        actual_num_params: usize,
    },
    StructInitMissingParameter {
        struct_name: Identifier,
        missing_param: Identifier,
    },
    StructInitTypeMismatch {
        struct_name: Identifier,
        field_name: Identifier,
        expected_type_info: TypeInfo,
        actual_type_info: TypeInfo,
    },
    StructAccessUnknownField {
        struct_name: Identifier,
        field_name: Identifier,
    },
}

impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Print source location
        // TODO: Add more context to the user
        match self {
            TypeCheckerError::TypeMismatch(lhs, rhs) => {
                write!(f, "Type mismatch between: {} and {}.", lhs, rhs)
            }
            TypeCheckerError::ExpectedTypeButGot(lhs, rhs) => {
                write!(f, "Expected type {} but got {}.", lhs, rhs)
            }
            TypeCheckerError::InvalidNumberOfParameters {
                struct_type,
                expected_num_params,
                actual_num_params,
            } => write!(
                f,
                "Invalid number of parameters for struct `{}`, expect {} but got {}.",
                struct_type, expected_num_params, actual_num_params
            ),
            TypeCheckerError::StructInitMissingParameter {
                struct_name: struct_type,
                missing_param,
            } => write!(
                f,
                "Missing parameter `{}` during the initialization of struct `{}`.",
                missing_param, struct_type
            ),
            TypeCheckerError::StructInitTypeMismatch {
                struct_name,
                field_name,
                expected_type_info,
                actual_type_info,
            } => write!(
                f,
                "Type mismatch during the initialization of field `{}` of struct `{}`, expect {} but got {}.",
                field_name, struct_name, expected_type_info, actual_type_info
            ),
            TypeCheckerError::StructAccessUnknownField {
                struct_name,
                field_name,
            } => write!(
                f,
                "Tried to access field `{}` which is unknown to struct `{}`.",
                field_name, struct_name
            ),
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

#[derive(Clone)]
struct StructSignature {
    struct_name: Identifier,
    parameters: HashMap<Identifier, TypeInfo>,
}

struct FunctionSignature {
    struct_name: Identifier,
    parameters: Vec<Parameter>,
    return_type_info: TypeInfo,
}

pub(crate) struct TypeChecker {
    /// Stores the mapping of variable identifier to their type.
    variables_symbol_table: HashMap<Identifier, TypeInfo>,
    /// Stores the mapping of struct identifier to their type signature.
    structs_symbol_table: HashMap<Identifier, StructSignature>,
    /// Stores the mapping of function identifier to their type signature.
    functions_symbol_table: HashMap<Identifier, FunctionSignature>,
}

impl TypeChecker {
    pub(crate) fn new() -> Self {
        TypeChecker {
            variables_symbol_table: HashMap::default(),
            structs_symbol_table: HashMap::default(),
            functions_symbol_table: HashMap::default(),
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
                .variables_symbol_table
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
        match expr {
            Expression::StructInit {
                struct_type,
                fields,
            } => {
                let mut evaluated_fields = HashMap::with_capacity(fields.len());
                for (id, expr) in fields {
                    let evaluated_field = self.evaluate(expr, environment)?;
                    evaluated_fields.insert(id.clone(), evaluated_field);
                }

                // TODO: Fix unwrap
                let expected_signature = self.structs_symbol_table.get(struct_type).unwrap();

                if expected_signature.parameters.len() != evaluated_fields.len() {
                    return Err(TypeCheckerError::InvalidNumberOfParameters {
                        struct_type: struct_type.clone(),
                        expected_num_params: expected_signature.parameters.len(),
                        actual_num_params: evaluated_fields.len(),
                    });
                }

                for (id, expected_type_info) in &expected_signature.parameters {
                    if let Some(type_info) = evaluated_fields.get(id) {
                        if expected_type_info != type_info {
                            return Err(TypeCheckerError::StructInitTypeMismatch {
                                struct_name: struct_type.clone(),
                                field_name: id.clone(),
                                expected_type_info: expected_type_info.clone(),
                                actual_type_info: type_info.clone(),
                            });
                        }
                    } else {
                        return Err(TypeCheckerError::StructInitMissingParameter {
                            struct_name: struct_type.clone(),
                            missing_param: id.clone(),
                        });
                    }
                }

                Ok(TypeInfo::UserDefined(struct_type.clone()))
            }
            _ => unreachable!(),
        }
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
        match expr {
            Expression::StructSetter {
                instance_name,
                field_name,
                value,
            } => {
                // TODO: Test with nested structs
                let evaluated_struct_type = self.evaluate(instance_name, environment)?;

                if let TypeInfo::UserDefined(struct_type) = &evaluated_struct_type {
                    let expected_type_signature = self
                        .structs_symbol_table
                        .get(&struct_type)
                        .cloned()
                        .unwrap();

                    let evaluated_type_info = self.evaluate(value, environment)?;
                    if let Some(expected_type_info) =
                        expected_type_signature.parameters.get(field_name)
                    {
                        if expected_type_info != &evaluated_type_info {
                            return Err(TypeCheckerError::StructInitTypeMismatch {
                                struct_name: struct_type.clone(),
                                field_name: field_name.clone(),
                                expected_type_info: expected_type_info.clone(),
                                actual_type_info: evaluated_type_info.clone(),
                            });
                        }
                    } else {
                        return Err(TypeCheckerError::StructAccessUnknownField {
                            struct_name: struct_type.clone(),
                            field_name: field_name.clone(),
                        });
                    }
                    Ok(evaluated_type_info)
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
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
                    .variables_symbol_table
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
        match stmt.clone() {
            Statement::Struct { name, fields } => {
                let parameters = fields
                    .into_iter()
                    .map(|parameter| (parameter.id, parameter.type_info))
                    .collect();
                let signature = StructSignature {
                    struct_name: name.clone(),
                    parameters,
                };
                self.structs_symbol_table.insert(name, signature);
                Ok(())
            }
            _ => unreachable!(),
        }
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

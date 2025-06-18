/// The [`Interpreter`] is the third phase of the innocence interpreter (after the
/// [`Scanner`] and [`Parser`]). The [`Interpreter`] takes the list of [`Statement`]s
/// produced by the [`Parser`] in executes/evaluates them one by one. The [`Interpreter`]
/// implements both [`StatementVisitor`] and [`ExpressionVisitor`], this is where the logic
/// for handling the different types of nodes of the AST are.
use std::{
    collections::HashMap,
    fmt::Display,
    iter::zip,
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    environment::Environment,
    expressions::{Expression, ExpressionVisitor},
    statements::{Statement, StatementVisitor},
    tokens::{Identifier, TokenType},
};

type Result<T> = std::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub(crate) enum InterpreterError {
    InvalidType(String),
    UnknownVariable(Identifier),
    /// This is used when the `return` keyword is encountered.
    /// It's used pass the return value up to the caller.
    /// The option is used since we can return void.
    Return(Value),
}

/// Construct that interprets innocence code.
pub(crate) struct Interpreter {
    /// Pointer to the global [`Environment`].
    root_environment: Rc<Environment>,
}

/// Represents a value, this is what an [`Expression`] evaluates to.
// TODO: Break this type up
#[derive(Clone, Debug)]
pub(crate) enum Value {
    /// Unit type that represents nothing
    Null,
    String(String),
    // TODO: Swap out to customer number type
    Number(f64),
    Bool(bool),
    // XXX: Not sure if we want this here. Maybe functions deserve
    // their own construct and storage in the [`Environment`].
    // Also, this is a copy of the Statement::Function...
    Function {
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
    StructType {
        name: Identifier,
        fields: Vec<Identifier>,
    },
    StructInstance {
        struct_type: Identifier,
        fields: Arc<Mutex<HashMap<Identifier, Value>>>,
    },
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function {
                name,
                parameters: _,
                body: _,
            } => write!(f, "<func> {:?}", name),
            Value::StructType { name, fields: _ } => write!(f, "<struct_type> {:?}", name),
            Value::StructInstance {
                struct_type,
                fields,
            } => write!(
                f,
                "<struct_instance> of type {:?}, fields: {:?}",
                struct_type, fields
            ),
        }
    }
}

impl Value {
    fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    fn unwrap_number(&self) -> Result<f64> {
        match self {
            Value::Number(number) => Ok(*number),
            _ => Err(InterpreterError::InvalidType(format!(
                "Expected a Number but got {:?}",
                self
            ))),
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    fn unwrap_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(bool) => Ok(*bool),
            _ => Err(InterpreterError::InvalidType(format!(
                "Expected a Bool but got {:?}",
                self
            ))),
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    fn unwrap_string(self) -> Result<String> {
        match self {
            Value::String(string) => Ok(string),
            _ => Err(InterpreterError::InvalidType(format!(
                "Expected a String but got {:?}",
                self
            ))),
        }
    }

    fn is_equal(left: &Self, right: &Self) -> Result<bool> {
        if left.is_number() {
            let left = left.unwrap_number()?;
            let right = right.unwrap_number()?;
            Ok(left == right)
        } else if left.is_string() {
            let left = left.clone().unwrap_string()?;
            let right = right.clone().unwrap_string()?;
            Ok(left == right)
        } else if left.is_bool() {
            let left = left.clone().unwrap_bool()?;
            let right = right.clone().unwrap_bool()?;
            Ok(left == right)
        } else {
            unreachable!()
        }
    }
}

impl Interpreter {
    pub(crate) fn new() -> Self {
        Self {
            root_environment: Rc::new(Environment::new_global()),
        }
    }

    /// Entry point of the interpreter, this takes a list of statements (the source code)
    /// and interprets it. This is done by executing the statements and evaluating the expressions
    /// contained in those statements.
    pub(crate) fn interpret(&mut self, statements: &Vec<Statement>) -> Result<()> {
        for statement in statements {
            let res = self.execute(statement, &self.root_environment.clone());
            if res.is_err() {
                todo!(
                    "call report error and make sure to show the line number: {:?}",
                    res.unwrap_err()
                )
            }
        }
        Ok(())
    }

    /// Executes a single statement, the [`Interpreter`] implements [`StatementVisitor`], so this
    /// function is basically just visiting the statement. An [`Environment`] is passed in since
    /// the statement might require reading/writing variables.
    fn execute(&mut self, statement: &Statement, environment: &Rc<Environment>) -> Result<()> {
        statement.accept(self, environment)
    }

    /// Executes a list (block) of statements within one [`Environment`].
    fn execute_block(
        &mut self,
        statements: &Vec<Statement>,
        environment: Rc<Environment>,
    ) -> Result<()> {
        for statement in statements {
            self.execute(statement, &environment)?;
        }

        Ok(())
    }

    /// Evaluate an [`Expression`] to produce a [`Value`]. Note that this can/will recurse, for instance
    /// calling `evaluate` on a [`Expression::Grouping`] expression, will return in a subsequent call to
    /// `evaluate` to what is _contained_ in the grouping, etc.
    fn evaluate(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<Value> {
        expr.accept(self, environment)
    }

    /// Makes a function call. This function will first create an [`Environment`] for
    /// the body of the function, populate it with all of the arguments that were passed
    /// to the function and then evaluate its body.
    fn make_function_call(
        &mut self,
        callee: Value,
        arguments: Vec<Value>,
        parent_environment: &Rc<Environment>,
    ) -> Result<Value> {
        let function_env = Rc::new(Environment::new(parent_environment.clone()));

        match callee {
            Value::Function {
                name: _,
                parameters,
                body,
            } => {
                assert_eq!(arguments.len(), parameters.len());
                for (parameter, argument) in zip(parameters, arguments) {
                    function_env.define(parameter, argument);
                }
                match &*body {
                    Statement::Block(statements) => {
                        match self.execute_block(statements, function_env) {
                            Err(InterpreterError::Return(value)) => Ok(value),
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
}

impl ExpressionVisitor<Result<Value>> for Interpreter {
    fn visit_binary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<Value> {
        match expr {
            Expression::Binary {
                left,
                operation,
                right,
            } => {
                let left = self.evaluate(left, environment)?;
                let right = self.evaluate(right, environment)?;
                match operation.token_type {
                    TokenType::Minus => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Number(left - right))
                    }
                    TokenType::Slash => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Number(left / right))
                    }
                    TokenType::Star => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Number(left * right))
                    }
                    TokenType::Plus => {
                        if left.is_number() {
                            let left = left.unwrap_number()?;
                            let right = right.unwrap_number()?;
                            Ok(Value::Number(left + right))
                        } else if left.is_string() {
                            let mut left = left.unwrap_string()?;
                            let right = right.unwrap_string()?;
                            left.push_str(right.as_str());
                            Ok(Value::String(left))
                        } else {
                            unreachable!()
                        }
                    }
                    TokenType::Greater => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Bool(left > right))
                    }
                    TokenType::GreaterEqual => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Bool(left >= right))
                    }
                    TokenType::Less => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Bool(left < right))
                    }
                    TokenType::LessEqual => {
                        let left = left.unwrap_number()?;
                        let right = right.unwrap_number()?;
                        Ok(Value::Bool(left <= right))
                    }
                    TokenType::BangEqual => {
                        let outcome = !Value::is_equal(&left, &right)?;
                        Ok(Value::Bool(outcome))
                    }
                    TokenType::EqualEqual => {
                        let outcome = Value::is_equal(&left, &right)?;
                        Ok(Value::Bool(outcome))
                    }

                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_logical(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<Value> {
        match expr {
            Expression::Logical {
                left,
                operation,
                right,
            } => {
                let left = self.evaluate(left, environment)?;
                match operation.token_type {
                    TokenType::Or => {
                        if left.unwrap_bool()? {
                            // this is short-circuiting, if it's a logical `or` and the first
                            // term is true the whole expression will be true
                            Ok(left)
                        } else {
                            self.evaluate(right, environment)
                        }
                    }
                    TokenType::And => {
                        if !left.unwrap_bool()? {
                            // this is short-circuiting, if it's a logical `and` and the first
                            // term is false the whole expression will be false
                            Ok(left)
                        } else {
                            self.evaluate(right, environment)
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_unary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<Value> {
        match expr {
            Expression::Unary { operation, right } => {
                let right = self.evaluate(right, environment)?;
                match operation.token_type {
                    TokenType::Minus => {
                        let number = right.unwrap_number()?;
                        Ok(Value::Number(-number))
                    }
                    TokenType::Bang => {
                        let bool = right.unwrap_bool()?;
                        Ok(Value::Bool(!bool))
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
    ) -> Result<Value> {
        match expr {
            Expression::Grouping { expr } => self.evaluate(expr, environment),
            _ => unreachable!(),
        }
    }

    fn visit_literal(&mut self, expr: &Expression, _: &Rc<Environment>) -> Result<Value> {
        match expr {
            Expression::Literal { literal } => match &literal.token_type {
                TokenType::Number(number) => Ok(Value::Number(*number)),
                TokenType::String(string) => Ok(Value::String(string.clone())),
                TokenType::True => Ok(Value::Bool(true)),
                TokenType::False => Ok(Value::Bool(false)),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn visit_variable(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<Value> {
        match expr {
            // TODO: I would like unknown variable errors to be detected
            // at scan time, not at runtime.
            Expression::Variable { id } => environment
                .get(id)
                .ok_or(InterpreterError::UnknownVariable(id.clone())),
            _ => unreachable!(),
        }
    }

    fn visit_assign(&mut self, expr: &Expression, environment: &Rc<Environment>) -> Result<Value> {
        match expr {
            Expression::Assign { id, value } => {
                let value = self.evaluate(value, environment)?;
                environment
                    .assign(id.clone(), value.clone())
                    .map_err(|_err| {
                        // TODO: log
                        InterpreterError::UnknownVariable(id.clone())
                    })?;
                Ok(value)
            }
            _ => unreachable!(),
        }
    }

    fn visit_function_call(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<Value> {
        match expr {
            Expression::FunctionCall {
                callee,
                paren: _,
                arguments,
            } => {
                let callee = self.evaluate(callee, environment)?;
                let arguments = arguments
                    .iter()
                    .map(|argument| self.evaluate(argument, environment))
                    .collect::<Result<Vec<Value>>>()?;

                self.make_function_call(callee, arguments, environment)
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_initialization(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<Value> {
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

                Ok(Value::StructInstance {
                    struct_type: struct_type.clone(),
                    fields: Arc::new(Mutex::new(evaluated_fields)),
                })
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_accessor(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<Value> {
        match expr {
            Expression::StructAccessor {
                instance_name,
                field_name,
            } => {
                if let Value::StructInstance {
                    struct_type: _,
                    fields,
                } = self.evaluate(instance_name, environment)?
                {
                    let fields = fields.lock().unwrap();
                    Ok(fields.get(field_name).unwrap().clone())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_setter(
        &mut self,
        expr: &Expression,
        environment: &Rc<Environment>,
    ) -> Result<Value> {
        match expr {
            Expression::StructSetter {
                instance_name,
                field_name,
                value,
            } => {
                if let Value::StructInstance {
                    struct_type: _,
                    fields,
                } = self.evaluate(instance_name, environment)?
                {
                    let mut fields = fields.lock().expect("lock poisoned");
                    let evaluated_value = self.evaluate(value, environment)?;
                    fields.insert(field_name.clone(), evaluated_value.clone());
                    Ok(evaluated_value)
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}

impl StatementVisitor<Result<()>> for Interpreter {
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

    fn visit_print_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::Print(expression) => {
                let evaluated = self.evaluate(expression, environment)?;
                Ok(println!("{}", evaluated))
            }
            _ => unreachable!(),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::Let { name, initializer } => {
                let value = self.evaluate(initializer, environment)?;
                environment.define(name.clone(), value);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt.clone() {
            Statement::Struct { name, fields } => {
                // TODO: Do I need two different types here? I'm
                // converting the statement to a value.
                let value = Value::StructType {
                    name: name.clone(),
                    fields,
                };
                environment.define(name, value);
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
        match stmt.clone() {
            Statement::Function {
                name,
                parameters,
                body,
            } => {
                // TODO: Do I need two different types here? I'm
                // converting the statement to a value.
                let value = Value::Function {
                    name: name.clone(),
                    parameters,
                    body,
                };
                environment.define(name, value);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn visit_block_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::Block(statements) => {
                self.execute_block(statements, Rc::new(Environment::new(environment.clone())))
            }
            _ => unreachable!(),
        }
    }

    fn visit_if_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::If {
                condition,
                if_branch,
                else_branch,
            } => {
                if self.evaluate(condition, environment)?.unwrap_bool()? {
                    self.execute(if_branch, environment)
                } else if else_branch.is_some() {
                    self.execute(else_branch.as_ref().unwrap(), environment)
                } else {
                    Ok(())
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_while_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::While { condition, body } => {
                while self.evaluate(condition, environment)?.unwrap_bool()? {
                    self.execute(body, environment)?;
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &Statement, environment: &Rc<Environment>) -> Result<()> {
        match stmt {
            Statement::Return { expr } => {
                let value = match expr {
                    Some(value) => self.evaluate(value, environment)?,
                    None => Value::Null,
                };
                Err(InterpreterError::Return(value))
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, scanner::Scanner};

    use super::*;

    #[test]
    fn arithmetic() {
        let source = "1 + 2 * 5 / 4 - 1;";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let parser = Parser::new(tokens);
        let statements = parser.parse().unwrap();

        match &statements[0] {
            Statement::Expression(expression) => {
                let mut interpreter = Interpreter::new();
                let interpreted = interpreter
                    .evaluate(expression, &Rc::new(Environment::new_global()))
                    .unwrap();

                assert!(matches!(interpreted, Value::Number(2.5)));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn truths() {
        let sources = [
            "true;",
            "true == true;",
            "true != false;",
            "false == false;",
            "1 == 1;",
            "1 != 2;",
            "1.5 == 1.5;",
            "1.5 != 2.5;",
            r#""foo" == "foo";"#,
            r#""foo" != "bar";"#,
        ];
        for source in sources {
            let mut scanner = Scanner::new(source.to_owned());
            let tokens = scanner.scan_tokens();
            let parser = Parser::new(tokens);
            let statements = parser.parse().unwrap();

            match &statements[0] {
                Statement::Expression(expression) => {
                    let mut interpreter = Interpreter::new();
                    let interpreted = interpreter
                        .evaluate(expression, &Rc::new(Environment::new_global()))
                        .unwrap();

                    assert!(matches!(interpreted, Value::Bool(true)));
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    fn lies() {
        let sources = [
            "false;",
            "true != true;",
            "true == false;",
            "false != false;",
            "1 != 1;",
            "1 == 2;",
            "1.5 != 1.5;",
            "1.5 == 2.5;",
            r#""foo" != "foo";"#,
            r#""foo" == "bar";"#,
        ];
        for source in sources {
            let mut scanner = Scanner::new(source.to_owned());
            let tokens = scanner.scan_tokens();
            let parser = Parser::new(tokens);
            let statements = parser.parse().unwrap();

            match &statements[0] {
                Statement::Expression(expression) => {
                    let mut interpreter = Interpreter::new();
                    let interpreted = interpreter
                        .evaluate(expression, &Rc::new(Environment::new_global()))
                        .unwrap();

                    assert!(matches!(interpreted, Value::Bool(false)));
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    fn negative() {
        let source = "-1;";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let parser = Parser::new(tokens);
        let statements = parser.parse().unwrap();

        match &statements[0] {
            Statement::Expression(expression) => {
                let mut interpreter = Interpreter::new();
                let interpreted = interpreter
                    .evaluate(expression, &Rc::new(Environment::new_global()))
                    .unwrap();

                assert!(matches!(interpreted, Value::Number(-1.0)));
            }
            _ => panic!(),
        }
    }
}

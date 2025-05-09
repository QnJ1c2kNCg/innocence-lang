use std::fmt::Display;

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
}

#[derive(Default)]
pub(crate) struct Interpreter {
    environment: Environment,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Value {
    String(String),
    // TODO: Swap out to customer number type
    Number(f64),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
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
    pub(crate) fn interpret(&mut self, statements: &Vec<Statement>) -> Result<()> {
        for statement in statements {
            let res = self.execute(statement);
            if res.is_err() {
                todo!("call report error and make sure to show the line number")
            }
        }
        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<()> {
        statement.accept(self)
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Value> {
        expr.accept(self)
    }
}

impl ExpressionVisitor<Result<Value>> for Interpreter {
    fn visit_binary(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Binary {
                left,
                operation,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
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

    fn visit_unary(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Unary { operation, right } => {
                let right = self.evaluate(right)?;
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

    fn visit_grouping(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Grouping { expr } => self.evaluate(&expr),
            _ => unreachable!(),
        }
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<Value> {
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

    fn visit_variable(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            // TODO: I would like unknown variable errors to be detected
            // at scan time, not at runtime.
            Expression::Variable { id } => self
                .environment
                .get(id)
                .cloned()
                .ok_or(InterpreterError::UnknownVariable(id.clone())),
            _ => unreachable!(),
        }
    }

    fn visit_assign(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Assign { id, value } => {
                let value = self.evaluate(value)?;
                self.environment
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
}

impl StatementVisitor<Result<()>> for Interpreter {
    fn visit_expression_stmt(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            // XXX: Kinda weird that we are not returning anything here, L3441
            Statement::Expression(expression) => self.evaluate(expression).map(|_| ()),
            _ => unreachable!(),
        }
    }

    fn visit_print_stmt(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Print(expression) => {
                let evaluated = self.evaluate(expression)?;
                Ok(println!("{}", evaluated))
            }
            _ => unreachable!(),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Let { name, initializer } => {
                let value = self.evaluate(initializer)?;
                self.environment.define(name.clone(), value);
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parser, Scanner};

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
                let mut interpreter = Interpreter::default();
                let interpreted = interpreter.evaluate(&expression).unwrap();

                assert_eq!(interpreted, Value::Number(2.5));
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
                    let mut interpreter = Interpreter::default();
                    let interpreted = interpreter.evaluate(&expression).unwrap();

                    assert_eq!(interpreted, Value::Bool(true));
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
                    let mut interpreter = Interpreter::default();
                    let interpreted = interpreter.evaluate(&expression).unwrap();

                    assert_eq!(interpreted, Value::Bool(false));
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
                let mut interpreter = Interpreter::default();
                let interpreted = interpreter.evaluate(&expression).unwrap();

                assert_eq!(interpreted, Value::Number(-1.0));
            }
            _ => panic!(),
        }
    }
}

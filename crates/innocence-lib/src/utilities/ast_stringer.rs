/// Utility that converts an AST to a human readable format
use std::rc::Rc;

use crate::{
    environment::Environment,
    expressions::{Expression, ExpressionVisitor},
};

pub(crate) struct AstStringer {}

impl AstStringer {
    pub(crate) fn stringify(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        expr.accept(self, environment)
    }

    fn parenthesize(
        &mut self,
        name: &str,
        exprs: Vec<&Expression>,
        environment: &Rc<Environment>,
    ) -> String {
        let mut str = String::new();

        str.push('(');
        str.push_str(name);
        for expr in exprs {
            str.push(' ');
            str.push_str(
                expr.accept(self as &mut dyn ExpressionVisitor<String>, environment)
                    .as_str(),
            );
        }
        str.push(')');

        str
    }
}

impl ExpressionVisitor<String> for AstStringer {
    fn visit_binary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Binary {
                left,
                operation,
                right,
            } => self.parenthesize(
                operation.lexeme().as_str(),
                vec![&left, &right],
                environment,
            ),
            _ => unreachable!(),
        }
    }

    fn visit_unary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Unary { operation, right } => {
                self.parenthesize(operation.lexeme().as_str(), vec![&right], environment)
            }
            _ => unreachable!(),
        }
    }

    fn visit_grouping(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Grouping { expr } => self.parenthesize("group", vec![&expr], environment),
            _ => unreachable!(),
        }
    }

    fn visit_literal(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Literal { literal } => literal.lexeme().to_owned(),
            _ => unreachable!(),
        }
    }

    fn visit_variable(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Variable { id } => {
                format!("variable: {:?}", id)
            }
            _ => unreachable!(),
        }
    }

    fn visit_assign(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Assign { id, value } => {
                format!("assign: {:?}, {:?}", id, value)
            }
            _ => unreachable!(),
        }
    }

    fn visit_logical(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Logical {
                left,
                operation,
                right,
            } => {
                format!("logical: {:?}, {:?}, {:?}", left, operation, right)
            }
            _ => unreachable!(),
        }
    }

    fn visit_function_call(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::FunctionCall {
                callee,
                paren,
                arguments,
            } => {
                format!("function_call: {:?}, {:?}, {:?}", callee, paren, arguments)
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_initialization(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::StructInit {
                struct_type,
                fields,
            } => {
                format!("struct_init: {:?}, {:?}", struct_type, fields)
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_accessor(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::StructAccessor {
                instance_name,
                field_name,
            } => {
                format!("struct_accessor: {:?}, {:?}", instance_name, field_name)
            }
            _ => unreachable!(),
        }
    }

    fn visit_struct_setter(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::StructSetter {
                instance_name,
                field_name,
                value,
            } => {
                format!(
                    "struct_setting: {:?}, {:?}, {:?}",
                    instance_name, field_name, value
                )
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokens::{SourceLocation, Token, TokenType};

    use super::*;

    #[test]
    fn smoke_test() {
        let test_expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operation: Token::new(TokenType::Minus, SourceLocation::new()),
                right: Box::new(Expression::Literal {
                    literal: Token::new(TokenType::Number(123f64), SourceLocation::new()),
                }),
            }),
            operation: Token::new(TokenType::Star, SourceLocation::new()),
            right: Box::new(Expression::Grouping {
                expr: Box::new(Expression::Literal {
                    literal: Token::new(TokenType::Number(45.67), SourceLocation::new()),
                }),
            }),
        };

        let mut ast_stringer = AstStringer {};

        assert_eq!(
            "(* (- 123) (group 45.67))",
            ast_stringer.stringify(&test_expression, &Rc::new(Environment::new_global()))
        );
    }
}

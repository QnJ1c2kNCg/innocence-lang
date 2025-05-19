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

        return str;
    }
}

impl ExpressionVisitor<String> for AstStringer {
    fn visit_binary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Binary {
                left,
                operation,
                right,
            } => {
                return self.parenthesize(
                    operation.lexeme().as_str(),
                    vec![&left, &right],
                    environment,
                );
            }
            _ => unreachable!(),
        }
    }

    fn visit_unary(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Unary { operation, right } => {
                return self.parenthesize(operation.lexeme().as_str(), vec![&right], environment);
            }
            _ => unreachable!(),
        }
    }

    fn visit_grouping(&mut self, expr: &Expression, environment: &Rc<Environment>) -> String {
        match expr {
            Expression::Grouping { expr } => {
                return self.parenthesize("group", vec![&expr], environment);
            }
            _ => unreachable!(),
        }
    }

    fn visit_literal(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Literal { literal } => {
                return literal.lexeme().to_owned();
            }
            _ => unreachable!(),
        }
    }

    fn visit_variable(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Variable { id } => {
                return format!("variable: {:?}", id);
            }
            _ => unreachable!(),
        }
    }

    fn visit_assign(&mut self, expr: &Expression, _: &Rc<Environment>) -> String {
        match expr {
            Expression::Assign { id, value } => {
                return format!("assign: {:?}, {:?}", id, value);
            }
            _ => unreachable!(),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::tokens::{Location, Token, TokenType};

    use super::*;

    #[test]
    fn smoke_test() {
        let test_expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operation: Token::new(TokenType::Minus, Location::new()),
                right: Box::new(Expression::Literal {
                    literal: Token::new(TokenType::Number(123f64), Location::new()),
                }),
            }),
            operation: Token::new(TokenType::Star, Location::new()),
            right: Box::new(Expression::Grouping {
                expr: Box::new(Expression::Literal {
                    literal: Token::new(TokenType::Number(45.67), Location::new()),
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

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    expressions::Expression,
    logger::report_error,
    statements::Statement,
    tokens::{Token, TokenType},
};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub(crate) struct ParserError {
    token: Token,
    message: String,
}

impl ParserError {
    fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

/*
program        → statement* EOF ;
statement      → exprStmt | printStmt ;
exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
*/

// A recursive descent parser
pub(crate) struct Parser<'a> {
    tokens: &'a [Token],
    current_index: AtomicUsize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_index: AtomicUsize::new(0),
        }
    }

    pub(crate) fn parse(&self) -> Result<Vec<Statement>> {
        let mut statements = Vec::with_capacity(1024);
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn statement(&self) -> Result<Statement> {
        if self.match_(&[TokenType::Print]) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Exprect ';' after value")?;
        Ok(Statement::Print(expr))
    }

    fn expression_statement(&self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Exprect ';' after expression")?;
        Ok(Statement::Expression(expr))
    }

    fn expression(&self) -> Result<Expression> {
        self.equality()
    }

    fn equality(&self) -> Result<Expression> {
        let mut expr = self.comparison()?;

        while self.match_(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operation = self.previous();
            let right = self.comparison()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&self) -> Result<Expression> {
        let mut expr = self.term()?;

        while self.match_(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operation = self.previous();
            let right = self.term()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&self) -> Result<Expression> {
        let mut expr = self.factor()?;

        while self.match_(&[TokenType::Minus, TokenType::Plus]) {
            let operation = self.previous();
            let right = self.factor()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&self) -> Result<Expression> {
        let mut expr = self.unary()?;

        while self.match_(&[TokenType::Slash, TokenType::Star]) {
            let operation = self.previous();
            let right = self.unary()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&self) -> Result<Expression> {
        if self.match_(&[TokenType::Bang, TokenType::Minus]) {
            let operation = self.previous();
            let right = self.unary()?;
            Ok(Expression::Unary {
                operation: operation.clone(),
                right: Box::new(right),
            })
        } else {
            self.primary()
        }
    }

    fn primary(&self) -> Result<Expression> {
        if self.match_(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::Number(0f64), // TODO: This is not clean at all
            TokenType::String("".to_owned()),
        ]) {
            return Ok(Expression::Literal {
                literal: self.previous().clone(),
            });
        }
        if self.match_(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expression::Grouping {
                expr: Box::new(expr),
            });
        }
        Err(ParserError::new(
            self.current().clone(),
            "Expected expression.".to_owned(),
        ))
    }

    // TODO: This can probably be refactored to use an actual `match`
    fn match_(&self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return &self.current().token_type == token_type;
    }

    fn advance(&self) -> &Token {
        if !self.is_at_end() {
            self.current_index.fetch_add(1, Ordering::Relaxed);
        }
        return self.previous();
    }

    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }

    fn current(&self) -> &Token {
        self.tokens
            .get(self.current_index.load(Ordering::Relaxed))
            .unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens
            .get(self.current_index.load(Ordering::Relaxed) - 1)
            .unwrap()
    }

    fn consume(&self, token_type: &TokenType, message: &str) -> Result<&Token> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let current_token = self.current().clone();

            report_error(&current_token, message);
            Err(ParserError::new(self.current().clone(), message.to_owned()))
        }
    }

    fn synchronize(self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.current().token_type {
                TokenType::Struct
                | TokenType::Fn
                | TokenType::Let
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{AstStringer, Scanner};

    use super::*;

    #[test]
    fn parser_arithmetic_precedence() {
        let source = "1 + 2 * 3 / 4;";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let parser = Parser::new(tokens);
        let statements = parser.parse().unwrap();

        match &statements[0] {
            Statement::Expression(expression) => {
                let mut ast_stringer = AstStringer {};
                assert_eq!("(+ 1 (/ (* 2 3) 4))", ast_stringer.stringify(&expression));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn parser_grouping_ok() {
        let source = "(1 + 2) * 3 / 4;";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let parser = Parser::new(tokens);
        let statements = parser.parse().unwrap();

        match &statements[0] {
            Statement::Expression(expression) => {
                let mut ast_stringer = AstStringer {};
                assert_eq!(
                    "(/ (* (group (+ 1 2)) 3) 4)",
                    ast_stringer.stringify(&expression)
                );
            }
            _ => panic!(),
        }
    }

    #[test]
    fn parser_grouping_err() {
        let source = "(1 + 2 * 3 / 4;";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let parser = Parser::new(tokens);
        let err = parser.parse().unwrap_err();

        assert_eq!(err.message, "Expect ')' after expression.")
    }
}

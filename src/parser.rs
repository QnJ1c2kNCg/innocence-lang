use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    expressions::Expression,
    logger::report_error,
    statements::Statement,
    tokens::{Identifier, Token, TokenType},
};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub(crate) enum ParserError {
    DoesNotRequireSynchronization(String),
    Recoverable,
    Unrecoverable { token: Token, message: String },
}

impl ParserError {
    fn new_unrecoverable(token: Token, message: String) -> Self {
        Self::Unrecoverable { token, message }
    }
}

/*
program        → declaration* EOF ;
declaration    → varDecl | statement ;
varDecl        → "let" IDENTIFIER ( "=" expression )? ";" ;
statement      → exprStmt | ifStmt | printStmt | block ;
ifStmt         → "if" expression "{" statement "}" ( "else" "{" statement "}" )? ;
exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
block          → "{" declaration* "}" ;
expression     → assignment ;
assignment     → IDENTIFIER "=" assignment | equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → "true" | "false" | "nil" | NUMBER | STRING | "(" expression ")" | IDENTIFIER ;

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
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&self) -> Result<Statement> {
        let res = if self.match_(&[TokenType::Let]) {
            self.let_declaration()
        } else {
            self.statement()
        };

        res.map_err(|err| {
            self.synchronize();
            println!("error and synchronize: TODO fix message: {:?}", err);
            err
        })
    }

    fn let_declaration(&self) -> Result<Statement> {
        let name = self.consume_expected_token(
            &TokenType::Identifier(Identifier::new("".to_owned())),
            "Expect variable name.",
        )?;

        let name = match &name.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => unreachable!(),
        };

        self.consume_expected_token(&TokenType::Equal, "Expect `=` sign.")?;
        let initializer = self.expression()?;
        self.consume_expected_token(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Statement::Let { name, initializer })
    }

    fn statement(&self) -> Result<Statement> {
        if self.match_(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_(&[TokenType::LeftBrace]) {
            Ok(Statement::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&self) -> Result<Statement> {
        let condition = self.expression()?;

        let if_branch = Box::new(self.statement()?);
        let else_branch = if self.match_(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        // TODO: proper error handling and check the else too
        assert!(matches!(*if_branch, Statement::Block(_)));

        Ok(Statement::If {
            condition,
            if_branch,
            else_branch,
        })
    }

    fn print_statement(&self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume_expected_token(&TokenType::Semicolon, "Exprect ';' after value")?;
        Ok(Statement::Print(expr))
    }

    /// Scoping block
    fn block(&self) -> Result<Vec<Statement>> {
        let mut statements = Vec::default();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume_expected_token(&TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn expression_statement(&self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume_expected_token(&TokenType::Semicolon, "Exprect ';' after expression")?;
        Ok(Statement::Expression(expr))
    }

    fn expression(&self) -> Result<Expression> {
        self.assignment()
    }

    fn assignment(&self) -> Result<Expression> {
        let expr = self.equality()?;

        // We use recursion to handle unbounded l-value assignments
        if self.match_(&[TokenType::Equal]) {
            let value = self.assignment()?;

            return if let Expression::Variable { id } = expr {
                Ok(Expression::Assign {
                    id,
                    value: Box::new(value),
                })
            } else {
                Err(ParserError::DoesNotRequireSynchronization(
                    "Invalid assignemnt target.".to_owned(),
                ))
            };
        }

        Ok(expr)
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
        if self.match_(&[TokenType::Identifier(Identifier::new("".to_owned()))]) {
            return Ok(Expression::Variable {
                id: Identifier::new(self.previous().lexeme()),
            });
        }

        if self.match_(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume_expected_token(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expression::Grouping {
                expr: Box::new(expr),
            });
        }
        Err(ParserError::new_unrecoverable(
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

    /// Advance the parser and return the next token
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

    /// Advances the parser if the next token is `expected_token_type`.
    /// If the token does not match, an error is returned with the provided `message`.
    fn consume_expected_token(
        &self,
        expected_token_type: &TokenType,
        message: &str,
    ) -> Result<&Token> {
        if self.check(expected_token_type) {
            Ok(self.advance())
        } else {
            let current_token = self.current().clone();

            report_error(&current_token, message);
            Err(ParserError::new_unrecoverable(
                self.current().clone(),
                message.to_owned(),
            ))
        }
    }

    fn synchronize(&self) {
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
    use std::rc::Rc;

    use crate::Scanner;
    use crate::environment::Environment;
    use crate::utilities::ast_stringer::AstStringer;

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
                assert_eq!(
                    "(+ 1 (/ (* 2 3) 4))",
                    ast_stringer.stringify(&expression, &Rc::new(Environment::new_global()))
                );
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
                    ast_stringer.stringify(&expression, &Rc::new(Environment::new_global()))
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

        match err {
            ParserError::Unrecoverable { token: _, message } => {
                assert_eq!(message, "Expect ')' after expression.")
            }
            _ => panic!(),
        }
    }
}

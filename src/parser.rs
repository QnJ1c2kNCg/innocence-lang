use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    expressions::Expression,
    tokens::{Token, TokenType},
};

/*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
*/

// A recursive descent parser
struct Parser {
    tokens: Vec<Token>,
    current_index: AtomicUsize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_index: AtomicUsize::new(0),
        }
    }

    fn expression(&self) -> Expression {
        self.equality()
    }

    fn equality(&self) -> Expression {
        let mut expr = self.comparison();

        while self.match_(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operation = self.previous();
            let right = self.comparison();
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        expr
    }

    fn comparison(&self) -> Expression {
        let mut expr = self.term();

        while self.match_(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operation = self.previous();
            let right = self.term();
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        expr
    }

    fn term(&self) -> Expression {
        let mut expr = self.factor();

        while self.match_(&[TokenType::Minus, TokenType::Plus]) {
            let operation = self.previous();
            let right = self.factor();
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        expr
    }

    fn factor(&self) -> Expression {
        let mut expr = self.unary();

        while self.match_(&[TokenType::Slash, TokenType::Star]) {
            let operation = self.previous();
            let right = self.unary();
            expr = Expression::Binary {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            };
        }

        expr
    }

    fn unary(&self) -> Expression {
        if self.match_(&[TokenType::Bang, TokenType::Minus]) {
            let operation = self.previous();
            let right = self.unary();
            Expression::Unary {
                operation: operation.clone(),
                right: Box::new(right),
            }
        } else {
            self.primary()
        }
    }

    fn primary(&self) -> Expression {
        if self.match_(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::Number(0f64), // TODO: This is not clean at all
            TokenType::String("".to_owned()),
        ]) {
            return Expression::Literal {
                literal: self.previous().clone(),
            };
        }
        if self.match_(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, "Expect ')' after expression.");
            return Expression::Grouping {
                expr: Box::new(expr),
            };
        }
        todo!()
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

    fn consume(&self, right_paren: TokenType, arg: &str) -> () {
        todo!()
    }
}

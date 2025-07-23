/// Everything related to the [`Parser`]. Parsing is the second phase
/// of the innocence interpreter (read [`Scanner`] doc for the first phase).
/// The [`Parser`] reads the [`Token`]s produced by the [`Scanner`].
///
/// This [`Parser`] is implemented using a top-down recursive descent approach.
/// This means that there is a function to handle every non-terminal grammar. The
/// parser will go through the rules of the grammar. Confusingly, the grammar rules
/// are ordered in reverse from a precedence perspective (the first rule has the lowest
/// precedence). In this top-down parser, you reach the lowest-precedence expressions first
/// because they may in turn contain subexpressions of higher precedence.
///
/// Here are innocence's grammar rules in English:
///  program        → declaration* EOF ;
///  declaration    → structDecl | funDecl | letDecl | statement ;
///  structDecl     → "struct" IDENTIFIER "{" parameters* "}" ;
///  funDecl        → "fn" function ;
///  function       → IDENTIFIER "(" parameters? ")" (-> IDENTIFIER)? block ;
///  parameters     → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" IDENTIFIER )* ;
///  letDecl        → "let" IDENTIFIER (":" IDENTIFIER)? "=" expression ";" ;
///  statement      → exprStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
///  exprStmt       → expression ";" ;
///  ifStmt         → "if" expression "{" statement "}" ( "else" "{" statement "}" )? ;
///  printStmt      → "print" expression ";" ;
///  returnStmt     → "return" expression? ";" ;
///  whileStmt      → "while" expression "{" statement "}" ;
///  block          → "{" declaration* "}" ;
///  expression     → assignment ;
///  assignment     → (call "." )? IDENTIFIER "=" assignment | logic_or ;
///  logic_or       → logic_and ( "or" logic_and )* ;
///  logic_and      → equality ( "and" equality )* ;
///  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
///  term           → factor ( ( "-" | "+" ) factor )* ;
///  factor         → unary ( ( "/" | "*" ) unary )* ;
///  unary          → ( "!" | "-" ) unary | ( structInit | call ) ;
///  structInit     → primary "{" ( assignment "," )* "}" | call ;
///  call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
///  arguments      → expression ( "," expression )* ;
///  primary        → "true" | "false" | "nil" | NUMBER | STRING | "(" expression ")" | IDENTIFIER ;
///
/// The [`Parser`] will produce a list of [`Statement`]s (which themselves contain [`Expression`]s).
/// These statements are what form our abstract syntax tree (AST) and will later be interpreted by [`Interpreter`].
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    expressions::Expression,
    logger::report_error,
    semantic_analysis::type_checker::TypeInfo,
    statements::{Parameter, Statement},
    tokens::{Identifier, Token, TokenType},
};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub(crate) enum ParserError {
    DoesNotRequireSynchronization(String),
    Recoverable,
    Unrecoverable { token: Token, message: String },
}

enum FunctionKind {
    Function,
}

impl std::fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &FunctionKind::Function => write!(f, "function"),
        }
    }
}

impl ParserError {
    fn new_unrecoverable(token: Token, message: String) -> Self {
        Self::Unrecoverable { token, message }
    }
}

/// A top-down recursive descent parser, read file documentation for more info.
pub(crate) struct Parser<'a> {
    /// The series of [`Token`]s produced by the [`Scanner`].
    tokens: &'a [Token],
    /// The index of the [`Token`] currently being looked at.
    current_index: AtomicUsize,
}

/// Implements the [`Parser`] grammar processing functions.
/// Since this parser is implemented using recursive descent
/// there is roughly one function here for each rule in the grammar.
impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_index: AtomicUsize::new(0),
        }
    }

    /// Entry point of the [`Parser`], will read all of the tokens and produce a
    /// list of [`Statement`]s.
    pub(crate) fn parse(&self) -> Result<Vec<Statement>> {
        // TODO: better heuristic for capacity
        let mut statements = Vec::with_capacity(1024);
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    /// Grammar rule for declarations. Currently checks if this a variable declaration,
    /// otherwise _descends_ to the next grammar rule to handle statement.
    /// NOTE: Refer to the file level rustdoc to see all grammar rules.
    fn declaration(&self) -> Result<Statement> {
        let res = if self.match_(&[TokenType::Struct]) {
            self.struct_declaration()
        } else if self.match_(&[TokenType::Fn]) {
            self.function_declaration(FunctionKind::Function)
        } else if self.match_(&[TokenType::Let]) {
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

    fn struct_declaration(&self) -> Result<Statement> {
        let name = self.consume_expected_token(
            &TokenType::Identifier(Identifier::any()),
            "Expect struct name.",
        )?;

        self.consume_expected_token(&TokenType::LeftBrace, "Expect '{' before struct body.")?;

        let mut fields = Vec::new();

        while !self.check(&TokenType::RightBrace) {
            let identifier = Identifier::new(
                self.consume_expected_token(
                    &TokenType::Identifier(Identifier::any()),
                    "Expect parameter name.",
                )?
                .lexeme(),
            );
            self.consume_expected_token(
                &TokenType::Colon,
                &format!("Expect ':' after parameter name ({}).", identifier),
            )?;
            let type_info = self.consume_type_info()?;
            fields.push(Parameter::new(identifier, type_info));
            if !self.match_(&[TokenType::Comma]) || self.is_at_end() {
                break;
            }
        }
        self.consume_expected_token(&TokenType::RightBrace, "Expect '}' after struct body.")?;

        Ok(Statement::Struct {
            name: Identifier::new(name.lexeme()),
            fields,
        })
    }

    fn function_declaration(&self, kind: FunctionKind) -> Result<Statement> {
        let name = self.consume_expected_token(
            &TokenType::Identifier(Identifier::any()),
            &format!("Expect {} name.", kind),
        )?;

        self.consume_expected_token(
            &TokenType::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;

        let mut parameters = Vec::new();
        // if we don't immediately see the right paren it means
        // that there are parameters
        if !self.check(&TokenType::RightParen) {
            loop {
                let identifier = Identifier::new(
                    self.consume_expected_token(
                        &TokenType::Identifier(Identifier::any()),
                        "Expect parameter name.",
                    )?
                    .lexeme(),
                );
                self.consume_expected_token(
                    &TokenType::Colon,
                    &format!("Expect ':' after parameter name ({}).", identifier),
                )?;
                let type_info = self.consume_type_info()?;
                parameters.push(Parameter::new(identifier, type_info));
                if !self.match_(&[TokenType::Comma]) || self.is_at_end() {
                    break;
                }
            }
        }

        self.consume_expected_token(
            &TokenType::RightParen,
            "Expect closing parenthesis after parameters.",
        )?;

        // Check if there is a return type annotation
        let return_type_info: Option<TypeInfo> = if self.check(&TokenType::Arrow) {
            self.advance();
            Some(self.consume_type_info()?)
        } else {
            None
        };

        self.consume_expected_token(
            &TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;

        let body = Box::new(Statement::Block(self.block()?));

        Ok(Statement::Function {
            name: Identifier::new(name.lexeme()),
            parameters,
            return_type_info,
            body,
        })
    }

    fn let_declaration(&self) -> Result<Statement> {
        let name = self.consume_expected_token(
            &TokenType::Identifier(Identifier::any()),
            "Expect variable name.",
        )?;

        let name = match &name.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => unreachable!(),
        };

        // Check for the optional type annotation
        let type_info: Option<TypeInfo> = if self.check(&TokenType::Colon) {
            self.advance();
            Some(self.consume_type_info()?)
        } else {
            None
        };

        self.consume_expected_token(&TokenType::Equal, "Expect `=` sign.")?;
        let initializer = if self.check_next(&TokenType::LeftBrace) {
            self.struct_init()?
        } else {
            self.expression()?
        };
        self.consume_expected_token(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Statement::Let {
            name,
            type_info,
            initializer,
        })
    }

    fn statement(&self) -> Result<Statement> {
        if self.match_(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_(&[TokenType::Return]) {
            self.return_statement()
        } else if self.match_(&[TokenType::While]) {
            self.while_statement()
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

    fn return_statement(&self) -> Result<Statement> {
        let expr = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume_expected_token(&TokenType::Semicolon, "Expect ';' after return value.")?;

        Ok(Statement::Return { expr })
    }

    fn while_statement(&self) -> Result<Statement> {
        let condition = self.expression()?;

        let body = Box::new(self.statement()?);

        // TODO: proper error handling and check the else too
        assert!(matches!(*body, Statement::Block(_)));

        Ok(Statement::While { condition, body })
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
        self.consume_expected_token(&TokenType::Semicolon, "Expect ';' after expression")?;
        Ok(Statement::Expression(expr))
    }

    fn expression(&self) -> Result<Expression> {
        self.assignment()
    }

    fn assignment(&self) -> Result<Expression> {
        let expr = self.or()?;

        // We use recursion to handle unbounded l-value assignments
        if self.match_(&[TokenType::Equal]) {
            let value = self.assignment()?;

            return match expr {
                Expression::Variable { id } => Ok(Expression::Assign {
                    id,
                    value: Box::new(value),
                }),
                // we parsed `expr` as a struct accessor, however we how found an equal sign (`=`)
                // this means that it's actually a struct setter, we thus convert it
                Expression::StructAccessor {
                    instance_name,
                    field_name,
                } => Ok(Expression::StructSetter {
                    instance_name,
                    field_name,
                    value: Box::new(value),
                }),
                _ => Err(ParserError::DoesNotRequireSynchronization(
                    "Invalid assignment target.".to_owned(),
                )),
            };
        }

        Ok(expr)
    }

    fn or(&self) -> Result<Expression> {
        let mut expr = self.and()?;

        while self.match_(&[TokenType::Or]) {
            let operation = self.previous();
            let right = self.and()?;

            expr = Expression::Logical {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn and(&self) -> Result<Expression> {
        let mut expr = self.equality()?;

        while self.match_(&[TokenType::And]) {
            let operation = self.previous();
            let right = self.equality()?;

            expr = Expression::Logical {
                left: Box::new(expr),
                operation: operation.clone(),
                right: Box::new(right),
            }
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
            // at this point, we are either calling a function or initializing
            // a struct (or a primary expression).

            let expr = self.primary()?;
            self.function_call(expr)
        }
    }

    fn struct_init(&self) -> Result<Expression> {
        let struct_type = self.consume_expected_token(
            &TokenType::Identifier(Identifier::any()),
            "Expect struct type name.",
        )?;

        let struct_type = match &struct_type.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => unreachable!(),
        };
        self.consume_expected_token(
            &TokenType::LeftBrace,
            "Expect '{' before struct initialization.",
        )?;
        let mut fields = Vec::new();
        while !self.check(&TokenType::RightBrace) {
            let assignment = self.assignment()?;

            if let Expression::Assign { id, value } = assignment {
                fields.push((id, *value));
            } else {
                return Err(ParserError::new_unrecoverable(
                    self.current().clone(),
                    "Expected an assignment using '='.".to_owned(),
                ));
            }

            self.consume_expected_token(
                &TokenType::Comma,
                "Expect ',' after each field during struct initialization.",
            )?;
        }
        self.consume_expected_token(
            &TokenType::RightBrace,
            "Expect '}' after struct initialization.",
        )?;
        Ok(Expression::StructInit {
            struct_type,
            fields,
        })
    }

    fn function_call(&self, mut expr: Expression) -> Result<Expression> {
        // after parsing the expression, we are seeing an open paren,
        // this means we are calling a function, we thus call `finish_call`
        loop {
            if self.match_(&[TokenType::LeftParen]) {
                // the expr here is the function callee
                expr = self.finish_call(expr)?;
            } else if self.match_(&[TokenType::Dot]) {
                // here we are accessing a field within a struct
                let field_name = self.consume_expected_token(
                    &TokenType::Identifier(Identifier::any()),
                    "Expect property after '.'.",
                )?;

                expr = Expression::StructAccessor {
                    instance_name: Box::new(expr),
                    field_name: Identifier::new(field_name.lexeme()),
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&self, callee: Expression) -> Result<Expression> {
        let mut arguments = Vec::new();

        // if we don't immediately see the right parent it means
        // that there are arguments
        if !self.check(&TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume_expected_token(
            &TokenType::RightParen,
            "Expect closing parenthesis after function call.",
        )?;

        // doesn't really matter for the interpreter but might matter if
        // we compile of bytecode at some point
        assert!(arguments.len() < 256, "too many function arguments");

        Ok(Expression::FunctionCall {
            callee: Box::new(callee),
            paren: paren.clone(),
            arguments,
        })
    }

    fn primary(&self) -> Result<Expression> {
        if self.match_(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::Number(0f64), // TODO: this is not clean at all
            TokenType::String("".to_owned()),
        ]) {
            return Ok(Expression::Literal {
                literal: self.previous().clone(),
            });
        }
        if self.match_(&[TokenType::Identifier(Identifier::any())]) {
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
}

/// Implements the [`Parser`] utility functions, these are the functions
/// needed to traverse the tokens but are not related to the grammar itself.
impl<'a> Parser<'a> {
    /// Check if [`Token`] at `self.current_index` is of a type
    /// present in `token_types`.
    fn match_(&self, token_types: &[TokenType]) -> bool {
        // TODO: this can probably be refactored to use an actual `match`
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    /// Check if [`Token`] at `self.current_index` is of type `token_type`.
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.current().token_type == token_type
    }

    /// Same as [`check`], but looks at next instead of current index
    fn check_next(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.next().token_type == token_type
    }

    /// Advance the parser and return the next [`Token`].
    fn advance(&self) -> &Token {
        if !self.is_at_end() {
            self.current_index.fetch_add(1, Ordering::Relaxed);
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }

    /// Getter for the current [`Token`].
    fn current(&self) -> &Token {
        self.tokens
            .get(self.current_index.load(Ordering::Relaxed))
            .unwrap()
    }

    /// Getter for the previous [`Token`].
    fn previous(&self) -> &Token {
        self.tokens
            .get(self.current_index.load(Ordering::Relaxed) - 1)
            .unwrap()
    }

    // Getter for the next [`Token`]
    fn next(&self) -> &Token {
        // TODO: Remove the panickness
        self.tokens
            .get(self.current_index.load(Ordering::Relaxed) + 1)
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

    /// This function can be used when the parser encounters an error, instead
    /// of aborting the entire parsing, `synchronize` can be use tomove to the next
    /// statement. This is to provide better UX and report many errors at once.
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
                | TokenType::Return => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// This function parses the identifier associated with the type annotation.
    /// This function should be called after the colon (":") was parsed.
    fn consume_type_info(&self) -> Result<TypeInfo> {
        let type_identifier = self.consume_expected_token(
            &TokenType::Identifier(Identifier::any()),
            "Expect type annotation after ':'.",
        )?;

        let type_identifier = match &type_identifier.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => unreachable!(),
        };

        Ok(type_identifier.into())
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::environment::Environment;
    use crate::scanner::Scanner;
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
                    ast_stringer.stringify(expression, &Rc::new(Environment::new_global()))
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
                    ast_stringer.stringify(expression, &Rc::new(Environment::new_global()))
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

/// Everything related to the [`Scanner`]. Scanning is the first phase
/// of the innocence interpreter. This phase reads the raw source code
/// and converts it to a series of [`Token`]s. The tokens will later be
/// parsed by the [`Parser`] to be converts in an AST.
use crate::tokens::{SourceLocation, Token, TokenType};
use std::str::FromStr;

type Result<T> = std::result::Result<T, ScannerError>;

#[derive(Debug)]
pub(crate) enum ScannerError {
    EndOfSource,
    NotAKeyword,
}

/// Core construct of the scanning phase. This is what is responsible
/// to convert raw source code to [`Token`]s.
pub(crate) struct Scanner {
    /// The raw source.
    source: String,
    /// Tracker for where we are currently in the source.
    lexeme_current_index: usize,
    /// Tracker for where we are current scanning, this is useful for error reporting.
    scan_location: SourceLocation,
    /// The _output_ of the [`Scanner`], the produced tokens.
    tokens: Option<Vec<Token>>,
}

impl Scanner {
    pub(crate) fn new(source: String) -> Scanner {
        Scanner {
            source,
            lexeme_current_index: 0,
            scan_location: SourceLocation::new(),
            tokens: None,
        }
    }

    /// Tells the [`Scanner`] to start processing the source and produce the tokens.
    pub(crate) fn scan_tokens(&mut self) -> &Vec<Token> {
        if self.tokens.is_none() {
            // TODO: use with_capacity
            let mut tokens = Vec::new();
            loop {
                match self.scan_token() {
                    Ok(Some(token)) => tokens.push(token),
                    // `scan_token` can return `None` for things like comments (`//`)
                    Ok(None) => (),
                    Err(e) => match e {
                        ScannerError::EndOfSource => break,
                        _ => unreachable!(),
                    },
                }
            }
            tokens.push(Token::new(TokenType::Eof, self.scan_location));
            self.tokens = Some(tokens);
        }
        self.tokens.as_ref().unwrap()
    }

    /// Move character by character to identify which is the next [`Token`].
    fn scan_token(&mut self) -> Result<Option<Token>> {
        let c = self.advance()?;
        let token = match c {
            '(' => Some(Token::new(TokenType::LeftParen, self.scan_location)),
            ')' => Some(Token::new(TokenType::RightParen, self.scan_location)),
            '{' => Some(Token::new(TokenType::LeftBrace, self.scan_location)),
            '}' => Some(Token::new(TokenType::RightBrace, self.scan_location)),
            ',' => Some(Token::new(TokenType::Comma, self.scan_location)),
            '.' => Some(Token::new(TokenType::Dot, self.scan_location)),
            '-' => Some(Token::new(TokenType::Minus, self.scan_location)),
            '+' => Some(Token::new(TokenType::Plus, self.scan_location)),
            ':' => Some(Token::new(TokenType::Colon, self.scan_location)),
            ';' => Some(Token::new(TokenType::Semicolon, self.scan_location)),
            '*' => Some(Token::new(TokenType::Star, self.scan_location)),
            '!' => {
                if self.conditional_advance('=')? {
                    Some(Token::new(TokenType::BangEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Bang, self.scan_location))
                }
            }
            '=' => {
                if self.conditional_advance('=')? {
                    Some(Token::new(TokenType::EqualEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Equal, self.scan_location))
                }
            }
            '<' => {
                if self.conditional_advance('=')? {
                    Some(Token::new(TokenType::LessEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Less, self.scan_location))
                }
            }
            '>' => {
                if self.conditional_advance('=')? {
                    Some(Token::new(TokenType::GreaterEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Greater, self.scan_location))
                }
            }
            '/' => {
                if self.conditional_advance('/')? {
                    // A comment, we need to advance until the end of line
                    while !self.conditional_advance('\n')? {
                        self.lexeme_current_index += 1;
                    }
                    self.scan_location.increment_line(1);
                    None
                } else {
                    Some(Token::new(TokenType::Slash, self.scan_location))
                }
            }
            ' ' | '\r' | '\t' => None,
            '"' => Some(self.string_literal()?),
            '\n' => {
                self.scan_location.increment_line(1);
                None
            }
            c if c.is_ascii_digit() => Some(self.number_literal()?),
            c if c.is_alphanumeric() => Some(self.identifier()?),
            c => panic!("[{:?}] Unrecognized character: {}", self.scan_location, c),
        };

        Ok(token)
    }

    /// Utility function that advance the [`lexeme_current_index`] by one, returning the
    /// _consumed_ character. This function will return a [`ScannerError::EndOfSource`]
    /// if there are no more characters to consume.
    fn advance(&mut self) -> Result<char> {
        self.end_reached(self.lexeme_current_index)?;
        let c = self.char_at(self.lexeme_current_index);

        self.lexeme_current_index += 1;
        Ok(c)
    }

    /// Only advances if the `expected` char matches the current index
    fn conditional_advance(&mut self, expected: char) -> Result<bool> {
        self.end_reached(self.lexeme_current_index)?;
        let c = self.char_at(self.lexeme_current_index);

        if c == expected {
            self.lexeme_current_index += 1;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Advances the [`lexeme_current_index`] until the `matched` function returns true.
    /// This function is useful to capture things like string or number literal.
    fn advance_until_and_capture(
        &self,
        start_index: usize,
        matched: fn(char) -> bool,
    ) -> Result<String> {
        let mut index = start_index;
        loop {
            match self.end_reached(index) {
                Ok(()) => {
                    if matched(self.char_at(index)) {
                        return Ok(String::from(&self.source[start_index..index]));
                    }
                    index += 1;
                }
                Err(ScannerError::EndOfSource) if start_index < index => {
                    return Ok(String::from(&self.source[start_index..index]));
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Getter helper.
    fn char_at(&self, index: usize) -> char {
        self.source
            .chars()
            .nth(index)
            .expect("`lexeme_current_index` should never be out of bound")
    }

    fn end_reached(&self, index: usize) -> Result<()> {
        if index >= self.source.len() {
            Err(ScannerError::EndOfSource)
        } else {
            Ok(())
        }
    }

    /// Consumes the characters composing a string literal.
    fn string_literal(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index, |c| c == '"')?;
        // the +1 here is so that _eat_ the closing `"`
        self.lexeme_current_index += literal.len() + 1;
        Ok(Token::new(TokenType::String(literal), self.scan_location))
    }

    /// Consumes the characters composing a number literal.
    fn number_literal(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index - 1, |c| {
            !(c.is_ascii_digit() || c == '.')
        });
        let literal = literal.unwrap();
        self.lexeme_current_index += literal.len() - 1;
        Ok(Token::new(
            TokenType::Number(f64::from_str(&literal).unwrap()),
            self.scan_location,
        ))
    }

    /// Consumes the characters composing an identifier.
    fn identifier(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index - 1, |c| {
            !(c.is_alphanumeric() || c == '_')
        })?;
        self.lexeme_current_index += literal.len() - 1;
        // check if the identifier is actually a keyword
        let token_type =
            TokenType::try_from(literal.as_str()).unwrap_or(TokenType::Identifier(literal.into()));
        Ok(Token::new(token_type, self.scan_location))
    }
}

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use super::*;

    #[test]
    fn smoke_test() {
        let source = r#"// this is a comment
            (( )){} // grouping stuff
            .,;:!*+-/=<> <= >= == // operators
            "string literal" "another one"
            123 4.5 67.89 10.0
            fn struct if else for while
            an_identifier AnotherOne
            "#;

        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();

        let expected_tokens = vec![
            Token::new(TokenType::LeftParen, 2.into()),
            Token::new(TokenType::LeftParen, 2.into()),
            Token::new(TokenType::RightParen, 2.into()),
            Token::new(TokenType::RightParen, 2.into()),
            Token::new(TokenType::LeftBrace, 2.into()),
            Token::new(TokenType::RightBrace, 2.into()),
            Token::new(TokenType::Dot, 3.into()),
            Token::new(TokenType::Comma, 3.into()),
            Token::new(TokenType::Semicolon, 3.into()),
            Token::new(TokenType::Colon, 3.into()),
            Token::new(TokenType::Bang, 3.into()),
            Token::new(TokenType::Star, 3.into()),
            Token::new(TokenType::Plus, 3.into()),
            Token::new(TokenType::Minus, 3.into()),
            Token::new(TokenType::Slash, 3.into()),
            Token::new(TokenType::Equal, 3.into()),
            Token::new(TokenType::Less, 3.into()),
            Token::new(TokenType::Greater, 3.into()),
            Token::new(TokenType::LessEqual, 3.into()),
            Token::new(TokenType::GreaterEqual, 3.into()),
            Token::new(TokenType::EqualEqual, 3.into()),
            Token::new(TokenType::String("string literal".to_owned()), 4.into()),
            Token::new(TokenType::String("another one".to_owned()), 4.into()),
            Token::new(TokenType::Number(123f64), 5.into()),
            Token::new(TokenType::Number(4.5), 5.into()),
            Token::new(TokenType::Number(67.89), 5.into()),
            Token::new(TokenType::Number(10.0), 5.into()),
            Token::new(TokenType::Fn, 6.into()),
            Token::new(TokenType::Struct, 6.into()),
            Token::new(TokenType::If, 6.into()),
            Token::new(TokenType::Else, 6.into()),
            Token::new(TokenType::For, 6.into()),
            Token::new(TokenType::While, 6.into()),
            Token::new(TokenType::Identifier("an_identifier".into()), 7.into()),
            Token::new(TokenType::Identifier("AnotherOne".into()), 7.into()),
            Token::new(TokenType::Eof, 8.into()),
        ];

        assert_eq!(tokens.len(), expected_tokens.len());
        for (t, expected) in zip(tokens, expected_tokens) {
            assert_eq!(*t, expected);
        }
    }

    #[test]
    fn single_line() {
        let source = "1 + 2";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();

        let expected_tokens = vec![
            Token::new(TokenType::Number(1f64), 1.into()),
            Token::new(TokenType::Plus, 1.into()),
            Token::new(TokenType::Number(2f64), 1.into()),
            Token::new(TokenType::Eof, 1.into()),
        ];

        assert_eq!(tokens.len(), expected_tokens.len());
        for (t, expected) in zip(tokens, expected_tokens) {
            assert_eq!(*t, expected);
        }
    }
}

use crate::tokens::{Location, Token, TokenType};
use std::str::FromStr;

type Result<T> = std::result::Result<T, ScannerError>;

pub(crate) enum ScannerError {
    EndOfSource,
    NotAKeyword,
}

pub(crate) struct Scanner {
    source: String,
    lexeme_start_index: usize,
    lexeme_current_index: usize,
    scan_location: Location,
    tokens: Option<Vec<Token>>,
}

impl Scanner {
    pub(crate) fn new(source: String) -> Scanner {
        Scanner {
            source,
            lexeme_start_index: 0,
            lexeme_current_index: 0,
            scan_location: Location::new(),
            tokens: None,
        }
    }

    pub(crate) fn scan_tokens(&mut self) -> &Vec<Token> {
        if self.tokens.is_none() {
            let mut tokens = Vec::new();
            loop {
                match self.scan_token() {
                    Ok(Some(token)) => tokens.push(token),
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

    fn scan_token(&mut self) -> Result<Option<Token>> {
        let c = self.advance()?;
        //dbg!(c);
        let token = match c {
            '(' => Some(Token::new(TokenType::LeftParen, self.scan_location)),
            ')' => Some(Token::new(TokenType::RightParen, self.scan_location)),
            '{' => Some(Token::new(TokenType::LeftBrace, self.scan_location)),
            '}' => Some(Token::new(TokenType::RightBrace, self.scan_location)),
            ',' => Some(Token::new(TokenType::Comma, self.scan_location)),
            '.' => Some(Token::new(TokenType::Dot, self.scan_location)),
            '-' => Some(Token::new(TokenType::Minus, self.scan_location)),
            '+' => Some(Token::new(TokenType::Plus, self.scan_location)),
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
            c if c.is_digit(10) => Some(self.number_literal()?),
            c if c.is_alphanumeric() => Some(self.identifier()?),
            c => panic!("[{:?}] Unrecognized character: {}", self.scan_location, c),
        };

        Ok(token)
    }

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

    fn advance_until_and_capture(
        &self,
        start_index: usize,
        matched: fn(char) -> bool,
    ) -> Result<String> {
        let mut index = start_index;
        loop {
            self.end_reached(index)?;
            if matched(self.char_at(index)) {
                return Ok(String::from(&self.source[start_index..index]));
            }
            index += 1;
        }
    }

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

    fn string_literal(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index, |c| c == '"')?;
        // The +1 here is so that _eat_ the closing "
        self.lexeme_current_index += literal.len() + 1;
        Ok(Token::new(TokenType::String(literal), self.scan_location))
    }

    fn number_literal(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index - 1, |c| {
            !(c.is_digit(10) || c == '.')
        })?;
        self.lexeme_current_index += literal.len() - 1;
        Ok(Token::new(
            TokenType::Number(f64::from_str(&literal).unwrap()),
            self.scan_location,
        ))
    }

    fn identifier(&mut self) -> Result<Token> {
        let literal = self.advance_until_and_capture(self.lexeme_current_index - 1, |c| {
            !(c.is_alphanumeric() || c == '_')
        })?;
        self.lexeme_current_index += literal.len() - 1;
        // Check if the identifier is actually a keyword
        let token_type =
            TokenType::try_from(literal.as_str()).unwrap_or(TokenType::Identifier(literal));
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
            .,;!*+-/=<> <= >= == // operators
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
            Token::new(TokenType::Identifier("an_identifier".to_owned()), 7.into()),
            Token::new(TokenType::Identifier("AnotherOne".to_owned()), 7.into()),
            Token::new(TokenType::Eof, 8.into()),
        ];

        dbg!(tokens);
        assert_eq!(tokens.len(), expected_tokens.len());
        for (t, expected) in zip(tokens, expected_tokens) {
            assert_eq!(*t, expected);
        }
    }
}

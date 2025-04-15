use crate::tokens::{Location, Token, TokenType};

pub(crate) type Result<T> = std::result::Result<T, ScannerError>;

enum ScannerError {
    EndOfSource,
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
                if self.conditional_advance('=') {
                    Some(Token::new(TokenType::BangEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Bang, self.scan_location))
                }
            }
            '=' => {
                if self.conditional_advance('=') {
                    Some(Token::new(TokenType::EqualEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Equal, self.scan_location))
                }
            }
            '<' => {
                if self.conditional_advance('=') {
                    Some(Token::new(TokenType::LessEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Less, self.scan_location))
                }
            }
            '>' => {
                if self.conditional_advance('=') {
                    Some(Token::new(TokenType::GreaterEqual, self.scan_location))
                } else {
                    Some(Token::new(TokenType::Greater, self.scan_location))
                }
            }
            '/' => {
                if self.conditional_advance('/') {
                    // A comment, we need to advance until the end of line
                    while !self.conditional_advance('\n') {
                        self.lexeme_current_index += 1;
                    }
                    self.scan_location.increment_line(1);
                    None
                } else {
                    Some(Token::new(TokenType::Slash, self.scan_location))
                }
            }
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.scan_location.increment_line(1);
                println!("incremented to: {:?}", self.scan_location);
                None
            }
            c => panic!("Unrecognized character: {}", c),
        };

        Ok(token)
    }

    fn advance(&mut self) -> Result<char> {
        if self.end_reached() {
            return Err(ScannerError::EndOfSource);
        } else {
            let c = self
                .source
                .chars()
                .nth(self.lexeme_current_index)
                .expect("`lexeme_current_index` should never be out of bound");

            self.lexeme_current_index += 1;
            Ok(c)
        }
    }

    /// Only advances if the `expected` char matches
    fn conditional_advance(&mut self, expected: char) -> bool {
        if self.end_reached() {
            return false;
        }
        let c = self
            .source
            .chars()
            .nth(self.lexeme_current_index)
            .expect("`lexeme_current_index` should never be out of bound");

        if c == expected {
            self.lexeme_current_index += 1;
            true
        } else {
            false
        }
    }

    fn end_reached(&self) -> bool {
        self.lexeme_current_index >= self.source.len()
    }
}

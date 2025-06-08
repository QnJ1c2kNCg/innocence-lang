use std::mem::discriminant;

use crate::scanner::ScannerError;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Identifier(String);

impl Identifier {
    pub(crate) fn new(id: String) -> Self {
        Self(id)
    }

    // TODO: This should be cleaned, we are using an empty
    // identifier as a way to match any identifier. What we should
    // use is something like looking at the discriminant
    pub(crate) fn any() -> Self {
        Self("".to_owned())
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self::new(value.to_owned())
    }
}

#[derive(Clone, Debug)]
pub(crate) enum TokenType {
    // Single char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(Identifier),
    String(String),
    Number(f64),

    // Keywords
    And,
    Or,
    Struct,
    If,
    Else,
    For,
    While,
    False,
    True,
    Fn,
    Nil,
    Print,
    Return,
    This,
    Let,

    Eof,
}

// XXX: Maybe this is dangerous? probably
impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
}

impl TryFrom<&str> for TokenType {
    type Error = ScannerError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "and" => Ok(TokenType::And),
            "or" => Ok(TokenType::Or),
            "struct" => Ok(TokenType::Struct),
            "if" => Ok(TokenType::If),
            "else" => Ok(TokenType::Else),
            "for" => Ok(TokenType::For),
            "while" => Ok(TokenType::While),
            "true" => Ok(TokenType::True),
            "false" => Ok(TokenType::False),
            "fn" => Ok(TokenType::Fn),
            "nil" => Ok(TokenType::Nil),
            "print" => Ok(TokenType::Print),
            "return" => Ok(TokenType::Return),
            "this" => Ok(TokenType::This),
            "let" => Ok(TokenType::Let),
            _ => Err(ScannerError::NotAKeyword),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct SourceLocation {
    line_number: usize,
}

impl SourceLocation {
    pub(crate) fn new() -> Self {
        Self { line_number: 1 }
    }

    pub(crate) fn increment_line(&mut self, amount: usize) {
        self.line_number += amount;
    }
}

impl From<usize> for SourceLocation {
    fn from(value: usize) -> Self {
        Self { line_number: value }
    }
}

#[derive(PartialEq, Clone)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    location: SourceLocation,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.location, self.token_type)
    }
}

impl Token {
    pub(crate) fn new(token_type: TokenType, location: SourceLocation) -> Self {
        Self {
            token_type,
            location,
        }
    }

    pub(crate) fn lexeme(&self) -> String {
        match &self.token_type {
            TokenType::LeftParen => "(".to_owned(),
            TokenType::RightParen => ")".to_owned(),
            TokenType::LeftBrace => "{".to_owned(),
            TokenType::RightBrace => "}".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::Dot => todo!(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Plus => "+".to_owned(),
            TokenType::Semicolon => ";".to_owned(),
            TokenType::Slash => "/".to_owned(),
            TokenType::Star => "*".to_owned(),
            TokenType::Bang => "!".to_owned(),
            TokenType::BangEqual => "!=".to_owned(),
            TokenType::Equal => "=".to_owned(),
            TokenType::EqualEqual => "==".to_owned(),
            TokenType::Greater => ">".to_owned(),
            TokenType::GreaterEqual => ">=".to_owned(),
            TokenType::Less => "<".to_owned(),
            TokenType::LessEqual => "<=".to_owned(),
            TokenType::Identifier(id) => id.0.clone(),
            TokenType::String(str) => str.clone(),
            TokenType::Number(number) => number.to_string(),
            TokenType::And => todo!(),
            TokenType::Or => todo!(),
            TokenType::Struct => todo!(),
            TokenType::If => todo!(),
            TokenType::Else => todo!(),
            TokenType::For => todo!(),
            TokenType::While => todo!(),
            TokenType::False => "false".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::Fn => todo!(),
            TokenType::Nil => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::This => todo!(),
            TokenType::Let => todo!(),
            TokenType::Eof => todo!(),
        }
    }
}

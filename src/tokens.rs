use crate::scanner::ScannerError;

#[derive(Debug, PartialEq)]
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
    Identifier(String),
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
pub(crate) struct Location {
    line_number: usize,
}

impl Location {
    pub(crate) fn new() -> Self {
        Self { line_number: 1 }
    }

    pub(crate) fn increment_line(&mut self, amount: usize) {
        self.line_number += amount;
    }
}

impl From<usize> for Location {
    fn from(value: usize) -> Self {
        Self { line_number: value }
    }
}

#[derive(PartialEq)]
pub(crate) struct Token {
    token_type: TokenType,
    location: Location,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.location, self.token_type)
    }
}

impl Token {
    pub(crate) fn new(token_type: TokenType, location: Location) -> Self {
        Self {
            token_type,
            location,
        }
    }

    pub(crate) fn lexeme(&self) -> String {
        match self.token_type {
            TokenType::LeftParen => todo!(),
            TokenType::RightParen => todo!(),
            TokenType::LeftBrace => todo!(),
            TokenType::RightBrace => todo!(),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Plus => todo!(),
            TokenType::Semicolon => todo!(),
            TokenType::Slash => todo!(),
            TokenType::Star => "*".to_owned(),
            TokenType::Bang => todo!(),
            TokenType::BangEqual => todo!(),
            TokenType::Equal => todo!(),
            TokenType::EqualEqual => todo!(),
            TokenType::Greater => todo!(),
            TokenType::GreaterEqual => todo!(),
            TokenType::Less => todo!(),
            TokenType::LessEqual => todo!(),
            TokenType::Identifier(_) => todo!(),
            TokenType::String(_) => todo!(),
            TokenType::Number(number) => number.to_string(),
            TokenType::And => todo!(),
            TokenType::Or => todo!(),
            TokenType::Struct => todo!(),
            TokenType::If => todo!(),
            TokenType::Else => todo!(),
            TokenType::For => todo!(),
            TokenType::While => todo!(),
            TokenType::False => todo!(),
            TokenType::True => todo!(),
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

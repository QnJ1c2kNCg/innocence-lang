#[derive(Debug)]
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
    Identifier,
    String(String),
    Number,

    // Keywords
    And,
    Or,
    Struct,
    If,
    Else,
    False,
    True,
    Fn,
    Nil,
    Print,
    Return,
    Super,
    This,
    Let,
    While,

    Eof,
}

#[derive(Copy, Clone, Debug)]
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
}

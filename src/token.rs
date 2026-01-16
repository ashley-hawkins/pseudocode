use std::fmt::Display;

use crate::lexer::LexerError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketKind {
    Round,
    Square,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketDirection {
    Open,
    Close,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Error(LexerError),

    Debug,
    DebugLn,
    DebugStack,
    StringLiteral(&'src str),
    Identifier(&'src str),

    // Operators

    //   Comparison
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    //   Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    //   Logic
    And,
    Or,
    Not,
    //   Misc
    Assign,
    Swap,

    LRoundBracket,
    RRoundBracket,
    LSquareBracket,
    RSquareBracket,

    // Separators
    Colon,
    Comma,

    // Literals
    NumberLiteral(&'src str),
    BoolLiteral(bool),

    Procedure,
    Algorithm,
    If,
    Then,
    Else,
    // for ... to ... do
    For,
    To,
    Do,
    While,
    Goto,
    Line,
    Return,

    Assert,
    In,
    Is,
    Strictly,
    Ascending,
    Descending,

    Indent,
    Dedent,
    Newline,

    UnexpectedCharacter(char),
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Debug => write!(f, "DEBUG"),
            Token::DebugLn => write!(f, "DEBUGLN"),
            Token::DebugStack => write!(f, "DEBUGSTACK"),
            Token::Error(e) => write!(f, "Error({})", e),
            Token::StringLiteral(s) => write!(f, "<string literal: \"{}\">", s),
            Token::Identifier(s) => write!(f, "{:?}", s),
            Token::Lt => write!(f, "<"),
            Token::Lte => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::Gte => write!(f, ">="),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
            Token::Add => write!(f, "+"),
            Token::Subtract => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Assign => write!(f, "<-"),
            Token::Swap => write!(f, "<=>"),
            Token::LRoundBracket => write!(f, "("),
            Token::RRoundBracket => write!(f, ")"),
            Token::LSquareBracket => write!(f, "["),
            Token::RSquareBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::NumberLiteral(s) => write!(f, "{}", s),
            Token::BoolLiteral(b) => write!(f, "{b}"),
            Token::Procedure => write!(f, "Procedure"),
            Token::Algorithm => write!(f, "Algorithm"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::To => write!(f, "to"),
            Token::Do => write!(f, "do"),
            Token::While => write!(f, "while"),
            Token::Goto => write!(f, "goto"),
            Token::Line => write!(f, "line"),
            Token::Return => write!(f, "return"),
            Token::Assert => write!(f, "assert"),
            Token::In => write!(f, "in"),
            Token::Is => write!(f, "is"),
            Token::Strictly => write!(f, "strictly"),
            Token::Ascending => write!(f, "ascending"),
            Token::Descending => write!(f, "descending"),
            Token::Indent => write!(f, "<indent>"),
            Token::Dedent => write!(f, "<dedent>"),
            Token::Newline => write!(f, "<newline>"),
            Token::UnexpectedCharacter(c) => {
                write!(f, "<unexpected character caught by lexer: '{c}'>")
            }
        }
    }
}

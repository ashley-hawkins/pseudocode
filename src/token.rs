use std::{fmt::Display, ops::Range};

use crate::lexer::{self, IndentationChange, LexerError, NewlineMetadata};

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
            Token::Error(e) => write!(f, "Error({})", e),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::SourceLocation;

    #[test]
    fn test_offset_to_source_location() {
        let line_offsets = vec![10, 20, 30];
        let loc = SourceLocation::new_from_bytes(15, &line_offsets);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_offset_to_source_location_beginning() {
        let line_offsets = vec![10, 20, 30];
        let loc = SourceLocation::new_from_bytes(5, &line_offsets);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_offset_to_source_location_0() {
        let line_offsets = vec![10, 20, 30];
        let loc = SourceLocation::new_from_bytes(0, &line_offsets);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 0);
    }
}

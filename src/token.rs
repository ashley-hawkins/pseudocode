use std::ops::Range;

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

    UnexpectedCharacter,
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

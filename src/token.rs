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

// tag structs

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ZeroIndexed;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OneIndexed;

trait IndexingType {}

impl IndexingType for ZeroIndexed {}
impl IndexingType for OneIndexed {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(private_bounds)]
pub struct SourceLocation<Indexing: IndexingType = ZeroIndexed> {
    pub bytes: usize,
    pub line: usize,
    pub column: usize,
    _marker: std::marker::PhantomData<Indexing>,
}

#[allow(private_bounds)]
impl<T: IndexingType> SourceLocation<T> {
    pub fn new(bytes: usize, line: usize, column: usize) -> Self {
        SourceLocation {
            bytes,
            line,
            column,
            _marker: std::marker::PhantomData,
        }
    }
}

impl SourceLocation<ZeroIndexed> {
    pub fn to_one_indexed(&self) -> SourceLocation<OneIndexed> {
        SourceLocation {
            bytes: self.bytes,
            line: self.line + 1,
            column: self.column + 1,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn new_from_bytes(bytes: usize, line_offsets: &[usize]) -> Self {
        offset_to_source_location(bytes, line_offsets)
    }
}

impl SourceLocation<OneIndexed> {
    pub fn to_zero_indexed(&self) -> SourceLocation<ZeroIndexed> {
        SourceLocation {
            bytes: self.bytes,
            line: self.line - 1,
            column: self.column - 1,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn new_from_bytes(bytes: usize, line_offsets: &[usize]) -> Self {
        let zero_indexed = offset_to_source_location(bytes, line_offsets);
        zero_indexed.to_one_indexed()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceSpan {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        SourceSpan { start, end }
    }

    pub fn new_from_range(range: Range<usize>, line_offsets: &[usize]) -> Self {
        SourceSpan {
            start: offset_to_source_location(range.start, line_offsets),
            end: offset_to_source_location(range.end, line_offsets),
        }
    }
}

// #[derive(Clone, PartialEq, Debug)]
// pub struct FatToken<'src> {
//     pub token: Token<'src>,
//     pub span: SourceSpan,
// }

fn offset_to_source_location(offset: usize, line_offsets: &[usize]) -> SourceLocation {
    let mut line = line_offsets.len();

    // Example: line_offsets = [10, 20] (0 is implicit since the first line always starts at 0)
    // Output line number is 0-indexed
    // offset = 15
    // First iteration: line = 2, offset > 20? No.
    // Second iteration: line = 1, offset > 10? Yes. Stop. So line = 1
    while line != 0 && offset < line_offsets[line - 1] {
        line -= 1;
    }

    SourceLocation {
        bytes: offset,
        // So this would be 1
        line,
        // And this would be 15 - 10 = 5
        column: offset - line.checked_sub(1).map(|i| line_offsets[i]).unwrap_or(0),

        _marker: std::marker::PhantomData,
    }
}

pub fn source_byte_range_to_source_span(
    token_range: Range<usize>,
    line_offsets: &[usize],
) -> SourceSpan {
    SourceSpan {
        start: offset_to_source_location(token_range.start, line_offsets),
        end: offset_to_source_location(token_range.end, line_offsets),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_offset_to_source_location() {
        let line_offsets = vec![10, 20, 30];
        let loc = offset_to_source_location(15, &line_offsets);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_offset_to_source_location_beginning() {
        let line_offsets = vec![10, 20, 30];
        let loc = offset_to_source_location(5, &line_offsets);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_offset_to_source_location_0() {
        let line_offsets = vec![10, 20, 30];
        let loc = offset_to_source_location(0, &line_offsets);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 0);
    }
}

use std::ops::Range;

use crate::lexer::{self, IndentationChange, LexerError, NewlineMetadata};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    // Comparison
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    // Logic
    And,
    Or,
    Not,
    // Misc
    Assign,
    Swap,
}

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
    Op(Operator),

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
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
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

pub fn into_fat_tokens<'src>(
    source_token: Result<lexer::Token<'src>, lexer::LexerError>,
    source_location: Range<usize>,
    line_offsets: &[usize],
) -> Result<
    ((Token<'src>, SourceSpan), Vec<(Token<'src>, SourceSpan)>),
    (lexer::LexerError, SourceSpan),
> {
    let start_loc = offset_to_source_location(source_location.start, line_offsets);
    let end_loc = offset_to_source_location(source_location.end, line_offsets);

    match source_token {
        Err(e) => Err((
            e,
            SourceSpan {
                start: start_loc,
                end: end_loc,
            },
        )),
        Ok(source_token) => Ok((
            (
                match source_token {
                    lexer::Token::RoundL => Token::LRoundBracket,
                    lexer::Token::RoundR => Token::RRoundBracket,
                    lexer::Token::SquareL => Token::LSquareBracket,
                    lexer::Token::SquareR => Token::RSquareBracket,
                    lexer::Token::Colon => Token::Colon,
                    lexer::Token::Comma => Token::Comma,
                    lexer::Token::Assign => Token::Op(Operator::Assign),
                    lexer::Token::Swap => Token::Op(Operator::Swap),
                    lexer::Token::Lt => Token::Op(Operator::Lt),
                    lexer::Token::Gt => Token::Op(Operator::Gt),
                    lexer::Token::Lte => Token::Op(Operator::Lte),
                    lexer::Token::Gte => Token::Op(Operator::Gte),
                    lexer::Token::Eq => Token::Op(Operator::Eq),
                    lexer::Token::Neq => Token::Op(Operator::Neq),
                    lexer::Token::Add => Token::Op(Operator::Add),
                    lexer::Token::Subtract => Token::Op(Operator::Subtract),
                    lexer::Token::Multiply => Token::Op(Operator::Multiply),
                    lexer::Token::Divide => Token::Op(Operator::Divide),
                    lexer::Token::And => Token::Op(Operator::And),
                    lexer::Token::Or => Token::Op(Operator::Or),
                    lexer::Token::Not => Token::Op(Operator::Not),
                    lexer::Token::Assert => Token::Assert,
                    lexer::Token::Is => Token::Is,
                    lexer::Token::In => Token::In,
                    lexer::Token::Strictly => Token::Strictly,
                    lexer::Token::Ascending => Token::Ascending,
                    lexer::Token::Descending => Token::Descending,
                    lexer::Token::Procedure => Token::Procedure,
                    lexer::Token::For => Token::For,
                    lexer::Token::To => Token::To,
                    lexer::Token::Do => Token::Do,
                    lexer::Token::While => Token::While,
                    lexer::Token::If => Token::If,
                    lexer::Token::Then => Token::Then,
                    lexer::Token::Else => Token::Else,
                    lexer::Token::Goto => Token::Goto,
                    lexer::Token::Line => Token::Line,
                    lexer::Token::Return => Token::Return,
                    lexer::Token::BoolTrue => Token::BoolLiteral(true),
                    lexer::Token::BoolFalse => Token::BoolLiteral(false),
                    lexer::Token::NumberLiteral(n) => Token::NumberLiteral(n),
                    lexer::Token::Identifier(s) => Token::Identifier(s),
                    lexer::Token::Newline(newline_metadata) => {
                        let NewlineMetadata {
                            indentation_change,
                            newline_range,
                        } = newline_metadata;

                        let newline_begin_loc =
                            offset_to_source_location(newline_range.start, line_offsets);
                        let newline_end_loc =
                            offset_to_source_location(newline_range.end, line_offsets);

                        let additional_tokens = match indentation_change {
                            Some(IndentationChange::Indent) => vec![(
                                Token::Indent,
                                SourceSpan {
                                    start: newline_end_loc,
                                    end: end_loc,
                                },
                            )],
                            Some(IndentationChange::Dedent(amount)) => (0..amount)
                                .map(|_| {
                                    (
                                        Token::Dedent,
                                        SourceSpan {
                                            start: newline_end_loc,
                                            end: end_loc,
                                        },
                                    )
                                })
                                .collect(),
                            None => vec![],
                        };

                        return Ok((
                            (
                                Token::Newline,
                                SourceSpan {
                                    start: newline_begin_loc,
                                    end: newline_end_loc,
                                },
                            ),
                            additional_tokens,
                        ));
                    }
                    lexer::Token::UnexpectedCharacter => Token::UnexpectedCharacter,
                },
                SourceSpan {
                    start: start_loc,
                    end: end_loc,
                },
            ),
            Vec::new(),
        )),
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

use std::{collections::VecDeque, ops::Range};

use logos::{
    FilterResult::{self, *},
    Logos,
};

use crate::epic_token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IndentationCharacter {
    Space,
    Tab,
}

#[derive(Debug, Default)]
pub struct LexerState {
    indentation_character: Option<IndentationCharacter>,
    indent_stack: Vec<usize>,
    line_offsets: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndentationChange {
    Indent,
    Dedent(usize),
}

#[derive(thiserror::Error, Debug, PartialEq, Eq, Clone)]
enum TokenValidationError {
    #[error("There is a non-indent character in the indentation portion of an indented line.")]
    NonIndentCharacterInIndentation(Range<usize>),
    #[error("Inconsistent use of indentation characters.")]
    InconsistentIndentation(Range<usize>),
    #[error("Unexpected dedent, does not match any previous indent level.")]
    UnexpectedDedent(Range<usize>),
}

#[derive(thiserror::Error, Debug, PartialEq, Eq, Clone, Default)]
pub enum LexerError {
    #[error("Unknown lexer error")]
    #[default]
    Unknown,
    #[error("Logical error in token: ")]
    TokenValidationError(#[from] TokenValidationError),
}

#[derive(Debug)]
pub enum BracketKind {
    Round,
    Square,
}

#[derive(Debug)]
enum BracketDirection {
    Open,
    Close,
}

#[derive(Debug)]
pub struct BracketToken {
    pub kind: BracketKind,
    pub direction: BracketDirection,
}

#[derive(Debug, Clone)]
pub struct NewlineMetadata {
    pub indentation_change: Option<IndentationChange>,
    // range from first newline to last newline
    pub newline_range: Range<usize>,
}

#[derive(Debug, Logos)]
#[logos(extras = LexerState)]
#[logos(error = LexerError)]
#[logos(subpattern single_whitespace = r"[ \t\n]")]
#[logos(subpattern block_comment = r"/\*([^*]|\*+[^*/])*\*+/")]
#[logos(subpattern line_comment = r"//[^\n]*")]
#[logos(skip(r"(?&block_comment)|(?&line_comment)", priority = 0))]
pub enum Token<'a> {
    // Single-character tokens
    #[token("(")]
    RoundL,
    #[token(")")]
    RoundR,
    #[token("[")]
    SquareL,
    #[token("]")]
    SquareR,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("<-")]
    Assign,
    #[token("<->")]
    Swap,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("=")]
    Eq,
    #[token("/=")]
    Neq,
    #[token("+")]
    Add,
    #[token("-")]
    Subtract,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    // Keywords
    //   Assertions
    #[token("assert")]
    Assert,
    #[token("is")]
    Is,
    #[token("in")]
    In,
    #[token("strictly")]
    Strictly,
    #[token("ascending")]
    Ascending,
    #[token("descending")]
    Descending,
    //   Control flow
    #[regex("[Pp]rocedure")]
    Procedure,
    #[token("for")]
    For,
    #[token("to")]
    To,
    #[token("do")]
    Do,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("goto")]
    Goto,
    #[token("line")]
    Line,
    #[token("return")]
    Return,
    //   Literals
    #[token("true")]
    BoolTrue,
    #[token("false")]
    BoolFalse,
    // More complex tokens
    #[regex(r"[0-9]+(\.[0-9]+)?")]
    NumberLiteral(&'a str),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'a str),
    // Matches newlines and following indentation. Can also match indentation without a preceding newline at the start of the file.
    #[regex(
        r"((?&single_whitespace)|(?&block_comment)|(?&line_comment))+",
        handle_whitespace
    )]
    Newline(NewlineMetadata),

    #[regex(".", priority = 0)]
    UnexpectedCharacter,
}

fn handle_whitespace<'src>(
    lex: &mut logos::Lexer<'src, Token<'src>>,
) -> FilterResult<NewlineMetadata, LexerError> {
    let span = lex.span();
    let slice = lex.slice();
    // println!("Handling whitespace: {:?}", slice);

    let mut first_newline_offset = None;
    let mut last_newline_offset = None;
    for (offset, ch) in slice.char_indices() {
        if ch == '\n' {
            // println!("Found newline at offset: {}. A span constructed from here would look like: {:?}", offset, &slice[offset+1..]);
            lex.extras.line_offsets.push(span.start + offset + 1);
            last_newline_offset = Some(offset);
            if first_newline_offset.is_none() {
                first_newline_offset = Some(offset);
            }
        }
    }

    if span.start != 0 && first_newline_offset.is_none() {
        return Skip;
    }

    let first_newline = first_newline_offset.unwrap_or(0);
    let last_newline = last_newline_offset.unwrap_or(0);

    let indentation_slice = &slice[last_newline.wrapping_add(1)..];
    let indentation_len = indentation_slice.len();

    let state = &mut lex.extras;
    if indentation_len != 0 && state.indentation_character.is_none() {
        let first_char = indentation_slice.chars().next().unwrap();
        state.indentation_character = match first_char {
            ' ' => Some(IndentationCharacter::Space),
            '\t' => Some(IndentationCharacter::Tab),
            _ => {
                // println!(
                //     "Non-indent character in indentation: {} found at index: {} in indentation span {:?}",
                //     first_char, 0, indentation_slice
                // );

                return Error(TokenValidationError::NonIndentCharacterInIndentation(
                    (span.start + last_newline + 1)..(span.start + last_newline + 2),
                ).into());
            }
        };
    }

    for (idx, ch) in indentation_slice.char_indices() {
        match (ch, state.indentation_character) {
            (' ', Some(IndentationCharacter::Space)) => {}
            ('\t', Some(IndentationCharacter::Tab)) => {}
            (' ', Some(IndentationCharacter::Tab)) | ('\t', Some(IndentationCharacter::Space)) => {
                return Error(TokenValidationError::InconsistentIndentation(
                    (span.start + last_newline + 1 + idx)..(span.start + last_newline + 2 + idx),
                ).into());
            }
            _ => {
                // println!(
                //     "Non-indent character in indentation: {} found at index: {} in indentation span {:?}",
                //     ch, idx, indentation_slice
                // );
                return Error(TokenValidationError::NonIndentCharacterInIndentation(
                    (span.start + last_newline + 1 + idx)..(span.start + last_newline + 2 + idx),
                ).into());
            }
        }
    }

    let newline_range = (span.start + first_newline)..(span.start + last_newline + 1);

    match indentation_len.cmp(&state.indent_stack.last().copied().unwrap_or(0)) {
        std::cmp::Ordering::Less => {
            let mut dedent_count = 0;
            while let Some(&top) = state.indent_stack.last() {
                if indentation_len < top {
                    state.indent_stack.pop();
                    dedent_count += 1;
                } else {
                    break;
                }
            }
            if state.indent_stack.last().copied().unwrap_or(0) != indentation_len {
                return Error(TokenValidationError::UnexpectedDedent(
                    newline_range,
                ).into());
            }
            Emit(NewlineMetadata {
                indentation_change: Some(IndentationChange::Dedent(dedent_count)),
                newline_range,
            })
        }
        std::cmp::Ordering::Equal => Emit(NewlineMetadata {
            indentation_change: None,
            newline_range,
        }),
        std::cmp::Ordering::Greater => {
            state.indent_stack.push(indentation_len);
            Emit(NewlineMetadata {
                indentation_change: Some(IndentationChange::Indent),
                newline_range,
            })
        }
    }
}

pub fn lex_str<'src>(source: &'src str) -> impl Iterator<Item = (Result<epic_token::Token, LexerError>, epic_token::SourceSpan)> {
    let mut lexer = Token::lexer_with_extras(source, LexerState::default());

    std::iter::from_fn({
        let mut once_flag = true;
        let mut failed = false;
        let mut queue: VecDeque<(epic_token::Token, epic_token::SourceSpan)> = VecDeque::new();
        move || -> Option<(Result<epic_token::Token, LexerError>, epic_token::SourceSpan)> {
            if failed {
                return None;
            }

            if let Some((token, span)) = queue.pop_front() {
                return Some((Ok(token), span));
            }

            let mut res = lexer.next()?;

            if once_flag {
                once_flag = false;
                if matches!(
                    res,
                    Ok(Token::Newline(NewlineMetadata {
                        indentation_change: None,
                        ..
                    }))
                ) {
                    res = lexer.next()?;
                }
            }
            
            let res = epic_token::into_fat_tokens(
                        res,
                        lexer.span(),
                        &lexer.extras.line_offsets,
                    );

            match res {
                Err((e, span)) => {
                    failed = true;
                    Some((Err(e), span))
                }
                Ok(((main_token, main_token_span), additional_tokens)) => {
                    
                    queue.extend(additional_tokens);

                    Some((Ok(main_token), main_token_span))
                }
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::epic_token::{OneIndexed, ZeroIndexed};

    use super::*;
    #[test]
    fn test_lexer_basic() {
        let source = r#"//  input: an integer, n ≥ 1
// output: true if n is even, false otherwise
currentVal <- n
currentVal <- currentVal - 2
if currentVal = 0 then
    return true
else
    if currentVal < 0 then
        return false
    else
        goto line 4"#;

        let tokens: Vec<_> = lex_str(source)
            .map(|(res, span)| (res.expect("Lexer error"), span))
            .collect();

        // eprintln!("{:#?}", &tokens[..3]);

        assert_eq!(
            tokens[0],
            (
                epic_token::Token::Identifier("currentVal"),
                epic_token::SourceSpan {
                    start: epic_token::SourceLocation::<OneIndexed>::new(77, 3, 1).to_zero_indexed(),
                    end: epic_token::SourceLocation::<OneIndexed>::new(87, 3, 11).to_zero_indexed(),
                }
            )
        );
    }
}

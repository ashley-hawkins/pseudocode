use std::{
    collections::VecDeque,
    io::{BufRead, BufReader, Seek},
};

enum Operator {
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
    // Misc
    Assign,
    Swap,
}

enum BracketKind {
    Round,
    Square,
}

enum BracketDirection {
    Open,
    Close,
}

enum Token {
    Identifier(String),
    Op(Operator),
    Bracket {
        kind: BracketKind,
        direction: BracketDirection,
    },
    NumberLiteral(f64),
    BoolLiteral(bool),
    If,
    Then,
    Else,
    Goto,
    Line,
    Return,

    Indent,
    Dedent,
    NewLine,
}

struct FatToken {
    token: Token,
}

type TokenResult = Result<Option<FatToken>, LexerError>;

#[derive(thiserror::Error, Debug)]
enum LexerError {
    #[error("Unexpected indentation level")]
    UnexpectedIndentation,
    #[error("End of file reached")]
    Eof,
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

enum LexerState {
    NewLine,
    ProcessingDedent { new_indent_level: u32 },
    Normal,
}

struct Lexer<'a> {
    source: &'a str,
    offset: usize,
    
    indent_stack: Vec<u32>,
    state: LexerState,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            offset: 0,
            indent_stack: vec![0],
            state: LexerState::NewLine,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.source.as_bytes().get(self.offset).copied()
    }

    fn ident_start(&mut self) -> Result<Option<u8>, LexerError> {
        let buf = self.reader.fill_buf()?;

        let Some(&first_char) = buf.get(0) else {
            return Err(LexerError::Eof);
        };

        drop(buf);

        if first_char.is_ascii_alphabetic() || first_char == b'_' {
            self.reader.consume(1);
            Ok(Some(first_char))
        } else {
            Ok(None)
        }
    }


    fn try_identifier(&mut self) -> TokenResult {
        let mut ident = String::new();

        let first_char = self.ident_start()?;


    }

    fn ignore_whitespace(&mut self) -> u32 {
        let mut count = 0;
        loop {
            let mut buf = [0u8];
            if self.reader.read_exact(&mut buf).is_err() || buf[0] != b' ' {
                break;
            }
            count += 1;
        }
        count
    }

    fn process_dedent(&mut self, new_indent_level: u32) -> TokenResult {
        let current_indent_level = *self.indent_stack.last().unwrap();
        if new_indent_level < current_indent_level {
            self.indent_stack.pop();
            Ok(Some(FatToken {
                token: Token::Dedent,
            }))
        } else if new_indent_level == current_indent_level {
            self.state = LexerState::Normal;
            Ok(None)
        } else {
            return Err(LexerError::UnexpectedIndentation);
        }
    }

    fn process(&mut self) -> TokenResult {
        match self.state {
            LexerState::NewLine => {
                let indent_level = self.ignore_whitespace();
                let current_indent_level = *self.indent_stack.last().unwrap();
                if indent_level > current_indent_level {
                    self.indent_stack.push(indent_level);
                    self.state = LexerState::Normal;
                    Ok(Some(FatToken {
                        token: Token::Indent,
                    }))
                } else if indent_level < current_indent_level {
                    self.state = LexerState::ProcessingDedent {
                        new_indent_level: indent_level,
                    };
                    self.process_dedent(indent_level)
                } else {
                    self.state = LexerState::Normal;
                    Ok(None)
                }
            }
            LexerState::ProcessingDedent { new_indent_level } => {
                self.process_dedent(new_indent_level)
            }
            LexerState::Normal => todo!(),
        }
    }

    fn next_token(&mut self) -> TokenResult {
        loop {
            match self.process() {
                Ok(None) => continue,
                Ok(Some(token)) => return Ok(Some(token)),
                Err(LexerError::Eof) => return Ok(None),
                Err(e) => return Err(e),
            }
        }
    }
}

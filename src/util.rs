// tag structs

use std::ops::Range;

use chumsky::span::{Span, WrappingSpan};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ZeroIndexed;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OneIndexed;

trait IndexingType {}

impl IndexingType for ZeroIndexed {}
impl IndexingType for OneIndexed {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    pub fn is_eof(&self) -> bool {
        self.bytes == usize::MAX
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

    pub fn eof() -> Self {
        SourceLocation {
            bytes: usize::MAX,
            line: 0,
            column: 0,
            _marker: std::marker::PhantomData,
        }
    }
}

impl Default for SourceLocation<ZeroIndexed> {
    fn default() -> Self {
        SourceLocation {
            bytes: 0,
            line: 0,
            column: 0,
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl std::fmt::Debug for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}..{}:{}",
            self.start.line + 1,
            self.start.column + 1,
            self.end.line + 1,
            self.end.column + 1
        )
    }
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

    pub fn eof() -> Self {
        SourceSpan {
            start: SourceLocation::eof(),
            end: SourceLocation::eof(),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.start.is_eof() && self.end.is_eof()
    }
}

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

impl Span for SourceSpan {
    type Context = ();

    type Offset = SourceLocation;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub type Spanned<T> = chumsky::span::Spanned<T, SourceSpan>;

impl<T> WrappingSpan<T> for SourceSpan {
    type Spanned = Spanned<T>;

    fn make_wrapped(self, inner: T) -> Self::Spanned {
        Spanned { span: self, inner }
    }

    fn inner_of(spanned: &Self::Spanned) -> &T {
        &spanned.inner
    }

    fn span_of(spanned: &Self::Spanned) -> &Self {
        &spanned.span
    }
}

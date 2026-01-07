use std::{
    fmt::{Debug, Display},
    ops::Range,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ParserRange {
    pub start: usize,
    pub end: usize,
}

impl Default for ParserRange {
    fn default() -> Self {
        Self { start: 0, end: 1 }
    }
}

impl Display for ParserRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Debug for ParserRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range<usize>> for ParserRange {
    fn from(range: Range<usize>) -> ParserRange {
        ParserRange {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<ParserRange> for Range<usize> {
    fn from(range: ParserRange) -> Range<usize> {
        range.start..range.end
    }
}

impl ParserRange {
    pub fn into_range(self) -> Range<usize> {
        self.into()
    }

    pub fn overlaps(&self, other: ParserRange) -> bool {
        self.start < other.end && other.start < self.end
    }

    pub fn contains(&self, position: usize) -> bool {
        position >= self.start && position <= self.end
    }
}

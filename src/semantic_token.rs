use crate::ParserRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[repr(u8)]
pub enum SemanticTokenKind {
    Namespace,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Type,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Decorator,
    Event,
    Function,
    Method,
    Macro,
    Comment,
    String,
    Keyword,
    Number,
    RegularExpression,
    Operator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SemanticToken {
    pub range: ParserRange,
    pub kind: SemanticTokenKind,
}

impl SemanticToken {
    pub fn length(&self) -> usize {
        self.range.end - self.range.start
    }
}

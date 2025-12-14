use crate::accumulate::Accumulate;
use std::fmt::{Debug, Display};
use std::mem::take;
use std::ops::Range;

pub mod accumulate;

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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Default)]
pub struct ParserRange {
    pub start: usize,
    pub end: usize,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Expectation {
    Literal(&'static str),
    Char(char),
    Identifier,
    Digit,
    Whitespace,
    NewlineWhitespace,
    Custom(&'static str),
    StartOfFile,
    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub span: ParserRange,
    pub messages: Vec<&'static str>,
    pub expected: Vec<Expectation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Suggestion {
    pub range: ParserRange,
    pub expected: Expectation,
}

#[derive(Debug, Clone)]
pub struct ValidationError {
    pub span: ParserRange,
    pub message: &'static str,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStateFull {
    position: usize,
    suggestions_len: usize,
    validation_errors_len: usize,
    signatures_len: usize,
    can_suggest_at_position: bool,
    force_suggest_range: Option<ParserRange>,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStatePartial {
    position: usize,
    can_suggest_at_position: bool,
    force_suggest_range: Option<ParserRange>,
    signatures_len: usize,
    signatures_depth: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStateChoice {
    suggestions_len: usize,
    validation_errors_len: usize,
    signatures_len: usize,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Signature {
    pub label: &'static str,
    pub parameters: &'static [(&'static str, Option<&'static str>)],
    pub documentation: Option<&'static str>,
    pub active_parameter: Option<usize>,
}

#[derive(Debug)]
pub struct Stream<'a> {
    pub input: &'a str,
    pub bytes: &'a [u8],
    pub position: usize,
    pub cursor: Option<usize>,
    pub max_error: ParseError,
    pub suggestions: Vec<Suggestion>,
    pub validation_errors: Vec<ValidationError>,
    pub can_suggest_at_position: bool,
    pub force_suggest_range: Option<ParserRange>,
    pub semantic_tokens: Vec<SemanticToken>,
    pub semantic_tokens_range: Option<ParserRange>,
    pub semantic_tokens_enabled: bool,
    pub max_validation_errors: usize,
    pub validation_errors_enabled: bool,
    pub signature_help_enabled: bool,
    pub active_parameter: Option<Option<usize>>,
    pub signatures: Vec<Signature>,
    pub signatures_depth: usize,
}

impl<'a> Stream<'a> {
    pub fn new(
        input: &'a str,
        completion_target: Option<usize>,
        max_validation_errors: Option<usize>,
    ) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            position: 0,
            cursor: completion_target,
            max_error: ParseError {
                span: ParserRange { start: 0, end: 1 },
                messages: Vec::new(),
                expected: Vec::new(),
            },
            suggestions: Vec::new(),
            validation_errors: Vec::new(),
            can_suggest_at_position: true,
            force_suggest_range: None,
            max_validation_errors: max_validation_errors.unwrap_or(0),
            validation_errors_enabled: max_validation_errors.is_some(),
            semantic_tokens_enabled: false,
            semantic_tokens: Vec::new(),
            semantic_tokens_range: None,
            signature_help_enabled: false,
            signatures: Vec::new(),
            active_parameter: None,
            signatures_depth: 0,
        }
    }

    pub fn add_syntax_from(&mut self, start: usize, kind: SemanticTokenKind) {
        if !self.semantic_tokens_enabled {
            return;
        }

        let token_range = ParserRange {
            start,
            end: self.position,
        };

        let should_add = match &self.semantic_tokens_range {
            Some(request_range) => token_range.overlaps(*request_range),
            None => true,
        };

        if should_add {
            self.semantic_tokens.push(SemanticToken {
                range: token_range,
                kind,
            });
        }
    }

    #[inline]
    #[must_use]
    pub fn save_choice(&self) -> StreamStateChoice {
        StreamStateChoice {
            suggestions_len: self.suggestions.len(),
            validation_errors_len: self.validation_errors.len(),
            signatures_len: self.signatures.len(),
        }
    }

    pub fn resolve_choice_success(
        &mut self,
        start_state: StreamStateChoice,
        pre_branch_state: StreamStateChoice,
    ) {
        if pre_branch_state.suggestions_len > start_state.suggestions_len {
            self.suggestions
                .drain(start_state.suggestions_len..pre_branch_state.suggestions_len);
        }

        if pre_branch_state.validation_errors_len > start_state.validation_errors_len {
            self.validation_errors
                .drain(start_state.validation_errors_len..pre_branch_state.validation_errors_len);
        }

        if pre_branch_state.signatures_len > start_state.signatures_len {
            self.signatures
                .drain(start_state.signatures_len..pre_branch_state.signatures_len);
        }
    }

    #[inline]
    #[must_use]
    pub fn save_full(&self) -> StreamStateFull {
        StreamStateFull {
            position: self.position,
            suggestions_len: self.suggestions.len(),
            validation_errors_len: self.validation_errors.len(),
            signatures_len: self.signatures.len(),
            can_suggest_at_position: self.can_suggest_at_position,
            force_suggest_range: self.force_suggest_range,
        }
    }

    #[inline]
    pub fn restore_full(&mut self, state: StreamStateFull) {
        self.position = state.position;
        self.suggestions.truncate(state.suggestions_len);
        self.validation_errors.truncate(state.validation_errors_len);
        self.signatures.truncate(state.signatures_len);
        self.can_suggest_at_position = state.can_suggest_at_position;
        self.force_suggest_range = state.force_suggest_range;
    }

    #[inline]
    #[must_use]
    pub fn save_partial(&self) -> StreamStatePartial {
        StreamStatePartial {
            position: self.position,
            can_suggest_at_position: self.can_suggest_at_position,
            force_suggest_range: self.force_suggest_range,
            signatures_len: self.signatures.len(),
            signatures_depth: self.signatures_depth,
        }
    }

    #[inline]
    pub fn restore_partial(&mut self, state: StreamStatePartial) {
        self.position = state.position;
        self.can_suggest_at_position = state.can_suggest_at_position;
        self.force_suggest_range = state.force_suggest_range;
        self.signatures.truncate(state.signatures_len);
        self.signatures_depth = state.signatures_depth;
    }

    pub fn reset(&mut self) {
        self.position = 0;
    }

    #[inline]
    #[must_use]
    pub fn remaining(&self) -> &str {
        &self.input[self.position..]
    }

    #[inline]
    #[must_use]
    pub fn remaining_bytes(&self) -> &[u8] {
        &self.bytes[self.position..]
    }

    #[inline]
    #[must_use]
    pub fn current_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    #[inline]
    pub fn consume_char(&mut self) -> Option<char> {
        let c = self.remaining().chars().next()?;
        self.position += c.len_utf8();
        Some(c)
    }

    #[inline]
    pub fn allow_suggestions(&mut self, start: usize, end: usize) {
        self.force_suggest_range = Some(ParserRange { start, end });
        self.can_suggest_at_position = true;
    }

    #[inline]
    pub fn disallow_suggestions(&mut self) {
        self.force_suggest_range = None;
        self.can_suggest_at_position = false;
    }

    #[inline]
    pub fn add_suggestion(&mut self, start_position: usize, expectation: &Expectation) {
        self.add_suggestion_range(
            ParserRange {
                start: start_position,
                end: start_position + 1,
            },
            expectation,
        );
    }

    pub fn add_suggestion_range<R: Into<ParserRange>>(
        &mut self,
        range: R,
        expectation: &Expectation,
    ) {
        let range = range.into();

        if self.can_suggest_at_position
            && let Some(cursor) = self.cursor
        {
            if let Some(force_suggest_range) = self.force_suggest_range.take()
                && cursor >= force_suggest_range.start
                && cursor <= force_suggest_range.end
            {
                self.suggestions.push(Suggestion {
                    range: ParserRange {
                        start: cursor,
                        end: cursor + (range.end - range.start),
                    },
                    expected: *expectation,
                });

                return;
            }

            if cursor >= range.start && cursor < range.end {
                self.suggestions.push(Suggestion {
                    range,
                    expected: *expectation,
                });
            }
        }
    }

    pub fn add_suggestions(&mut self, start_position: usize, expectations: &Vec<Expectation>) {
        for expectation in expectations {
            self.add_suggestion(start_position, expectation);
        }
    }

    #[must_use]
    pub fn fail_expected<T>(&mut self, expectation: &Expectation) -> Option<T> {
        let _ = self.fail::<T>();
        self.max_error.expected.push(*expectation);

        None
    }

    #[must_use]
    pub fn fail<T>(&mut self) -> Option<T> {
        if self.position > self.max_error.span.start {
            self.max_error.span = ParserRange {
                start: self.position,
                end: self.position + 1,
            };
            self.max_error.messages.clear();
            self.max_error.expected.clear();
        }

        None
    }

    #[must_use]
    pub fn fail_expected_suggestion<T>(&mut self, expected: &Expectation) -> Option<T> {
        self.add_suggestion(self.position, expected);

        self.fail_expected(expected)
    }

    pub fn fail_message<T>(&mut self, message: &'static str) -> Option<T> {
        if self.position > self.max_error.span.start {
            self.max_error.span = ParserRange {
                start: self.position,
                end: self.position + 1,
            };
            self.max_error.expected.clear();

            self.max_error.messages.clear();
            self.max_error.messages.push(message);
        } else if self.position == self.max_error.span.start {
            self.max_error.messages.push(message);
        }

        None
    }

    pub fn fail_expected_multiple<T>(&mut self, expectations: &Vec<Expectation>) -> Option<T> {
        for expectation in expectations {
            let _ = self.fail_expected::<T>(expectation);
        }

        None
    }

    #[inline]
    #[must_use]
    pub fn slice(&self, start: usize, end: usize) -> &'a str {
        &self.input[start..end]
    }

    #[inline]
    #[must_use]
    pub fn slice_from(&self, start: usize) -> &'a str {
        self.slice(start, self.position)
    }

    #[inline]
    pub fn add_validation_error(&mut self, message: &'static str) {
        self.add_validation_error_position(self.position, message);
    }

    #[inline]
    pub fn add_validation_error_position(&mut self, position: usize, message: &'static str) {
        self.add_validation_error_span(
            ParserRange {
                start: position,
                end: position + 1,
            },
            message,
        );
    }

    #[inline]
    pub fn add_validation_error_span<R: Into<ParserRange>>(
        &mut self,
        span: R,
        message: &'static str,
    ) {
        if self.validation_errors_enabled
            && self.validation_errors.len() < self.max_validation_errors
        {
            self.validation_errors.push(ValidationError {
                span: span.into(),
                message,
            });
        }
    }

    #[inline]
    pub fn add_validation_error_fn<F, R>(&mut self, span: R, callback: F)
    where
        F: FnOnce() -> &'static str,
        R: Into<ParserRange>,
    {
        if self.validation_errors_enabled
            && self.validation_errors.len() < self.max_validation_errors
        {
            self.validation_errors.push(ValidationError {
                span: span.into(),
                message: callback(),
            });
        }
    }

    #[inline]
    pub fn suggest(&mut self, start_position: usize, expectation: &Expectation) {
        self.suggest_range(
            ParserRange {
                start: start_position,
                end: start_position + 1,
            },
            expectation,
        )
    }

    #[inline]
    pub fn suggest_literal(&mut self, start: usize, literal: &'static str) {
        self.add_suggestion_range(start..start + literal.len(), &Expectation::Literal(literal));
    }

    #[inline]
    pub fn suggest_range<R: Into<ParserRange>>(&mut self, range: R, expectation: &Expectation) {
        self.add_suggestion_range(range, expectation);
    }
}

pub trait FnParser<'a, T>
where
    Self: Sized,
{
    fn signatures(
        mut self,
        signatures: &'static [(&'static str, &'static [(&'static str, Option<&'static str>)], Option<&'static str>)],
    ) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            if !input.signature_help_enabled {
                return self.parse(input);
            }

            let Some(cursor) = input.cursor else {
                return self.parse(input);
            };

            if cursor < input.position {
                return self.parse(input);
            }

            let old_signatures = take(&mut input.signatures);

            let old_active_parameter = input.active_parameter;

            for (label, parameters, documentation) in signatures.iter().copied() {
                input.signatures.push(Signature {
                    label,
                    parameters,
                    documentation,
                    active_parameter: None,
                });
            }

            input.signatures_depth += 1;

            let result = self.parse(input);

            if cursor > input.position {
                input.signatures = old_signatures;
                input.signatures_depth -= 1;
            }

            input.active_parameter = old_active_parameter;

            result
        }
    }

    fn signature(mut self, commit_signature: usize) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            if !input.signature_help_enabled {
                return self.parse(input);
            }

            let Some(cursor) = input.cursor else {
                return self.parse(input);
            };

            if cursor < input.position {
                return self.parse(input);
            }

            let mut signature = input.signatures.get(commit_signature).copied();

            let old_active_parameter = input.active_parameter;
            input.active_parameter = Some(None);

            let start = input.position;
            let old_signatures_depth = input.signatures_depth;

            let result = self.parse(input);

            if let Some(signature) = &mut signature
                && let Some(active_parameter) = input.active_parameter
            {
                signature.active_parameter = active_parameter;
            }

            let no_new_signatures_created = input.signatures_depth == old_signatures_depth;

            if no_new_signatures_created {
                let advanced = input.position != start;

                if advanced {
                    input.signatures.clear();

                    if let Some(signature) = signature {
                        input.signatures.push(signature);
                    }
                } else if let Some(signature) = input.signatures.get_mut(commit_signature)
                    && let Some(active_parameter) = input.active_parameter
                {
                    signature.active_parameter = active_parameter;

                    input.active_parameter = old_active_parameter;
                }
            }

            result
        }
    }

    fn next_signature_parameter(mut self) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            if !input.signature_help_enabled {
                return self.parse(input);
            }
            let Some(cursor) = input.cursor else {
                return self.parse(input);
            };

            let should_force = if let Some(range) = input.force_suggest_range {
                cursor >= range.start && cursor <= range.end
            } else {
                false
            };

            let start = input.position;

            let old_signatures_depth = input.signatures_depth;

            let result = self.parse(input);

            let no_new_signatures_created = input.signatures_depth == old_signatures_depth;

            if (should_force || cursor >= start)
                && no_new_signatures_created
                && let Some(active_parameter) = input.active_parameter
            {
                let new_active_parameter = active_parameter.map(|p| p + 1).unwrap_or(0);

                input.active_parameter = Some(Some(new_active_parameter));
            }

            result
        }
    }

    fn optional(mut self) -> impl FnParser<'a, Option<T>> {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();

            match self.parse(input) {
                Some(val) => Some(Some(val)),
                None => {
                    if input.position != partial.position {
                        None
                    } else {
                        input.restore_partial(partial);

                        Some(None)
                    }
                }
            }
        }
    }

    fn dont_suggest_if(&mut self, shouldnt_suggest: bool) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            if shouldnt_suggest {
                let can_suggest_at_position = input.can_suggest_at_position;
                input.can_suggest_at_position = false;

                let result = self.parse(input);

                input.can_suggest_at_position = can_suggest_at_position;

                result
            } else {
                self.parse(input)
            }
        }
    }

    fn attempt(mut self) -> impl FnParser<'a, Option<T>> {
        move |input: &mut Stream<'a>| {
            let start = input.position;
            let suggestions_len = input.suggestions.len();

            match self.parse(input) {
                Some(val) => Some(Some(val)),
                None => {
                    input.position = start;
                    input.suggestions.truncate(suggestions_len);

                    Some(None)
                }
            }
        }
    }

    fn sliced(mut self) -> impl FnParser<'a, &'a str> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            self.parse(input).map(|_| input.slice_from(start))
        }
    }

    fn spanned(mut self) -> impl FnParser<'a, (ParserRange, T)> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            match self.parse(input) {
                Some(val) => Some((
                    ParserRange {
                        start,
                        end: input.position,
                    },
                    val,
                )),
                None => None,
            }
        }
    }

    fn many<C>(mut self) -> impl FnParser<'a, C>
    where
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();

            loop {
                let start = input.position;
                match self.parse(input) {
                    Some(val) => {
                        if input.position == start {
                            break;
                        }

                        collection.accumulate(val);
                    }
                    None => {
                        if input.position != start {
                            return None;
                        }

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn many_one<C>(mut self) -> impl FnParser<'a, C>
    where
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let first = self.parse(input)?;

            let mut collection = C::default();
            collection.accumulate(first);

            loop {
                let start = input.position;

                match self.parse(input) {
                    Some(val) => {
                        if input.position == start {
                            break;
                        }

                        collection.accumulate(val);
                    }
                    None => {
                        if input.position != start {
                            return None;
                        } else {
                            break;
                        }
                    }
                }
            }

            Some(collection)
        }
    }

    fn many_range<C>(mut self, min: usize, max: usize) -> impl FnParser<'a, C>
    where
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection: Option<C> = None;
            let mut count = 0;

            for _ in 0..min {
                let item = self.parse(input)?;

                if collection.is_none() {
                    collection = Some(C::default());
                }

                collection.as_mut().unwrap().accumulate(item);
                count += 1;

                if count >= max {
                    break;
                }
            }

            if collection.is_none() {
                collection = Some(C::default());
            }

            loop {
                if count >= max {
                    break;
                }

                let start = input.position;
                match self.parse(input) {
                    Some(val) => {
                        if input.position == start {
                            break;
                        }
                        collection.as_mut().unwrap().accumulate(val);
                        count += 1;
                    }
                    None => {
                        if input.position != start {
                            return None;
                        } else {
                            break;
                        }
                    }
                }
            }

            Some(collection.unwrap())
        }
    }

    fn remap(mut self, expected: Expectation) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            let start_position = input.position;

            let pre_parse_error_len = if input.max_error.span.start == start_position {
                input.max_error.expected.len()
            } else {
                0
            };

            match self.parse(input) {
                Some(val) => {
                    if input.position == start_position {
                        input.add_suggestion(input.position, &expected);
                    }
                    Some(val)
                }
                None => {
                    if input.position == start_position {
                        input.max_error.expected.truncate(pre_parse_error_len);
                        input.max_error.expected.push(expected);
                    }
                    None
                }
            }
        }
    }

    fn remap_multiple(mut self, expectations: Vec<Expectation>) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            let start_position = input.position;

            let pre_parse_error_len = if input.max_error.span.start == start_position {
                input.max_error.expected.len()
            } else {
                0
            };

            match self.parse(input) {
                Some(val) => {
                    if input.position == start_position {
                        input.add_suggestions(input.position, &expectations);
                    }
                    Some(val)
                }
                None => {
                    if input.position == start_position {
                        input.max_error.expected.truncate(pre_parse_error_len);
                        input
                            .max_error
                            .expected
                            .extend(expectations.iter().copied());
                    }
                    None
                }
            }
        }
    }

    fn separated_by<S, U, C>(mut self, mut separator: S) -> impl FnParser<'a, C>
    where
        S: FnParser<'a, U>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();

            let (advanced, first) = match self.parse(input) {
                Some(first) => (input.position != partial.position, first),
                None => {
                    if input.position != partial.position {
                        return None;
                    }

                    input.restore_partial(partial);

                    return Some(C::default());
                }
            };

            let mut collection = C::default();
            collection.accumulate(first);

            if !advanced {
                return Some(collection);
            }

            loop {
                let separator_partial = input.save_partial();

                match separator.parse(input) {
                    Some(_) => {
                        let partial = input.save_partial();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);

                                if input.position == partial.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != partial.position {
                                    return None;
                                }

                                input.restore_partial(separator_partial);
                                break;
                            }
                        }
                    }
                    None => {
                        if input.position != separator_partial.position {
                            return None;
                        }

                        input.restore_partial(separator_partial);

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_trailing<S, U, C>(mut self, mut separator: S) -> impl FnParser<'a, C>
    where
        S: FnParser<'a, U>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();

            let (advanced, first) = match self.parse(input) {
                Some(first) => (input.position != partial.position, first),
                None => {
                    if input.position != partial.position {
                        return None;
                    }

                    input.restore_partial(partial);

                    return Some(C::default());
                }
            };

            let mut collection = C::default();
            collection.accumulate(first);

            if !advanced {
                return Some(collection);
            }

            loop {
                let partial = input.save_partial();

                match separator.parse(input) {
                    Some(_) => {
                        if input.position == partial.position {
                            break;
                        }

                        let partial = input.save_partial();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);

                                if input.position == partial.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != partial.position {
                                    return None;
                                }

                                input.restore_partial(partial);

                                break;
                            }
                        }
                    }
                    None => {
                        if input.position != partial.position {
                            return None;
                        }

                        input.restore_partial(partial);

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_one<S, U, C>(mut self, mut separator: S) -> impl FnParser<'a, C>
    where
        S: FnParser<'a, U>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();

            let start = input.position;
            let first = self.parse(input)?;
            let advanced = input.position != start;

            collection.accumulate(first);

            if !advanced {
                return Some(collection);
            }

            loop {
                let partial = input.save_partial();
                match separator.parse(input) {
                    Some(_) => {
                        if input.position == partial.position {
                            break;
                        }

                        let partial = input.save_partial();
                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);
                                if input.position == partial.position {
                                    break;
                                }
                            }
                            None => {
                                return None;
                            }
                        }
                    }
                    None => {
                        if input.position != partial.position {
                            return None;
                        }
                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_range<S, U, C>(
        mut self,
        min: usize,
        max: usize,
        mut separator: S,
    ) -> impl FnParser<'a, C>
    where
        S: FnParser<'a, U>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();
            let mut count = 0;

            let partial = input.save_partial();

            match self.parse(input) {
                Some(first) => {
                    let advanced = input.position != partial.position;
                    collection.accumulate(first);
                    count += 1;

                    if !advanced {
                        if count < min {
                            return None;
                        } else {
                            return Some(collection);
                        }
                    }
                }
                None => {
                    if min > 0 {
                        return None;
                    } else {
                        return Some(collection);
                    }
                }
            }

            loop {
                if count >= max {
                    break;
                }

                let partial = input.save_partial();
                match separator.parse(input) {
                    Some(_) => {
                        if input.position == partial.position {
                            break;
                        }

                        let partial = input.save_partial();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);
                                count += 1;

                                if input.position == partial.position {
                                    break;
                                }
                            }
                            None => {
                                return None;
                            }
                        }
                    }
                    None => {
                        if count >= min && input.position == partial.position {
                            break;
                        } else {
                            return None;
                        }
                    }
                }
            }

            Some(collection)
        }
    }

    #[inline]
    fn label(self, label: &'static str) -> impl FnParser<'a, T> {
        self.remap(Expectation::Custom(label))
    }

    #[must_use]
    fn syntax(mut self, kind: SemanticTokenKind) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            match self.parse(input) {
                Some(value) => {
                    if !input.semantic_tokens_enabled {
                        return Some(value);
                    }

                    let token_range = ParserRange {
                        start,
                        end: input.position,
                    };

                    let should_add = match &input.semantic_tokens_range {
                        Some(request_range) => token_range.overlaps(*request_range),
                        None => true,
                    };

                    if should_add {
                        input.semantic_tokens.push(SemanticToken {
                            range: token_range,
                            kind,
                        });
                    }

                    Some(value)
                }
                None => None,
            }
        }
    }

    #[inline]
    #[must_use]
    fn syntax_keyword(self) -> impl FnParser<'a, T> {
        self.syntax(SemanticTokenKind::Keyword)
    }

    fn on_error(mut self, f: impl Fn(&mut Stream<'a>)) -> impl FnParser<'a, T> {
        move |input: &mut Stream<'a>| match self.parse(input) {
            Some(value) => Some(value),
            None => {
                f(input);

                None
            }
        }
    }

    fn map<U>(mut self, f: impl Fn(T) -> U) -> impl FnParser<'a, U> {
        move |input: &mut Stream<'a>| self.parse(input).map(&f)
    }

    fn map_to<U: Clone>(mut self, value: U) -> impl FnParser<'a, U> {
        move |input: &mut Stream<'a>| self.parse(input).map(|_| value.clone())
    }

    fn parse(&mut self, input: &mut Stream<'a>) -> Option<T>;
}

impl<'a, T, F> FnParser<'a, T> for F
where
    F: FnMut(&mut Stream<'a>) -> Option<T>,
{
    fn parse(&mut self, input: &mut Stream<'a>) -> Option<T> {
        self(input)
    }
}

pub fn literal<'a>(literal: &'static str) -> impl FnParser<'a, &'a str> {
    move |input: &mut Stream<'a>| {
        if input.remaining_bytes().starts_with(literal.as_bytes()) {
            input.position += literal.len();
            Some(literal)
        } else {
            let expectation = Expectation::Literal(literal);
            input.add_suggestion_range(
                ParserRange {
                    start: input.position,
                    end: input.position + literal.len(),
                },
                &expectation,
            );

            input.fail_expected(&expectation)
        }
    }
}

pub fn char<'a>(expected_char: char) -> impl FnParser<'a, char> {
    move |input: &mut Stream<'a>| {
        if let Some(&char) = input.bytes.get(input.position) {
            if expected_char.is_ascii() {
                if char == expected_char as u8 {
                    input.position += 1;

                    return Some(expected_char);
                }
            } else {
                let mut buf = [0; 4];
                let encoded = expected_char.encode_utf8(&mut buf);
                if input
                    .bytes
                    .get(input.position..)
                    .is_some_and(|s| s.starts_with(encoded.as_bytes()))
                {
                    input.position += encoded.len();
                    return Some(expected_char);
                }
            }
        }

        input.fail_expected_suggestion(&Expectation::Char(expected_char))
    }
}

pub fn take_while<'a, F>(predicate: F) -> impl FnParser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let s = &input.input[start..];
        let mut len = 0;

        for c in s.chars() {
            if !predicate(c) {
                break;
            }
            len += c.len_utf8();
        }

        input.position += len;

        Some(&input.input[start..input.position])
    }
}

pub fn take_while_bytes<'a, F>(predicate: F) -> impl FnParser<'a, &'a str>
where
    F: Fn(u8) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let bytes = &input.bytes[start..];

        let len = bytes
            .iter()
            .position(|&b| !predicate(b))
            .unwrap_or(bytes.len());

        input.position += len;

        Some(&input.input[start..input.position])
    }
}

pub fn take_while_one<'a, F>(predicate: F, expected: Expectation) -> impl FnParser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let s = &input.input[start..];
        let mut len = 0;

        for c in s.chars() {
            if !predicate(c) {
                break;
            }
            len += c.len_utf8();
        }

        if len == 0 {
            input.fail_expected_suggestion(&expected)
        } else {
            input.position += len;
            Some(&input.input[start..input.position])
        }
    }
}

pub fn take_while_one_bytes<'a, F>(
    predicate: F,
    expected: Expectation,
) -> impl FnParser<'a, &'a str>
where
    F: Fn(u8) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let bytes = &input.bytes[start..];

        let len = bytes
            .iter()
            .position(|&b| !predicate(b))
            .unwrap_or(bytes.len());

        if len == 0 {
            input.fail_expected_suggestion(&expected)
        } else {
            input.position += len;
            Some(&input.input[start..input.position])
        }
    }
}

pub fn take_while_range<'a, F>(
    predicate: F,
    min: usize,
    max: usize,
    expected: Expectation,
) -> impl FnParser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let s = &input.input[start..];

        let mut byte_len = 0;
        let mut char_count = 0;

        for c in s.chars() {
            if char_count >= max || !predicate(c) {
                break;
            }
            char_count += 1;
            byte_len += c.len_utf8();
        }

        if char_count < min {
            input.fail_expected_suggestion(&expected)
        } else {
            input.position += byte_len;
            Some(&input.input[start..input.position])
        }
    }
}

pub fn take_while_range_bytes<'a, F>(
    predicate: F,
    min: usize,
    max: usize,
    expected: Expectation,
) -> impl FnParser<'a, &'a str>
where
    F: Fn(u8) -> bool,
{
    move |input: &mut Stream<'a>| {
        let start = input.position;
        let bytes = &input.bytes[start..];

        let mut len = 0;
        for &b in bytes.iter().take(max) {
            if !predicate(b) {
                break;
            }
            len += 1;
        }

        if len < min {
            input.fail_expected_suggestion(&expected)
        } else {
            input.position += len;
            Some(&input.input[start..input.position])
        }
    }
}

pub fn digits<'a>(input: &mut Stream<'a>) -> Option<&'a str> {
    let result = take_while_bytes(|b| b.is_ascii_digit()).parse(input)?;

    if result.is_empty() {
        input.fail_expected(&Expectation::Digit)
    } else {
        Some(result)
    }
}

pub fn end_of_file(input: &mut Stream) -> Option<()> {
    if input.position == input.input.len() {
        Some(())
    } else {
        input.fail_expected(&Expectation::EndOfFile)
    }
}

pub fn fail<'a, T>(message: &'static str) -> impl FnParser<'a, T> {
    move |input: &mut Stream<'a>| input.fail_message(message)
}

macro_rules! impl_choice_tuple {
    ($($idx:tt : $name:ident),*) => {
        impl<'a, T, $($name),*> FnParser<'a, T> for ($($name,)*)
        where
            $($name: FnParser<'a, T>),*
        {
            fn parse(&mut self, input: &mut Stream<'a>) -> Option<T> {
                let choice_start = input.save_choice();
                let partial = input.save_partial();

                $(
                    let branch_start = input.save_choice();

                    match self.$idx.parse(input) {
                        Some(res) => {
                            input.resolve_choice_success(choice_start, branch_start);

                            return Some(res);
                        },
                        None => {
                            if input.position != partial.position {
                                input.suggestions.drain(
                                    choice_start.suggestions_len..branch_start.suggestions_len
                                );
                                input.validation_errors.drain(
                                    choice_start.validation_errors_len..branch_start.validation_errors_len
                                );

                                return None;
                            }

                            input.restore_partial(partial);
                        }
                    }
                )*

                None
            }
        }
    };
}

impl_choice_tuple!(0: P0);
impl_choice_tuple!(0: P0, 1: P1);
impl_choice_tuple!(0: P0, 1: P1, 2: P2);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20, 21: P21);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20, 21: P21, 22: P22);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20, 21: P21, 22: P22, 23: P23);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20, 21: P21, 22: P22, 23: P23, 24: P24);
impl_choice_tuple!(0: P0, 1: P1, 2: P2, 3: P3, 4: P4, 5: P5, 6: P6, 7: P7, 8: P8, 9: P9, 10: P10, 11: P11, 12: P12, 13: P13, 14: P14, 15: P15, 16: P16, 17: P17, 18: P18, 19: P19, 20: P20, 21: P21, 22: P22, 23: P23, 24: P24, 25: P25);

#[inline(always)]
#[must_use]
pub fn choice<'a, T, P>(parser: P) -> impl FnParser<'a, T>
where
    P: FnParser<'a, T>,
{
    parser
}

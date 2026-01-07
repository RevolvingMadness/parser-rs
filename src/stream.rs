use crate::{
    Checkpoint, Expectation, MaxParseError, ParserRange, Signature, StreamStateFull,
    StreamStatePartial, Suggestion, ValidationError,
    semantic_token::{SemanticToken, SemanticTokenKind},
};

#[derive(Debug, Clone, Copy, Default)]
pub struct StreamConfig {
    pub semantic_tokens: bool,
    pub max_validation_errors: usize,
    pub signatures: bool,
}

#[derive(Debug)]
pub struct Stream<'a> {
    pub input: &'a str,
    pub bytes: &'a [u8],
    pub position: usize,
    pub cursor: Option<usize>,
    pub max_error: MaxParseError,
    pub suggestions: Vec<Suggestion>,
    pub validation_errors: Vec<ValidationError>,
    pub can_suggest_at_position: bool,
    pub force_suggest_range: Option<ParserRange>,
    pub semantic_tokens: Vec<SemanticToken>,
    pub semantic_tokens_range: Option<ParserRange>,
    pub active_parameter: Option<Option<usize>>,
    pub signatures: Vec<Signature>,
    pub signatures_depth: usize,
    pub config: StreamConfig,
}

impl<'a> Stream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            position: 0,
            cursor: None,
            max_error: MaxParseError::default(),
            suggestions: Vec::new(),
            validation_errors: Vec::new(),
            can_suggest_at_position: true,
            force_suggest_range: None,
            semantic_tokens: Vec::new(),
            semantic_tokens_range: None,
            signatures: Vec::new(),
            active_parameter: None,
            signatures_depth: 0,
            config: StreamConfig::default(),
        }
    }

    #[inline(always)]
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            position: self.position,
            suggestions_len: self.suggestions.len(),
            validation_errors_len: self.validation_errors.len(),
            signatures_len: self.signatures.len(),
            semantic_tokens_len: self.semantic_tokens.len(),
            signatures_depth: self.signatures_depth,
            can_suggest_at_position: self.can_suggest_at_position,
            force_suggest_range: self.force_suggest_range,
        }
    }

    #[inline(always)]
    pub fn partial_rollback(&mut self, checkpoint: Checkpoint) {
        self.position = checkpoint.position;

        self.validation_errors
            .truncate(checkpoint.validation_errors_len);
        self.signatures.truncate(checkpoint.signatures_len);
        self.semantic_tokens
            .truncate(checkpoint.semantic_tokens_len);

        self.signatures_depth = checkpoint.signatures_depth;
        self.can_suggest_at_position = checkpoint.can_suggest_at_position;
        self.force_suggest_range = checkpoint.force_suggest_range;
    }

    #[inline(always)]
    pub fn full_rollback(&mut self, checkpoint: Checkpoint) {
        self.position = checkpoint.position;

        self.suggestions.truncate(checkpoint.suggestions_len);
        self.validation_errors
            .truncate(checkpoint.validation_errors_len);
        self.signatures.truncate(checkpoint.signatures_len);
        self.semantic_tokens
            .truncate(checkpoint.semantic_tokens_len);

        self.signatures_depth = checkpoint.signatures_depth;
        self.can_suggest_at_position = checkpoint.can_suggest_at_position;
        self.force_suggest_range = checkpoint.force_suggest_range;
    }

    pub fn add_syntax_from(&mut self, start: usize, kind: SemanticTokenKind) {
        if !self.config.semantic_tokens || self.position == start {
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
        if self.can_suggest_at_position
            && let Some(cursor) = self.cursor
        {
            let range = range.into();

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

        if self.position == self.max_error.span.start {
            self.max_error.expected.push(*expectation);
        }

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
            self.max_error.semantic_tokens = self.semantic_tokens.clone();
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
    pub fn add_validation_error<M: ToString>(&mut self, message: M) {
        self.add_validation_error_position(self.position, message);
    }

    #[inline]
    pub fn add_validation_error_position<M: ToString>(&mut self, position: usize, message: M) {
        self.add_validation_error_span(
            ParserRange {
                start: position,
                end: position + 1,
            },
            message,
        );
    }

    #[inline]
    pub fn add_validation_error_span<R: Into<ParserRange>, M: ToString>(
        &mut self,
        span: R,
        message: M,
    ) {
        if self.config.max_validation_errors != 0
            && self.validation_errors.len() < self.config.max_validation_errors
        {
            self.validation_errors.push(ValidationError {
                span: span.into(),
                message: message.to_string(),
            });
        }
    }

    #[inline]
    pub fn add_validation_error_fn<F, R, M>(&mut self, span: R, callback: F)
    where
        F: FnOnce() -> M,
        R: Into<ParserRange>,
        M: ToString,
    {
        if self.config.max_validation_errors != 0
            && self.validation_errors.len() < self.config.max_validation_errors
        {
            self.validation_errors.push(ValidationError {
                span: span.into(),
                message: callback().to_string(),
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

use std::mem::take;

use crate::{
    Expectation, ParseError, ParseResult, ParserRange, Signature,
    accumulate::Accumulate,
    combinators::end_of_file,
    semantic_token::{SemanticToken, SemanticTokenKind},
    stream::Stream,
};

pub mod separated_by;

pub trait FnParser<'a>
where
    Self: Sized,
{
    type Output;

    #[allow(clippy::type_complexity)]
    fn signatures(
        mut self,
        signatures: &'static [(
            &'static str,
            &'static [(&'static str, Option<&'static str>)],
            Option<&'static str>,
        )],
    ) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            if !input.config.signatures {
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

    fn signature(mut self, commit_signature: usize) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            if !input.config.signatures {
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

    fn next_signature_parameter(mut self) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            if !input.config.signatures {
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

    fn optional(mut self) -> impl FnParser<'a, Output = Option<Self::Output>> {
        move |input: &mut Stream<'a>| {
            let checkpoint = input.checkpoint();

            match self.parse(input) {
                Some(val) => Some(Some(val)),

                None => {
                    input.partial_rollback(checkpoint);
                    Some(None)
                }
            }
        }
    }

    fn dont_suggest_if(
        &mut self,
        shouldnt_suggest: bool,
    ) -> impl FnParser<'a, Output = Self::Output> {
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

    fn sliced(mut self) -> impl FnParser<'a, Output = &'a str> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            self.parse(input).map(|_| input.slice_from(start))
        }
    }

    fn sliced_include(mut self) -> impl FnParser<'a, Output = (&'a str, Self::Output)> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            self.parse(input)
                .map(|value| (input.slice_from(start), value))
        }
    }

    fn spanned(mut self) -> impl FnParser<'a, Output = (ParserRange, Self::Output)> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            self.parse(input).map(|value| {
                (
                    ParserRange {
                        start,
                        end: input.position,
                    },
                    value,
                )
            })
        }
    }

    fn many<C>(mut self) -> impl FnParser<'a, Output = C>
    where
        C: Default + Accumulate<Self::Output>,
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

    fn many_one<C>(mut self) -> impl FnParser<'a, Output = C>
    where
        C: Default + Accumulate<Self::Output>,
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

    fn many_range<C>(mut self, min: usize, max: usize) -> impl FnParser<'a, Output = C>
    where
        C: Default + Accumulate<Self::Output>,
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

    fn remap(mut self, expected: Expectation) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            let start_position = input.position;

            let pre_parse_error_len = if input.max_error.span.start == start_position {
                input.max_error.expected.len()
            } else {
                0
            };

            match self.parse(input) {
                Some(val) => Some(val),
                None => {
                    if input.position == start_position
                        && input.max_error.span.start == start_position
                    {
                        input.max_error.expected.truncate(pre_parse_error_len);
                        input.max_error.expected.push(expected);
                    }
                    None
                }
            }
        }
    }

    fn suggest(mut self, expected: Expectation) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            let start_position = input.position;

            match self.parse(input) {
                Some(val) => Some(val),
                None => {
                    if input.position == start_position
                        && input.max_error.span.start == start_position
                    {
                        input.max_error.expected.push(expected);
                    }

                    None
                }
            }
        }
    }

    fn remap_multiple(
        mut self,
        expectations: Vec<Expectation>,
    ) -> impl FnParser<'a, Output = Self::Output> {
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

    fn separated_by<Separator, SeparatorOutput, AccumulatedOutput>(
        mut self,
        mut separator: Separator,
    ) -> impl FnParser<'a, Output = AccumulatedOutput>
    where
        Separator: FnParser<'a, Output = SeparatorOutput>,
        AccumulatedOutput: Default + Accumulate<Self::Output>,
    {
        move |input: &mut Stream<'a>| {
            let start_checkpoint = input.checkpoint();

            let first = match self.parse(input) {
                Some(first) => first,
                None => {
                    if input.position != start_checkpoint.position {
                        return None;
                    }

                    let suggestions_len = start_checkpoint.suggestions_len;
                    let preserved_suggestions = if input.suggestions.len() > suggestions_len {
                        input.suggestions.split_off(suggestions_len)
                    } else {
                        Vec::new()
                    };

                    input.full_rollback(start_checkpoint);

                    input.suggestions.extend(preserved_suggestions);

                    return Some(AccumulatedOutput::default());
                }
            };

            let mut collection = AccumulatedOutput::default();
            collection.accumulate(first);

            if input.position == start_checkpoint.position {
                return Some(collection);
            }

            loop {
                let before_separator = input.checkpoint();

                match separator.parse(input) {
                    Some(_) => {
                        let before_item = input.checkpoint();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);

                                if input.position == before_item.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != before_item.position {
                                    return None;
                                }

                                input.full_rollback(before_separator);
                                break;
                            }
                        }
                    }
                    None => {
                        input.full_rollback(before_separator);

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_trailing<S, U, C>(mut self, mut separator: S) -> impl FnParser<'a, Output = C>
    where
        S: FnParser<'a, Output = U>,
        C: Default + Accumulate<Self::Output>,
    {
        move |input: &mut Stream<'a>| {
            let start_checkpoint = input.checkpoint();

            let first = match self.parse(input) {
                Some(first) => first,
                None => {
                    if input.position != start_checkpoint.position {
                        return None;
                    }

                    let suggestions_len = start_checkpoint.suggestions_len;
                    let preserved_suggestions = if input.suggestions.len() > suggestions_len {
                        input.suggestions.split_off(suggestions_len)
                    } else {
                        Vec::new()
                    };

                    input.full_rollback(start_checkpoint);
                    input.suggestions.extend(preserved_suggestions);

                    return Some(C::default());
                }
            };

            let mut collection = C::default();
            collection.accumulate(first);

            if input.position == start_checkpoint.position {
                return Some(collection);
            }

            loop {
                let before_separator = input.checkpoint();

                match separator.parse(input) {
                    Some(_) => {
                        if input.position == before_separator.position {
                            break;
                        }

                        let before_item = input.checkpoint();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);

                                if input.position == before_item.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != before_item.position {
                                    return None;
                                }

                                input.full_rollback(before_item);
                                break;
                            }
                        }
                    }
                    None => {
                        input.full_rollback(before_separator);

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_one<S, U, C>(mut self, mut separator: S) -> impl FnParser<'a, Output = C>
    where
        S: FnParser<'a, Output = U>,
        C: Default + Accumulate<Self::Output>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();

            let start_checkpoint = input.checkpoint();

            let first = self.parse(input)?;

            let advanced = input.position != start_checkpoint.position;
            collection.accumulate(first);

            if !advanced {
                return Some(collection);
            }

            loop {
                let before_separator = input.checkpoint();
                match separator.parse(input) {
                    Some(_) => {
                        if input.position == before_separator.position {
                            break;
                        }

                        let before_item = input.checkpoint();
                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);
                                if input.position == before_item.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != before_item.position {
                                    return None;
                                }
                                input.full_rollback(before_separator);
                                break;
                            }
                        }
                    }
                    None => {
                        input.full_rollback(before_separator);

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
    ) -> impl FnParser<'a, Output = C>
    where
        S: FnParser<'a>,
        C: Default + Accumulate<Self::Output>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();
            let mut count = 0;

            let start_checkpoint = input.checkpoint();

            match self.parse(input) {
                Some(first) => {
                    let advanced = input.position != start_checkpoint.position;
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
                        if input.position == start_checkpoint.position {
                            input.full_rollback(start_checkpoint);
                        }
                        return Some(collection);
                    }
                }
            }

            loop {
                if count >= max {
                    break;
                }

                let before_separator = input.checkpoint();
                match separator.parse(input) {
                    Some(_) => {
                        if input.position == before_separator.position {
                            break;
                        }

                        let before_item = input.checkpoint();

                        match self.parse(input) {
                            Some(next) => {
                                collection.accumulate(next);
                                count += 1;

                                if input.position == before_item.position {
                                    break;
                                }
                            }
                            None => {
                                if input.position != before_item.position {
                                    return None;
                                }
                                input.full_rollback(before_separator);
                                break;
                            }
                        }
                    }
                    None => {
                        input.full_rollback(before_separator);

                        if count < min {
                            return None;
                        }
                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    #[inline]
    fn label(self, label: &'static str) -> impl FnParser<'a, Output = Self::Output> {
        self.remap(Expectation::Custom(label))
    }

    #[must_use]
    fn syntax(mut self, kind: SemanticTokenKind) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            let result = self.parse(input);

            if result.is_none() || !input.config.semantic_tokens || input.position == start {
                return result;
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

            result
        }
    }

    #[inline]
    #[must_use]
    fn syntax_keyword(self) -> impl FnParser<'a, Output = Self::Output> {
        self.syntax(SemanticTokenKind::Keyword)
    }

    fn on_error(mut self, f: impl Fn(&mut Stream<'a>)) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| match self.parse(input) {
            Some(value) => Some(value),
            None => {
                f(input);

                None
            }
        }
    }

    fn map<U>(mut self, mut f: impl FnMut(Self::Output) -> U) -> impl FnParser<'a, Output = U> {
        move |input: &mut Stream<'a>| {
            let result = self.parse(input)?;

            Some(f(result))
        }
    }

    fn map_input<U>(
        mut self,
        mut f: impl FnMut(&mut Stream, Self::Output) -> U,
    ) -> impl FnParser<'a, Output = U> {
        move |input: &mut Stream<'a>| {
            let result = self.parse(input)?;

            Some(f(input, result))
        }
    }

    fn map_to<U: Clone>(mut self, value: U) -> impl FnParser<'a, Output = U> {
        move |input: &mut Stream<'a>| {
            self.parse(input)?;

            Some(value.clone())
        }
    }

    fn padded<U>(
        mut self,
        mut p: impl FnParser<'a, Output = Self::Output>,
    ) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            p.parse(input)?;
            let result = self.parse(input)?;
            p.parse(input)?;

            Some(result)
        }
    }

    fn peek(mut self) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();
            match self.parse(input) {
                Some(val) => {
                    input.restore_partial(partial);
                    Some(val)
                }
                None => {
                    input.restore_partial(partial);
                    None
                }
            }
        }
    }

    fn not_followed_by<U>(
        mut self,
        mut other: impl FnParser<'a, Output = U>,
    ) -> impl FnParser<'a, Output = Self::Output> {
        move |input: &mut Stream<'a>| {
            let start = input.position;
            let result = self.parse(input)?;

            let full = input.save_full();
            if other.parse(input).is_some() {
                input.restore_full(full);
                input.position = start;
                return None;
            }

            input.restore_full(full);

            Some(result)
        }
    }

    fn parse(&mut self, input: &mut Stream<'a>) -> Option<Self::Output>;

    fn parse_fully(&mut self, mut input: Stream<'a>) -> ParseResult<Self::Output> {
        let result = (|input: &mut Stream<'a>| {
            let result = self.parse(input)?;

            end_of_file(input)?;

            Some(result)
        })
        .parse(&mut input);

        let semantic_tokens = if result.is_none() {
            input.max_error.semantic_tokens
        } else {
            input.semantic_tokens
        };

        ParseResult {
            result: result.ok_or(ParseError {
                span: input.max_error.span,
                messages: input.max_error.messages,
                expected: input.max_error.expected,
            }),
            suggestions: input.suggestions,
            validation_errors: input.validation_errors,
            semantic_tokens,
            signatures: input.signatures,
        }
    }
}

impl<'a, T, F> FnParser<'a> for F
where
    F: FnMut(&mut Stream<'a>) -> Option<T>,
{
    type Output = T;

    fn parse(&mut self, input: &mut Stream<'a>) -> Option<Self::Output> {
        self(input)
    }
}

use crate::accumulate::Accumulate;
use std::fmt::Debug;
use std::ops::Range;

pub mod accumulate;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Default)]
pub struct ParserRange {
    start: usize,
    end: usize,
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
    pub span: Range<usize>,
    pub messages: Vec<&'static str>,
    pub expected: Vec<Expectation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Suggestion {
    pub range: Range<usize>,
    pub expected: Expectation,
}

#[derive(Debug, Clone)]
pub struct ValidationError {
    pub span: Range<usize>,
    pub message: &'static str,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStateFull {
    position: usize,
    suggestions_len: usize,
    validation_errors_len: usize,
    can_suggest_at_position: bool,
    force_suggest_range: Option<ParserRange>,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStatePartial {
    position: usize,
    can_suggest_at_position: bool,
    force_suggest_range: Option<ParserRange>,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStateChoice {
    suggestions_len: usize,
    validation_errors_len: usize,
}

#[derive(Debug, Clone)]
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
    pub max_validation_errors: usize,
    pub validation_errors_enabled: bool,
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
                span: 0..1,
                messages: vec![],
                expected: vec![],
            },
            suggestions: Vec::new(),
            validation_errors: Vec::new(),
            can_suggest_at_position: true,
            force_suggest_range: None,
            max_validation_errors: max_validation_errors.unwrap_or(0),
            validation_errors_enabled: max_validation_errors.is_some(),
        }
    }

    #[inline]
    #[must_use]
    pub fn save_choice(&self) -> StreamStateChoice {
        StreamStateChoice {
            suggestions_len: self.suggestions.len(),
            validation_errors_len: self.validation_errors.len(),
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
    }

    #[inline]
    #[must_use]
    pub fn save_full(&self) -> StreamStateFull {
        StreamStateFull {
            position: self.position,
            suggestions_len: self.suggestions.len(),
            validation_errors_len: self.validation_errors.len(),
            can_suggest_at_position: self.can_suggest_at_position,
            force_suggest_range: self.force_suggest_range,
        }
    }

    #[inline]
    pub fn restore_full(&mut self, state: StreamStateFull) {
        self.position = state.position;
        self.suggestions.truncate(state.suggestions_len);
        self.validation_errors.truncate(state.validation_errors_len);
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
        }
    }

    #[inline]
    pub fn restore_partial(&mut self, state: StreamStatePartial) {
        self.position = state.position;
        self.can_suggest_at_position = state.can_suggest_at_position;
        self.force_suggest_range = state.force_suggest_range;
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

    pub fn consume_char(&mut self) -> Option<char> {
        match self.current_char() {
            Some(c) => {
                let len = c.len_utf8();
                self.position += len;
                Some(c)
            }
            None => None,
        }
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
        self.add_suggestion_range(start_position..start_position + 1, expectation);
    }

    pub fn add_suggestion_range(&mut self, range: Range<usize>, expectation: &Expectation) {
        let error_position = range.start;
        if error_position > self.max_error.span.start {
            self.max_error.span = error_position..error_position + 1;
            self.max_error.messages.clear();
            self.max_error.expected.clear();
            self.max_error.expected.push(*expectation);
        } else if error_position == self.max_error.span.start {
            self.max_error.expected.push(*expectation);
        }

        if self.can_suggest_at_position
            && let Some(cursor) = self.cursor
        {
            if let Some(force_suggest_range) = self.force_suggest_range.take()
                && cursor >= force_suggest_range.start
                && cursor <= force_suggest_range.end
            {
                self.suggestions.push(Suggestion {
                    range: cursor..(cursor + (range.end - range.start)),
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

    pub fn fail_expected<T>(&mut self, expectation: &Expectation) -> Option<T> {
        self.add_suggestion(self.position, expectation);

        None
    }

    pub fn fail<T>(&mut self) -> Option<T> {
        if self.position > self.max_error.span.start {
            self.max_error.span = self.position..self.position + 1;
            self.max_error.messages.clear();
            self.max_error.expected.clear();
        }

        None
    }

    pub fn fail_expected_no_suggestion<T>(&mut self, expected: &Expectation) -> Option<T> {
        if self.position > self.max_error.span.start {
            self.max_error.span = self.position..self.position + 1;
            self.max_error.messages.clear();

            self.max_error.expected.clear();
            self.max_error.expected.push(*expected);
        } else if self.position == self.max_error.span.start {
            self.max_error.expected.push(*expected);
        }

        None
    }

    pub fn fail_message<T>(&mut self, message: &'static str) -> Option<T> {
        if self.position > self.max_error.span.start {
            self.max_error.span = self.position..self.position + 1;
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
        self.add_validation_error_span(position..position + 1, message);
    }

    #[inline]
    pub fn add_validation_error_span(&mut self, span: Range<usize>, message: &'static str) {
        self.add_validation_error_fn(|| ValidationError { span, message })
    }

    #[inline]
    pub fn add_validation_error_fn<F>(&mut self, callback: F)
    where
        F: FnOnce() -> ValidationError,
    {
        if self.validation_errors_enabled
            && self.validation_errors.len() < self.max_validation_errors
        {
            self.validation_errors.push(callback());
        }
    }

    #[inline]
    pub fn suggest(&mut self, start_position: usize, expectation: &Expectation) {
        self.suggest_range(start_position..start_position + 1, expectation)
    }

    #[inline]
    pub fn suggest_range(&mut self, range: Range<usize>, expectation: &Expectation) {
        self.add_suggestion_range(range, expectation);
    }
}

pub trait FnParser<'a, T>
where
    Self: Sized,
{
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

    fn spanned(mut self) -> impl FnParser<'a, (Range<usize>, T)> {
        move |input: &mut Stream<'a>| {
            let start = input.position;

            match self.parse(input) {
                Some(val) => Some((start..input.position, val)),
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

            let expected_len = input.max_error.expected.len();

            match self.parse(input) {
                Some(val) => {
                    if input.position == start_position {
                        input.add_suggestion(input.position, &expected);

                        input.max_error.expected.truncate(expected_len);
                        input.max_error.expected.push(expected);
                    }

                    Some(val)
                }
                None => {
                    if input.position == start_position {
                        input.max_error.expected.truncate(expected_len);
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

            let expected_len = input.max_error.expected.len();

            match self.parse(input) {
                Some(val) => {
                    if input.position == start_position {
                        input.add_suggestions(input.position, &expectations);

                        input.max_error.expected.truncate(expected_len);
                        input
                            .max_error
                            .expected
                            .extend(expectations.iter().copied());
                    }

                    Some(val)
                }
                None => {
                    if input.position == start_position {
                        input.max_error.expected.truncate(expected_len);
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

                        input.restore_partial(partial);

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

    fn label(self, label: &'static str) -> impl FnParser<'a, T> {
        self.remap(Expectation::Custom(label))
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
            input
                .add_suggestion_range(input.position..input.position + literal.len(), &expectation);
            None
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

        input.fail_expected(&Expectation::Char(expected_char))
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

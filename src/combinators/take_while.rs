use crate::{Expectation, fn_parser::FnParser, stream::Stream};

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
            input.fail_expected(&expected)
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
            input.fail_expected(&expected)
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
            input.fail_expected(&expected)
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
            input.fail_expected(&expected)
        } else {
            input.position += len;
            Some(&input.input[start..input.position])
        }
    }
}

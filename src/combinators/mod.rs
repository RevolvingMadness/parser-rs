use crate::{
    Expectation, combinators::take_while::take_while_bytes, fn_parser::FnParser, stream::Stream,
};

pub mod choice;
pub mod take_while;

pub fn literal<'a>(literal: &'static str) -> impl FnParser<'a, &'static str> {
    move |input: &mut Stream<'a>| {
        if input.remaining_bytes().starts_with(literal.as_bytes()) {
            input.position += literal.len();
            Some(literal)
        } else {
            let expectation = Expectation::Literal(literal);

            input.fail_expected(&expectation)
        }
    }
}

pub fn suggest_literal<'a>(literal: &'static str) -> impl FnParser<'a, &'static str> {
    move |input: &mut Stream<'a>| {
        if input.remaining_bytes().starts_with(literal.as_bytes()) {
            input.position += literal.len();
            Some(literal)
        } else {
            let expectation = Expectation::Literal(literal);

            input.add_suggestion_range(
                input.position..input.position + literal.len(),
                &Expectation::Literal(literal),
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

        input.fail_expected(&Expectation::Char(expected_char))
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

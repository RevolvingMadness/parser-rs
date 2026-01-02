use parser_rs::{
    FnParser, ParserRange, Stream, char, choice, digits, end_of_file, suggest_literal, take_while,
};
use std::collections::{BTreeMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

fn padded<'a, T, P>(mut parser: P) -> impl FnParser<'a, T>
where
    P: FnParser<'a, T> + 'a,
    T: 'a,
{
    move |input: &mut Stream<'a>| {
        let _ = take_while(char::is_whitespace).parse(input)?;
        let result = parser.parse(input)?;
        let _ = take_while(char::is_whitespace).parse(input)?;
        Some(result)
    }
}

fn parse_null(input: &mut Stream) -> Option<JsonValue> {
    suggest_literal("null")
        .map(|_| JsonValue::Null)
        .parse(input)
}

fn parse_bool(input: &mut Stream) -> Option<JsonValue> {
    choice((
        suggest_literal("true").map(|_| JsonValue::Bool(true)),
        suggest_literal("false").map(|_| JsonValue::Bool(false)),
    ))
    .parse(input)
}

fn parse_string(input: &mut Stream) -> Option<JsonValue> {
    char('"').parse(input)?;

    let mut content = String::new();
    loop {
        let current_char = input.consume_char();
        match current_char {
            Some('"') => break,
            Some('\\') => {
                let escaped = input.consume_char().or_else(|| {
                    input.fail_message("Unexpected end of input after escape character")
                })?;
                content.push(match escaped {
                    '"' => '"',
                    '\\' => '\\',
                    '/' => '/',
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => {
                        input.add_validation_error_position(
                            input.position - 1,
                            "Invalid escape sequence",
                        );
                        escaped
                    }
                });
            }
            Some(c) => content.push(c),
            None => return input.fail_message("Unterminated string"),
        }
    }

    Some(JsonValue::String(content))
}

fn parse_number<'a>(input: &mut Stream<'a>) -> Option<JsonValue> {
    let number_str = (|input: &mut Stream<'a>| {
        char('-').optional().parse(input)?;
        digits.parse(input)?;
        (|input: &mut Stream<'a>| {
            char('.').parse(input)?;
            digits.parse(input)?;

            Some(())
        })
        .optional()
        .parse(input)?;
        (|input: &mut Stream<'a>| {
            choice((char('e'), char('E'))).parse(input)?;
            choice((char('+'), char('-'))).optional().parse(input)?;
            digits.parse(input)?;

            Some(())
        })
        .optional()
        .parse(input)?;

        Some(())
    })
    .sliced()
    .parse(input)?;

    match number_str.parse::<f64>() {
        Ok(num) => Some(JsonValue::Number(num)),
        Err(_) => input.fail_message("Invalid number format"),
    }
}

fn parse_array(input: &mut Stream) -> Option<JsonValue> {
    let items_parser = parse_value.separated_by_trailing(padded(char(',')));

    char('[').parse(input)?;
    let items = padded(items_parser).parse(input)?;
    char(']').parse(input)?;

    Some(JsonValue::Array(items))
}

fn parse_object<'a>(input: &mut Stream<'a>) -> Option<JsonValue> {
    let kv_pair_parser = |input: &mut Stream<'a>| {
        let (key_span, key_val) = parse_string.spanned().parse(input)?;
        let key = match key_val {
            JsonValue::String(s) => s,
            _ => unreachable!(),
        };
        padded(char(':')).parse(input)?;
        let value = parse_value(input)?;
        Some(((key_span, key), value))
    };

    let items_parser = kv_pair_parser.separated_by_trailing(padded(char(',')));

    char('{').parse(input)?;
    let items: Vec<((ParserRange, String), JsonValue)> = padded(items_parser).parse(input)?;
    char('}').parse(input)?;

    let mut object = BTreeMap::new();
    let mut seen_keys = HashSet::new();
    for ((key_span, key), value) in items {
        if !seen_keys.insert(key.clone()) {
            input.add_validation_error_span(key_span, "Duplicate key in object");
        }
        object.insert(key, value);
    }

    Some(JsonValue::Object(object))
}

fn parse_value(input: &mut Stream) -> Option<JsonValue> {
    padded(choice((
        parse_string,
        parse_number,
        parse_object,
        parse_array,
        parse_bool,
        parse_null,
    )))
    .label("JSON value")
    .parse(input)
}

pub fn parse_json(input: &mut Stream) -> Option<JsonValue> {
    let value = parse_value(input)?;
    end_of_file(input)?;
    Some(value)
}

fn main() {
    let input_valid = r#"
    {
        "name": "John Doe",
        "age": 30,
        "isStudent": false,
        "courses": ["History", "Math"],
        "address": null,
        "address": "override"
    }
    "#;
    let mut stream_valid = Stream::new(input_valid, None, Some(10));
    let result = parse_json(&mut stream_valid);

    println!("Input '{}'", input_valid);
    match result {
        Some(ast) => {
            println!("{:#?}", ast);
        }
        None => unreachable!(),
    }
    println!("\n\n");

    let input_invalid = r#" { "key": tru } "#;
    let mut stream_invalid = Stream::new(input_invalid, None, Some(10));
    let result = parse_json(&mut stream_invalid);

    println!("Input '{}'", input_valid,);
    match result {
        Some(_) => unreachable!(),
        None => {
            println!("{:#?}", stream_invalid.max_error);
        }
    }
    println!("\n\n");

    let input_completion = r#" { "key": tru "#;
    let cursor_position = 10;
    let mut stream_completion = Stream::new(input_completion, Some(cursor_position), Some(10));
    let _ = parse_json(&mut stream_completion);

    println!(
        "Input '{}' with cursor at {}",
        input_completion, cursor_position
    );
    println!(
        "Generated suggestions:\n{:#?}",
        stream_completion.suggestions
    );
}

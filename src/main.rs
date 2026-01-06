use parser_rs::{FnParser, SemanticTokenKind, Stream, choice, suggest_literal};

fn main() {
    let input = "first";
    let mut input = Stream::new(input, Some(input.len()), None);
    input.semantic_tokens_enabled = true;

    let mut parser = choice((suggest_literal("first"), suggest_literal("second")));

    let result = parser.parse_with_eof(&mut input);

    println!("{:?}", result);
    println!("{:?}", input.suggestions);
    println!("{:?}", input.max_error);
    println!("{:?}", input.semantic_tokens);
}

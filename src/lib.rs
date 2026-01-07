use crate::parser_range::ParserRange;
use crate::semantic_token::SemanticToken;
use std::fmt::Debug;

pub mod accumulate;
pub mod combinators;
pub mod fn_parser;
pub mod parser_range;
pub mod semantic_token;
pub mod stream;

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

#[derive(Debug, Clone, Default, PartialEq)]
pub struct MaxParseError {
    pub span: ParserRange,
    pub messages: Vec<&'static str>,
    pub expected: Vec<Expectation>,
    pub semantic_tokens: Vec<SemanticToken>,
}

#[derive(Debug, Clone, Default, PartialEq)]
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
    pub message: String,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStateFull {
    pub position: usize,
    pub suggestions_len: usize,
    pub validation_errors_len: usize,
    pub signatures_len: usize,
    pub can_suggest_at_position: bool,
    pub force_suggest_range: Option<ParserRange>,
}

#[derive(Debug, Clone, Copy)]
pub struct StreamStatePartial {
    position: usize,
    can_suggest_at_position: bool,
    force_suggest_range: Option<ParserRange>,
    signatures_len: usize,
    signatures_depth: usize,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Signature {
    pub label: &'static str,
    pub parameters: &'static [(&'static str, Option<&'static str>)],
    pub documentation: Option<&'static str>,
    pub active_parameter: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub struct Checkpoint {
    pub position: usize,
    pub suggestions_len: usize,
    pub validation_errors_len: usize,
    pub signatures_len: usize,
    pub semantic_tokens_len: usize,
    pub signatures_depth: usize,
    pub can_suggest_at_position: bool,
    pub force_suggest_range: Option<ParserRange>,
}

#[derive(Debug)]
pub struct ParseResult<T> {
    pub result: Result<T, ParseError>,
    pub suggestions: Vec<Suggestion>,
    pub validation_errors: Vec<ValidationError>,
    pub semantic_tokens: Vec<SemanticToken>,
    pub signatures: Vec<Signature>,
}

impl<T> ParseResult<T> {
    pub fn succeeded(&self) -> bool {
        self.result.is_ok()
    }

    pub fn failed(&self) -> bool {
        self.result.is_err()
    }
}

#[cfg(test)]
mod tests {
    use super::combinators::choice::choice;
    use super::combinators::{literal, suggest_literal};
    use super::fn_parser::FnParser;
    use super::semantic_token::{SemanticToken, SemanticTokenKind};
    use super::stream::Stream;
    use super::{Expectation, ParseResult, Suggestion};

    #[cfg(test)]
    mod semantic_token_tests {
        use super::*;

        pub fn parser_1(input: Stream) -> ParseResult<()> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first")
                        .syntax(SemanticTokenKind::Function)
                        .parse(input)?;
                    suggest_literal("second").parse(input)?;

                    Some(())
                },
                suggest_literal("first")
                    .map_to(())
                    .syntax(SemanticTokenKind::Variable),
            ))
            .parse_fully(input)
        }

        #[test]
        pub fn test_semantic_variable() {
            let mut input = Stream::new("first");
            input.config.semantic_tokens = true;

            let result = parser_1(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Variable,
                }]
            );
        }

        #[test]
        pub fn test_semantic_function() {
            let mut input = Stream::new("firstsecond");
            input.config.semantic_tokens = true;

            let result = parser_1(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Function,
                }]
            );
        }
    }

    #[cfg(test)]
    mod choice_suggestion_tests {
        use super::*;

        pub fn suggestion_parser_1(input: Stream) -> ParseResult<()> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first").parse(input)?;
                    suggest_literal("second").parse(input)
                },
                suggest_literal("first"),
            ))
            .map_to(())
            .parse_fully(input)
        }

        #[test]
        pub fn test_suggest_second() {
            let input = "first";
            let input_len = input.len();
            let mut input = Stream::new(input);
            input.cursor = Some(input_len);

            let result = suggestion_parser_1(input);
            assert!(result.succeeded());
            assert_eq!(
                result.suggestions,
                vec![Suggestion {
                    range: (5..11).into(),
                    expected: Expectation::Literal("second")
                }]
            );
        }

        pub fn suggestion_parser_2(input: Stream) -> ParseResult<()> {
            choice((suggest_literal("a"), suggest_literal("b")))
                .separated_by::<_, _, ()>(suggest_literal("i"))
                .parse_fully(input)
        }

        #[test]
        pub fn test_no_suggestions() {
            let input = Stream::new("a");

            let result = suggestion_parser_2(input);
            assert!(result.succeeded());
            assert_eq!(result.suggestions, vec![]);
        }

        pub fn suggestion_parser_3(input: Stream) -> ParseResult<()> {
            choice((suggest_literal("a"), suggest_literal("b")))
                .map_to(())
                .parse_fully(input)
        }

        #[test]
        pub fn test_suggest_a_and_b() {
            let mut input = Stream::new("");
            input.cursor = Some(0);

            let result = suggestion_parser_3(input);
            assert!(result.failed());
            assert_eq!(
                result.suggestions,
                vec![
                    Suggestion {
                        range: (0..1).into(),
                        expected: Expectation::Literal("a")
                    },
                    Suggestion {
                        range: (0..1).into(),
                        expected: Expectation::Literal("b")
                    }
                ]
            );
        }
    }

    #[cfg(test)]
    mod whitespace_tests {
        use super::*;

        fn parser_no_whitespace<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((
                literal("command1").syntax(SemanticTokenKind::Variable),
                |input: &mut Stream| {
                    literal("command1")
                        .syntax(SemanticTokenKind::Function)
                        .parse(input)?;
                    literal(" ").parse(input)?;
                    literal("argument").parse(input)
                },
            ))
            .parse_fully(input)
        }

        #[test]
        pub fn test_no_ws_variable() {
            let mut input = Stream::new("command1");
            input.config.semantic_tokens = true;

            let result = parser_no_whitespace(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..8).into(),
                    kind: SemanticTokenKind::Variable
                }]
            )
        }

        #[test]
        pub fn test_no_ws_keyword() {
            let mut input = Stream::new("command1 ");
            input.config.semantic_tokens = true;

            let result = parser_no_whitespace(input);
            assert!(result.failed());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..8).into(),
                    kind: SemanticTokenKind::Function
                }]
            )
        }

        fn parser_whitespace<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            (|input: &mut Stream| {
                let r = choice((
                    literal("command1").syntax(SemanticTokenKind::Variable),
                    |input: &mut Stream| {
                        literal("command1")
                            .syntax(SemanticTokenKind::Function)
                            .parse(input)?;
                        literal(" ").parse(input)?;
                        literal("argument").parse(input)
                    },
                ))
                .parse(input)?;

                literal(" ").optional().parse(input)?;

                Some(r)
            })
            .parse_fully(input)
        }

        #[test]
        pub fn test_ws_variable_1() {
            let mut input = Stream::new("command1");
            input.config.semantic_tokens = true;

            let result = parser_whitespace(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..8).into(),
                    kind: SemanticTokenKind::Variable
                }]
            )
        }

        #[test]
        pub fn test_ws_variable_2() {
            let mut input = Stream::new("command1 ");
            input.config.semantic_tokens = true;

            let result = parser_whitespace(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..8).into(),
                    kind: SemanticTokenKind::Variable
                }]
            )
        }

        #[test]
        pub fn test_ws_function_1() {
            let mut input = Stream::new("command1 argument");
            input.config.semantic_tokens = true;

            let result = parser_whitespace(input);
            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..8).into(),
                    kind: SemanticTokenKind::Function
                }]
            )
        }
    }

    mod literal_tests {
        use super::*;

        fn parser<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            suggest_literal("literal").parse_fully(input)
        }

        #[test]
        fn test_full_fail() {
            let mut input = Stream::new("");
            input.cursor = Some(0);

            let result = parser(input);

            assert!(result.failed());

            let error = result.result.unwrap_err();

            assert_eq!(error.span, (0..1).into());
            assert_eq!(error.expected, vec![Expectation::Literal("literal")]);
            assert_eq!(
                result.suggestions,
                vec![Suggestion {
                    range: (0..7).into(),
                    expected: Expectation::Literal("literal")
                }]
            );
        }

        #[test]
        fn test_partial_fail() {
            let mut input = Stream::new("lit");
            input.cursor = Some(3);

            let result = parser(input);

            assert!(result.failed());

            let error = result.result.unwrap_err();

            assert_eq!(error.span, (0..1).into());
            assert_eq!(error.expected, vec![Expectation::Literal("literal")]);
            assert_eq!(
                result.suggestions,
                vec![Suggestion {
                    range: (0..7).into(),
                    expected: Expectation::Literal("literal")
                }]
            );
        }

        #[test]
        fn test_succeed() {
            let input = Stream::new("literal");

            let result = parser(input);

            assert!(result.succeeded());
            assert!(result.suggestions.is_empty());
        }

        fn complex_parser<'a>(input: Stream<'a>) -> ParseResult<Option<&'a str>> {
            suggest_literal("literal").optional().parse_fully(input)
        }

        #[test]
        fn test_suggest_literal() {
            let mut input = Stream::new("");
            input.cursor = Some(0);

            let result = complex_parser(input);

            assert!(result.succeeded());
            assert_eq!(result.suggestions.len(), 1);
        }
    }

    mod choice_tests {
        use super::*;

        fn parser<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((suggest_literal("a"), suggest_literal("b"))).parse_fully(input)
        }

        #[test]
        fn test_suggest_a_and_b() {
            let mut input = Stream::new("");
            input.cursor = Some(0);

            let result = parser(input);

            assert!(result.failed());

            let error = result.result.unwrap_err();

            assert_eq!(error.span, (0..1).into());
            assert_eq!(
                error.expected,
                vec![Expectation::Literal("a"), Expectation::Literal("b")]
            );

            assert_eq!(result.suggestions.len(), 2);
            assert!(result.suggestions.contains(&Suggestion {
                range: (0..1).into(),
                expected: Expectation::Literal("a")
            }));
            assert!(result.suggestions.contains(&Suggestion {
                range: (0..1).into(),
                expected: Expectation::Literal("b")
            }));
        }

        #[test]
        fn test_suggest_nothing() {
            let input = Stream::new("a");

            let result = parser(input);

            assert!(result.succeeded());
            assert_eq!(result.suggestions, vec![]);

            let input = Stream::new("b");

            let result = parser(input);

            assert!(result.succeeded());
            assert_eq!(result.suggestions, vec![]);
        }

        fn complex_parser_1<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first").parse(input)?;
                    suggest_literal("second").optional().parse(input)?;

                    Some("optional second")
                },
                suggest_literal("first"),
            ))
            .parse_fully(input)
        }

        #[test]
        fn suggest_second_1() {
            let mut input = Stream::new("first");
            input.cursor = Some(5);

            let result = complex_parser_1(input);

            assert!(result.succeeded());
            assert_eq!(
                result.suggestions,
                vec![Suggestion {
                    range: (5..11).into(),
                    expected: Expectation::Literal("second")
                }]
            );
        }

        fn complex_parser_2<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first").parse(input)?;
                    suggest_literal("second").optional().parse(input)?;

                    Some("optional second")
                },
                suggest_literal("first"),
            ))
            .parse_fully(input)
        }

        #[test]
        fn suggest_second_2() {
            let mut input = Stream::new("first");
            input.cursor = Some(5);

            let result = complex_parser_2(input);

            assert!(result.succeeded());
            assert_eq!(
                result.suggestions,
                vec![Suggestion {
                    range: (5..11).into(),
                    expected: Expectation::Literal("second")
                }]
            );
        }
    }

    mod semantic_choice_tests {
        use super::*;

        fn complex_parser_1<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first")
                        .syntax(SemanticTokenKind::Function)
                        .parse(input)?;
                    suggest_literal("second").parse(input)?;

                    Some("optional second")
                },
                suggest_literal("first").syntax(SemanticTokenKind::Variable),
            ))
            .parse_fully(input)
        }

        #[test]
        fn semantic_variable() {
            let mut input = Stream::new("first");
            input.config.semantic_tokens = true;

            let result = complex_parser_1(input);

            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Variable
                }]
            );
        }

        #[test]
        fn semantic_function() {
            let mut input = Stream::new("firstsecond");
            input.config.semantic_tokens = true;

            let result = complex_parser_1(input);

            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Function
                }]
            );
        }

        fn complex_parser_2<'a>(input: Stream<'a>) -> ParseResult<&'a str> {
            choice((
                |input: &mut Stream| {
                    suggest_literal("first")
                        .syntax(SemanticTokenKind::Function)
                        .parse(input)?;
                    suggest_literal("second").parse(input)?;
                    suggest_literal("third").parse(input)?;

                    Some("optional third")
                },
                suggest_literal("first").syntax(SemanticTokenKind::Variable),
            ))
            .parse_fully(input)
        }

        #[test]
        fn semantic_function_2() {
            let mut input = Stream::new("firstsecond");
            input.config.semantic_tokens = true;

            let result = complex_parser_2(input);

            assert!(result.failed());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Function
                }]
            );
        }

        #[test]
        fn semantic_variable_2() {
            let mut input: Stream<'_> = Stream::new("first");
            input.config.semantic_tokens = true;

            let result = complex_parser_2(input);

            assert!(result.succeeded());
            assert_eq!(
                result.semantic_tokens,
                vec![SemanticToken {
                    range: (0..5).into(),
                    kind: SemanticTokenKind::Variable
                }]
            );
        }
    }
}

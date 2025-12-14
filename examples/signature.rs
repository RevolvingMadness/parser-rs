#[cfg(test)]
mod signature_tests {
    #[cfg(test)]
    mod regular_tests {
        use parser_rs::{char, choice, literal, FnParser, Signature, Stream};

        fn parser(input: &mut Stream) -> Option<()> {
            (|input: &mut Stream| {
                literal("command").parse(input)?;
                literal(" ").parse(input)?;

                choice((
                    (|input: &mut Stream| {
                        literal("1param").next_signature_parameter().parse(input)?;
                        literal(" ").parse(input)?;
                        literal("param").next_signature_parameter().parse(input)?;
                        Some(())
                    })
                        .signature(0),
                    (|input: &mut Stream| {
                        literal("2param").next_signature_parameter().parse(input)?;
                        literal(" ").parse(input)?;
                        literal("param").next_signature_parameter().parse(input)?;
                        literal(" ").parse(input)?;
                        literal("param").next_signature_parameter().parse(input)?;
                        Some(())
                    })
                        .signature(1),
                ))
                    .parse(input)?;

                Some(())
            })
                .signatures(&[
                    ("command 1param param", &[("1param", None), ("param", None)], None),
                    ("command 2param param param", &[("2param", None), ("param", None), ("param", None)], None),
                ])
                .parse(input)?;

            Some(())
        }

        #[test]
        fn test_two_signatures_active_param_0() {
            let text = "command ";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![
                    Signature {
                        label: "command 1param param",
                        parameters: &[("1param", None), ("param", None)],
                        active_parameter: Some(0),
                        documentation: None,
                    },
                    Signature {
                        label: "command 2param param param",
                        parameters: &[("2param", None), ("param", None), ("param", None)],
                        active_parameter: Some(0),
                        documentation: None,
                    }
                ]
            );
        }

        #[test]
        fn test_two_signatures_no_active_param() {
            let text = "command ";
            let mut input = Stream::new(text, Some(7), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![
                    Signature {
                        label: "command 1param param",
                        parameters: &[("1param", None), ("param", None)],
                        active_parameter: None,
                        documentation: None,
                    },
                    Signature {
                        label: "command 2param param param",
                        parameters: &[("2param", None), ("param", None), ("param", None)],
                        active_parameter: None,
                        documentation: None,
                    }
                ]
            );
        }

        #[test]
        fn test_two_signatures_no_active_param_2() {
            let text = "command  ";
            let mut input = Stream::new(text, Some(9), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(input.signatures, vec![]);
        }

        #[test]
        fn test_1param_signature() {
            let text = "command 1param";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command 1param param",
                    parameters: &[("1param", None), ("param", None)],
                    active_parameter: Some(0),
                    documentation: None,
                },]
            );
        }

        #[test]
        fn test_1param_signature_2() {
            let text = "command 1param param#command param1 param";
            let mut input = Stream::new(text, Some(18), Some(10));
            input.signature_help_enabled = true;

            let _ = (|input: &mut Stream| {
                parser.parse(input)?;
                literal("#").parse(input)?;
                parser.parse(input)?;

                Some(())
            }).parse(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command 1param param",
                    parameters: &[("1param", None), ("param", None)],
                    active_parameter: Some(1),
                    documentation: None,
                },]
            );
        }

        #[test]
        fn test_2param_signature() {
            let text = "command 2param";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command 2param param param",
                    parameters: &[("2param", None), ("param", None), ("param", None)],
                    active_parameter: Some(0),
                    documentation: None,
                },]
            );
        }

        #[test]
        fn test_signature() {
            let text = "command 1param param";
            let mut input = Stream::new(text, Some(20), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command 1param param",
                    parameters: &[("1param", None), ("param", None)],
                    active_parameter: Some(1),
                    documentation: None,
                },]
            );
        }

        #[test]
        fn test_signature_2() {
            let text = "";
            let mut input = Stream::new(text, Some(0), Some(10));
            input.signature_help_enabled = true;

            let _ = parser.parse(&mut input);

            assert_eq!(
                input.signatures,
                vec![
                    Signature {
                        label: "command 1param param",
                        parameters: &[("1param", None), ("param", None)],
                        active_parameter: None,
                        documentation: None,
                    },
                    Signature {
                        label: "command 2param param param",
                        parameters: &[("2param", None), ("param", None), ("param", None)],
                        active_parameter: None,
                        documentation: None,
                    }
                ]
            );
        }

        #[test]
        fn test_no_signature() {
            let text = "";
            let mut input = Stream::new(text, Some(0), Some(10));
            input.signature_help_enabled = true;

            let _ = parser.separated_by::<_, _, ()>(char('/')).parse(&mut input);

            assert_eq!(input.signatures, vec![]);
        }
    }

    #[cfg(test)]
    mod complex_tests {
        use parser_rs::{choice, literal, FnParser, Signature, Stream};

        fn complex_parser(input: &mut Stream) -> Option<()> {
            (|input: &mut Stream| {
                literal("command").parse(input)?;
                literal(" ").parse(input)?;

                choice((
                    literal("mode1").signature(0),
                    (|input: &mut Stream| {
                        literal("mode2").parse(input)?;
                        literal(" ").parse(input)?;
                        literal("mode2value")
                            .next_signature_parameter()
                            .parse(input)?;

                        Some("")
                    })
                        .signature(1),
                ))
                    .signatures(&[("mode1", &[("", None)], None), ("mode2 mode2value", &[("mode2value", None)], None)])
                    .next_signature_parameter()
                    .parse(input)?;

                literal(" ").parse(input)?;

                literal("param2").next_signature_parameter().parse(input)?;

                Some(())
            })
                .signature(0)
                .signatures(&[("command <mode> param2", &[("<mode>", None), ("param2", None)], None)])
                .parse(input)?;

            Some(())
        }

        #[test]
        fn test_active_param_0() {
            let text = "command ";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = complex_parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![
                    Signature {
                        label: "mode1",
                        parameters: &[("", None)],
                        active_parameter: None,
                        documentation: None,
                    },
                    Signature {
                        label: "mode2 mode2value",
                        parameters: &[("mode2value", None)],
                        active_parameter: None,
                        documentation: None,
                    }
                ]
            );
        }

        #[test]
        fn test_nested_active_param_0() {
            let text = "command mode2 ";
            let mut input = Stream::new(text, Some(14), Some(10));
            input.signature_help_enabled = true;

            let _ = complex_parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "mode2 mode2value",
                    parameters: &[("mode2value", None)],
                    active_parameter: Some(0),
                    documentation: None,
                }]
            );
        }

        #[test]
        fn test_nested_active_param_1() {
            let text = "command mode2 mode2value param2";
            let mut input = Stream::new(text, Some(28), Some(10));
            input.signature_help_enabled = true;

            let _ = complex_parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command <mode> param2",
                    parameters: &[("<mode>", None), ("param2", None)],
                    active_parameter: Some(1),
                    documentation: None,
                }]
            );
        }

        #[test]
        fn test_active_param_1() {
            let text = "command mode1 ";
            let mut input = Stream::new(text, Some(14), Some(10));
            input.signature_help_enabled = true;

            let _ = complex_parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command <mode> param2",
                    parameters: &[("<mode>", None), ("param2", None)],
                    active_parameter: Some(1),
                    documentation: None,
                }]
            );
        }
    }

    #[cfg(test)]
    mod whitespace_tests {
        use parser_rs::{literal, Expectation, FnParser, Signature, Stream};

        pub fn parse_required_whitespace(input: &mut Stream) -> Option<()> {
            let mut is_first = true;

            loop {
                let bytes = input.remaining().as_bytes();

                let ws_len = bytes
                    .iter()
                    .position(|b| !b.is_ascii_whitespace())
                    .unwrap_or(bytes.len());
                let threshold = if is_first { 1 } else { 0 };
                if ws_len > threshold {
                    input.allow_suggestions(input.position + threshold, input.position + ws_len);
                }
                input.position += ws_len;
                is_first = false;

                let bytes = input.remaining().as_bytes();

                if bytes.starts_with(b"//") {
                    let len = bytes
                        .iter()
                        .position(|b| *b == b'\n')
                        .map(|p| p + 1)
                        .unwrap_or(bytes.len());
                    input.position += len;
                } else if bytes.starts_with(b"/*") {
                    match bytes.windows(2).position(|w| w == b"*/") {
                        Some(pos) => {
                            input.position += pos + 2;
                        }
                        None => {
                            input.position += bytes.len();

                            input.add_suggestion(input.position, &Expectation::Literal("*/"));

                            return input.fail_message("Unclosed block comment");
                        }
                    }
                } else {
                    break;
                }
            }

            Some(())
        }

        fn parser(input: &mut Stream) -> Option<()> {
            (|input: &mut Stream| {
                literal("command").parse(input)?;
                parse_required_whitespace(input)?;
                literal("param").next_signature_parameter().parse(input)?;

                Some(())
            })
                .signature(0)
                .signatures(&[("command param", &[("param", None)], None)])
                .parse(input)?;

            Some(())
        }

        #[test]
        fn test_signature_whitespace() {
            let text = "command  param";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command param",
                    parameters: &[("param", None)],
                    active_parameter: Some(0),
                    documentation: None,
                }]
            );
        }

        #[test]
        fn test_signature_whitespace_2() {
            let text = "command  ";
            let mut input = Stream::new(text, Some(8), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command param",
                    parameters: &[("param", None)],
                    active_parameter: Some(0),
                    documentation: None,
                }]
            );
        }

        #[test]
        fn test_signature_whitespace_3() {
            let text = "command  ";
            let mut input = Stream::new(text, Some(7), Some(10));
            input.signature_help_enabled = true;

            let _ = parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command param",
                    parameters: &[("param", None)],
                    active_parameter: None,
                    documentation: None,
                }]
            );
        }
    }
}

fn main() {}
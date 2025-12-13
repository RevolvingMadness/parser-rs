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
                ("command 1param param", &["1param", "param"]),
                ("command 2param param param", &["2param", "param", "param"]),
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
                        parameter_labels: &["1param", "param"],
                        active_parameter: Some(0)
                    },
                    Signature {
                        label: "command 2param param param",
                        parameter_labels: &["2param", "param", "param"],
                        active_parameter: Some(0)
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
                        parameter_labels: &["1param", "param"],
                        active_parameter: None
                    },
                    Signature {
                        label: "command 2param param param",
                        parameter_labels: &["2param", "param", "param"],
                        active_parameter: None
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
                    parameter_labels: &["1param", "param"],
                    active_parameter: Some(0)
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
                    parameter_labels: &["2param", "param", "param"],
                    active_parameter: Some(0)
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
                    parameter_labels: &["1param", "param"],
                    active_parameter: Some(1)
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
                        parameter_labels: &["1param", "param"],
                        active_parameter: None
                    },
                    Signature {
                        label: "command 2param param param",
                        parameter_labels: &["2param", "param", "param"],
                        active_parameter: None
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

                choice((literal("mode1").signature(0), |input: &mut Stream| {
                    literal("mode2").signature(1).parse(input)?;
                    literal(" ").parse(input)?;
                    literal("mode2value")
                        .next_signature_parameter()
                        .parse(input)?;

                    Some("")
                }))
                .signatures(&[("mode1", &[""]), ("mode2 mode2value", &["mode2value"])])
                .next_signature_parameter()
                .parse(input)?;

                literal(" ").parse(input)?;

                literal("param2").next_signature_parameter().parse(input)?;

                Some(())
            })
            .signatures(&[("command <mode> param2", &["<mode>", "param2"])])
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
                        parameter_labels: &[""],
                        active_parameter: None
                    },
                    Signature {
                        label: "mode2 mode2value",
                        parameter_labels: &["mode2value"],
                        active_parameter: None
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
                    parameter_labels: &["mode2value"],
                    active_parameter: Some(0)
                }]
            );
        }

        #[test]
        fn test_active_param_1() {
            let text = "command mode2 mode2value ";
            let mut input = Stream::new(text, Some(25), Some(10));
            input.signature_help_enabled = true;

            let _ = complex_parser(&mut input);

            assert_eq!(
                input.signatures,
                vec![Signature {
                    label: "command <mode> param2",
                    parameter_labels: &["<mode>", "param2"],
                    active_parameter: Some(1)
                }]
            );
        }
    }

    #[cfg(test)]
    mod whitespace_tests {
        use parser_rs::{literal, Expectation, FnParser, Signature, Stream};

        pub fn parse_whitespace(input: &mut Stream) -> Option<()> {
            loop {
                let bytes = input.remaining().as_bytes();

                let ws_len = bytes
                    .iter()
                    .position(|b| !b.is_ascii_whitespace())
                    .unwrap_or(bytes.len());
                if ws_len > 0 {
                    input.allow_suggestions(input.position, input.position + ws_len);
                }
                input.position += ws_len;

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
                parse_whitespace(input)?;
                literal("param").next_signature_parameter().parse(input)?;

                Some(())
            })
            .signatures(&[("command param", &["param"])])
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
                    parameter_labels: &["param"],
                    active_parameter: Some(0)
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
                    parameter_labels: &["param"],
                    active_parameter: Some(0)
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
                    parameter_labels: &["param"],
                    active_parameter: None
                }]
            );
        }
    }
}

fn main() {}

use crate::{
    Signature, Suggestion, ValidationError, fn_parser::FnParser, semantic_token::SemanticToken,
    stream::Stream,
};

macro_rules! impl_choice_tuple {
    ($($idx:tt : $name:ident),*) => {
        impl<'a, T, $($name),*> FnParser<'a> for ($($name,)*)
        where
            $($name: FnParser<'a, Output = T>),*
        {
            type Output = T;

            fn parse(&mut self, input: &mut Stream<'a>) -> Option<T> {
                let start_checkpoint = input.checkpoint();

                let mut best_success_pos = start_checkpoint.position;
                let mut best_success_result: Option<T> = None;
                let mut best_success_meta: (Vec<ValidationError>, Vec<Signature>, Vec<SemanticToken>) = (Vec::new(), Vec::new(), Vec::new());
                let mut best_success_state = (
                    start_checkpoint.signatures_depth,
                    start_checkpoint.can_suggest_at_position,
                    start_checkpoint.force_suggest_range
                );

                let mut all_suggestions: Vec<Suggestion> = Vec::new();

                $(
                    let result = self.$idx.parse(input);

                    if input.suggestions.len() > start_checkpoint.suggestions_len {
                        let new_suggestions = input.suggestions[start_checkpoint.suggestions_len..].to_vec();
                        all_suggestions.extend(new_suggestions);
                    }

                    if let Some(res) = result {
                        if best_success_result.is_none() || input.position > best_success_pos {
                            best_success_pos = input.position;
                            best_success_result = Some(res);

                            best_success_meta.0 = if input.validation_errors.len() > start_checkpoint.validation_errors_len {
                                input.validation_errors[start_checkpoint.validation_errors_len..].to_vec()
                            } else { Vec::new() };
                            best_success_meta.1 = if input.signatures.len() > start_checkpoint.signatures_len {
                                input.signatures[start_checkpoint.signatures_len..].to_vec()
                            } else { Vec::new() };
                            best_success_meta.2 = if input.semantic_tokens.len() > start_checkpoint.semantic_tokens_len {
                                input.semantic_tokens[start_checkpoint.semantic_tokens_len..].to_vec()
                            } else { Vec::new() };

                            best_success_state = (
                                input.signatures_depth,
                                input.can_suggest_at_position,
                                input.force_suggest_range
                            );
                        }
                    }

                    input.full_rollback(start_checkpoint);
                )*

                if let Some(result) = best_success_result {
                    input.position = best_success_pos;

                    let should_add_all_suggestions = if let Some(cursor) = input.cursor {
                        best_success_pos <= cursor
                    } else {
                        false
                    };

                    if should_add_all_suggestions {
                        for suggestion in all_suggestions {
                            if !input.suggestions.contains(&suggestion) {
                                input.suggestions.push(suggestion);
                            }
                        }
                    }

                    input.validation_errors.extend(best_success_meta.0);
                    input.signatures.extend(best_success_meta.1);
                    input.semantic_tokens.extend(best_success_meta.2);

                    input.signatures_depth = best_success_state.0;
                    input.can_suggest_at_position = best_success_state.1;
                    input.force_suggest_range = best_success_state.2;

                    return Some(result);
                }

                input.suggestions.extend(all_suggestions);

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
pub fn choice<'a, T, P>(parser: P) -> impl FnParser<'a, Output = T>
where
    P: FnParser<'a, Output = T>,
{
    parser
}

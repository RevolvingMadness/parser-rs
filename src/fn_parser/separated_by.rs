use crate::{accumulate::Accumulate, fn_parser::FnParser, stream::Stream};

pub trait FnParserSeparatedBy<'a, T, C>
where
    Self: Sized,
{
    fn separated_by<S, U>(mut self, mut separator: S) -> impl FnParser<'a>
    where
        S: FnParser<'a>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();

            let mut collection = C::default();

            let (advanced, first) = match self.parse(input, &mut collection) {
                Some(first) => (input.position != partial.position, first),
                None => {
                    if input.position != partial.position {
                        return None;
                    }

                    input.restore_partial(partial);

                    return Some(C::default());
                }
            };

            collection.accumulate(first);

            if !advanced {
                return Some(collection);
            }

            loop {
                let separator_partial = input.save_partial();

                match separator.parse(input) {
                    Some(_) => {
                        let partial = input.save_partial();

                        match self.parse(input, &mut collection) {
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

                                input.restore_partial(separator_partial);
                                break;
                            }
                        }
                    }
                    None => {
                        if input.position != separator_partial.position {
                            return None;
                        }

                        input.restore_partial(separator_partial);

                        break;
                    }
                }
            }

            Some(collection)
        }
    }

    fn separated_by_trailing<S>(mut self, mut separator: S) -> impl FnParser<'a>
    where
        S: FnParser<'a>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let partial = input.save_partial();

            let mut collection = C::default();

            let (advanced, first) = match self.parse(input, &mut collection) {
                Some(first) => (input.position != partial.position, first),
                None => {
                    if input.position != partial.position {
                        return None;
                    }

                    input.restore_partial(partial);

                    return Some(C::default());
                }
            };

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

                        match self.parse(input, &mut collection) {
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

    fn separated_by_one<S, U>(mut self, mut separator: S) -> impl FnParser<'a>
    where
        S: FnParser<'a>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();

            let start = input.position;
            let first = self.parse(input, &mut collection)?;
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
                        match self.parse(input, &mut collection) {
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

    fn separated_by_range<S, U>(
        mut self,
        min: usize,
        max: usize,
        mut separator: S,
    ) -> impl FnParser<'a>
    where
        S: FnParser<'a>,
        C: Default + Accumulate<T>,
    {
        move |input: &mut Stream<'a>| {
            let mut collection = C::default();
            let mut count = 0;

            let partial = input.save_partial();

            match self.parse(input, &mut collection) {
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

                        match self.parse(input, &mut collection) {
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

    fn parse(&mut self, input: &mut Stream<'a>, accumulated: &mut C) -> Option<T>;
}

impl<'a, T, F, C> FnParserSeparatedBy<'a, T, C> for F
where
    F: FnMut(&mut Stream<'a>, &mut C) -> Option<T>,
{
    fn parse(&mut self, input: &mut Stream<'a>, accumulated: &mut C) -> Option<T> {
        self(input, accumulated)
    }
}

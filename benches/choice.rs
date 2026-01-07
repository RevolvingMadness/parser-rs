use chumsky::error::Rich;
use chumsky::primitive::{choice as chumsky_choice, just};
use chumsky::{Parser, extra};
use criterion::{Criterion, criterion_group, criterion_main};
use parser_rs::fn_parser::FnParser;
use parser_rs::stream::Stream;
use parser_rs::{
    ParseResult,
    combinators::{char, choice::choice as parser_rs_choice},
};
use std::hint::black_box;

fn parser_rs_parser(input: Stream) -> ParseResult<()> {
    parser_rs_choice((
        char('a'),
        char('b'),
        char('c'),
        char('d'),
        char('e'),
        char('f'),
        char('g'),
        char('h'),
        char('i'),
        char('j'),
        char('k'),
        char('l'),
        char('m'),
        char('n'),
        char('o'),
        char('p'),
        char('q'),
        char('r'),
        char('s'),
        char('t'),
        char('u'),
        char('v'),
        char('w'),
        char('x'),
        char('y'),
        char('z'),
    ))
    .many::<()>()
    .parse_fully(input)
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("choice");

    let input = "z".repeat(500);

    group.bench_function("parser-rs-v2", |b| {
        b.iter(|| {
            let stream = Stream::new(&input);

            black_box(parser_rs_parser(stream));
        })
    });

    let chumsky_parser = chumsky_choice((
        just::<_, &str, extra::Err<Rich<_>>>('a'),
        just('b'),
        just('c'),
        just('d'),
        just('e'),
        just('f'),
        just('g'),
        just('h'),
        just('i'),
        just('j'),
        just('k'),
        just('l'),
        just('m'),
        just('n'),
        just('o'),
        just('p'),
        just('q'),
        just('r'),
        just('s'),
        just('t'),
        just('u'),
        just('v'),
        just('w'),
        just('x'),
        just('y'),
        just('z'),
    ))
    .repeated()
    .ignored();

    group.bench_function("chumsky", |b| {
        b.iter(|| {
            black_box(chumsky_parser.parse(&input));
        })
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

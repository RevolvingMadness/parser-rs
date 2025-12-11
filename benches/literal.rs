use chumsky::error::Rich;
use chumsky::primitive::just;
use chumsky::{extra, Parser};
use criterion::{criterion_group, criterion_main, Criterion};
use parser_rs::{literal, FnParser, Stream};
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("literal");

    let input = "literal".repeat(10000);

    group.bench_function("parser-rs-v2", |b| {
        b.iter(|| {
            let mut stream = Stream::new(&input, None, None);

            black_box(literal("literal").many::<()>().parse(&mut stream))
        })
    });

    let chumsky_parser = just::<_, &str, extra::Err<Rich<_>>>("literal")
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

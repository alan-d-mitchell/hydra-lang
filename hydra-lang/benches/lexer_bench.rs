use hydra_lang::hydrac::parse::lexer::lexer::Lexer;

use std::fs;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_real_file(c: &mut Criterion) {
    let contents = fs::read_to_string("tests/lex_benchmark.hydra").expect("file not found");

    c.bench_function("tokenize_real_file", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(&contents));
            let _ = black_box(lexer.tokenize().unwrap());
        })
    });
}

criterion_group!(benches, benchmark_real_file);
criterion_main!(benches);
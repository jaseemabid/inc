# An incremental scheme compiler [![Build Status][tbadge]][travis] [![Docs][dbadge]][docs]

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][paper] by Abdulaziz
Ghuloum.

## Getting started

    $ cargo build
    $ cargo test
    $ echo "(let ((a 1) (b 2)) (+ a b))" | cargo run -q

The generated assembly is usually easy to read

    $ echo "(let ((a 1) (b 2)) (+ a b))" | cargo run -q -- -S

## Docs

Inc is reasonably well documented and is preferably read with Cargo docs.

    $ cargo doc --document-private-items --open

## Colophon

This project started in [Chez], later [ported it to Racket][rkt] and then again
to [rust]. The old project still lives at [rkt](./rkt).

[Chez]:    https://www.scheme.com
[book]:    https://doc.rust-lang.org/book/#the-rust-programming-language
[docs]:    https://docs.rs/inc
[dbadge]:  https://docs.rs/inc/badge.svg
[iter]:    https://hermanradtke.com/2015/06/22/effectively-using-iterators-in-rust.html
[paper]:   docs/paper.pdf?raw=true
[rkt]:     https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:    https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
[travis]:  https://travis-ci.org/jaseemabid/inc
[tbadge]:  https://travis-ci.org/jaseemabid/inc.svg?branch=master

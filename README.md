# 🌱 A tiny scheme compiler [![Build Status][tbadge]][travis] [![Docs][dbadge]][docs] [![](https://meritbadge.herokuapp.com/inc)](https://crates.io/crates/inc)

🐳 A tiny educational [Scheme][wiki] compiler written in Rust that generates
readable x86 assembly. Implements the paper [An Incremental Approach to Compiler
Construction][paper] by Abdulaziz Ghuloum. We aim to be complete, correct and fast,
in that order of importance.

## Getting started

    $ cargo build
    $ cargo test

Running simple programs is straight forward.

    $ echo "(define (twice x) (* x 2)) (twice 21)" | cargo run -q
    42

## How does this work?

The previous step generates x86 assembly that gets compiled to a very tiny
(~13KB) native executable binary along with some runtime written in Rust and
some glue code in C.

    $ ./inc
    42

    $ file inc
    inc: Mach-O 64-bit executable x86_64

    $ stat -f "%z" inc
    13556

The generated assembly is usually easy to read, and if you squint hard enough
kinda looks like the source code 😉

     $ echo "(define (twice x) (* x 2)) (twice 21)" | cargo run -q -- -S

```asm
    .section __TEXT,__text
    .intel_syntax noprefix

    .globl "_init"
"_init":
    push rbp
    mov rbp, rsp
    mov r12, rdi        # Store heap index to R12
    mov rax, 168
    mov qword ptr [rbp - 24], rax
    call "twice"
    pop rbp
    ret

    .globl "twice"
"twice":
    push rbp
    mov rbp, rsp
    mov rax, [rbp - 8]
    mov qword ptr [rbp - 16], rax
    mov rax, 16
    sar rax, 3
    mul qword ptr [rbp - 16]
    pop rbp
    ret
```

Under the hood, inc compiles scheme to x86 assembly and uses Clang (or GCC on
Linux) to generate machine executable binaries.

Generate the asm

    $ echo "(define (twice x) (* x 2)) (twice 21)" | cargo run -q -- -S > inc.s

Compile the runtime as well the generated assembly into shared object files

    $ clang -c inc.s     # Generates inc.o
    $ clang -c runtime.c # Generates runtime.o
    $ cargo build        # Generates ./target/debug/libinc.dylib

Link it all together with a linker

    $ ld -L./target/debug runtime.o inc.o -linc -ldl -lpthread -o inc

The same binary is generated again

    $ ./inc
    42

Conveniently clang can do it all in one step if you prefer it that way.

    $ clang -L./target/debug inc.s runtime.c  -linc -ldl -lpthread -o inc

## Docs

Inc is reasonably well documented and is preferably read with Cargo docs. Build
docs locally or [read online][docs] (⚠ Could be outdated)

    $ cargo doc --document-private-items --open

## Where can I learn more?

1. Read the [paper] for an overview
2. Watch a [talk about this project][talk] if that works better.
3. [Ask HN: What's the best resource for learning modern x64 assembly?][hn]

## Colophon

This project started in [Chez], later [ported it to Racket][rkt] and then again
to [rust]. The old project still lives at [rkt](./rkt).

[book]:    https://doc.rust-lang.org/book/#the-rust-programming-language
[chez]:    https://www.scheme.com
[dbadge]:  https://docs.rs/inc/badge.svg
[docs]:    https://docs.rs/inc
[iter]:    https://hermanradtke.com/2015/06/22/effectively-using-iterators-in-rust.html
[paper]:   docs/paper.pdf?raw=true
[rkt]:     https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:    https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
[talk]:    https://www.youtube.com/watch?v=WBWRkUuyuE0
[tbadge]:  https://travis-ci.org/jaseemabid/inc.svg?branch=master
[travis]:  https://travis-ci.org/jaseemabid/inc
[wiki]:    https://en.wikipedia.org/wiki/Scheme_(programming_language)
[hn]:      https://news.ycombinator.com/item?id=22279051

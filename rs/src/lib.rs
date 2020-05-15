#![feature(bindings_after_at)] // For Lambda(code @ Closure { name: Some(n), .. }) => {
#![feature(box_syntax, box_patterns)]
#![feature(inner_deref)] // For Option::as_deref
#![feature(llvm_asm)]
#![feature(const_extern_fn)]
#![deny(clippy::missing_const_for_fn)]

/*!
# An Incremental scheme compiler

A tiny scheme to x86 asm compiler as described in the paper [An Incremental
Approach to Compiler Construction][paper] by Abdulaziz Ghuloum.

## Where do I get started? 🕵️‍♀️

Read the first few sections of the [paper] to understand the premise.

There is a mix of [Scheme], C, Rust and x86 assembly here and these are some
good places to start learning them.

- [x86 module documentation](crate::x86) contains links to a few good x86/assembly tutorials.
- [How to C in 2016](https://matt.sh/howto-c) is a pretty good C refresher.
- [The Rust Programming language][book] book is a good place to start learning rust.

Compile the project and run a few sample programs (see README.md or tests)
before diving deep into the code.

The [core module](core) contains the main data types used by the compiler and is
a good place to start reading the code. [parser](parser) implements a scheme
parser using [nom](nom) can be understood independently from the rest of the
project. [immediate](crate::immediate) documents the runtime representation of
the scheme objects. [rt](rt) and [ffi](ffi) describe the runtime nuances like
memory management while [lang](lang) implements language level semantics like
functions and variables.

See [docs](docs) for some additional notes and comments.

[Scheme]: https://www.scheme.com
[book]:   https://doc.rust-lang.org/book/#the-rust-programming-language
[paper]:  https://github.com/jaseemabid/inc/blob/master/docs/paper.pdf
*/

pub mod cli;
pub mod compiler;
pub mod core;
pub mod docs;
pub mod ffi;
pub mod immediate;
pub mod lambda;
pub mod lang;
pub mod parser;
pub mod primitives;
pub mod rt;
pub mod strings;
pub mod symbols;
pub mod x86;

#![feature(slice_patterns, box_syntax, box_patterns)]
/*!

# An Incremental scheme compiler

A tiny scheme to x86 asm compiler as described in the paper [An Incremental
Approach to Compiler Construction][paper] by Abdulaziz Ghuloum.

## Where do I get started? 🕵️‍♀️

Read the first few sections of the paper to understand the premise.

## Background Reading 📚

There is a lot of C, Rust and x86 assembly here and these are some good places
to start learning them.

- [x86 module documentation](crate::x86) contains links to a few good x86 tutorials.
- [How to C in 2016](https://matt.sh/howto-c) is a pretty good C refresher.
- [The Rust Programming language][book] book is a good place to start learning rust.

This project also uses a lot of iterators, so [Effectively Using Iterators In
Rust][iter] might be useful as well

## Misc

Micro blogs & lessons learned 🤷

### 1. Debugging with GDB

Debugging (occasionally wrong) generated assembly without a debugger is pretty
hard and it is absolutely worth the effort getting familiar with gdb. GDB
doesn't work on OSX despite the several dozens of blogs that claim otherwise and
this project would be impossible without gdb. It is easier to setup remote
debugging with docker than fight code signing on osx.

Build the image

```bash
$ docker build . -t inc:latest
```

Run the container in privileged mode and expose a port

```bash
$ docker run --rm -it --privileged -p 8080:8080 inc
```


Run the program you want to debug in the container and build the executable

```bash
/inc# echo "(let ((f (lambda (x) (+ x 1)))) (f 41))" | cargo run -q
```

Start a remote debugging session

```bash
/inc# gdbserver 127.0.0.1:8080 ./inc
```

Start GDB on the host machine with the custom `.gdbinit` file

```bash
$ cat .gdbinit

set startup-with-shell off
target remote 127.0.0.1:8080
```

This should work with the CLI as well as Emacs

```bash
$ gdb

Reading /inc/inc from remote target...
warning: File transfers from remote targets can be slow. Use "set sysroot" to access files locally instead.
Reading /inc/inc from remote target...
Reading symbols from target:/inc/inc...
Reading /lib64/ld-linux-x86-64.so.2 from remote target...
Reading /lib64/ld-linux-x86-64.so.2 from remote target...
Reading /lib64/5dfd7b95be4ba386fd71080accae8c0732b711.debug from remote target...
Reading /lib64/.debug/5dfd7b95be4ba386fd71080accae8c0732b711.debug from remote target...
Reading /usr/local/Cellar/gdb/8.3/lib/debug//lib64/5dfd7b95be4ba386fd71080accae8c0732b711.debug from remote target...
Reading /usr/local/Cellar/gdb/8.3/lib/debug/lib64//5dfd7b95be4ba386fd71080accae8c0732b711.debug from remote target...
Reading target:/usr/local/Cellar/gdb/8.3/lib/debug/lib64//5dfd7b95be4ba386fd71080accae8c0732b711.debug from remote target...
0x00007ffff7fd6090 in ?? () from target:/lib64/ld-linux-x86-64.so.2
(gdb)
```

![Screenshot of GDB running in Emacs over remote protocol][screenshot]

### 2. All the different kind of functions

While implementing stdlib functions, I noticed that they belong to a few
different levels - closely resembling the kind of privilege they have.

The low level primitives get access to everything, including the register
allocation. The runtime functions know about the memory layout of objects. A
scheme function is far more limited and can only see the high level functional
constructs. When possible a function should be implemented in the highest level
possible - prefer scheme over rust for safety and kind of a self referential
check.

**Primitives**

These are things you really have to build into the core of the compiler and are
written in Rust. `primitives::string::make` is a pretty good example since
inlining the string constants is not something you could do with scheme.

**Sort of primitives**

All the math! You don't really have to implement + and ** in Rust, but it allows
the compiler to not treat them as function calls and emit a single efficient
instruction immediately. I'd consider a compiler performing basic math during
compilation as form of interpretation - inc doesn't do this, but is fairly
trivial to implement.

**Runtime**

Functions like `string-length` understand the memory layout of the objects and
is probably easiest done in C or ASM. Because of the currently odd 'everything
in stack' calling convention, this is written in asm instead of C, but must be
rewritten in C for simplicity once FFI works.

All syscalls and FFI probably belong here in the same level.

**Stdlib**

AFAIU there shouldn't be a difference b/w user defined functions and functions
shipped as a stdlib implemented in scheme.


[Chez]:        https://www.scheme.com
[book]:        https://doc.rust-lang.org/book/#the-rust-programming-language
[iter]:        https://hermanradtke.com/2015/06/22/effectively-using-iterators-in-rust.html
[paper]:       https://github.com/jaseemabid/inc/blob/master/docs/paper.pdf
[rkt]:         https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:        https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
[screenshot]:  https://raw.githubusercontent.com/jaseemabid/inc/master/docs/gdb.png

*/

pub mod cli;
pub mod compiler;
pub mod core;
pub mod immediate;
pub mod lambda;
pub mod lang;
pub mod parser;
pub mod primitives;
pub mod rt;
pub mod ffi;
pub mod strings;
pub mod symbols;
pub mod x86;

/*!
# ✏ Other notes and documentation

# Not all functions are implemented the same!

There are a few different choices when implementing Scheme primitives and each
approach comes with a distinctive set of pros and cons.

### 1. Functions written in pure Scheme as part of standard library

📣 This is the preferred approach whenever possible.

Stdlib is the best way to measure the expressiveness and effectiveness of this
compiler. This module can serve as an example of what this compiler can do, and
as a small set of tests that's always checked for.

### 2. Rust Runtime functions implemented in Rust

Functions like [string-length](rt::string_length) understands the memory layout
of the objects and is probably easiest done in Rust instead of Scheme. Low level
memory handling and bit fiddling is where Rust shines and there is no need to
migrate it all into Scheme.

Code here is expressive, powerful and easy to maintain thanks to Rust.

### 3. Functions implemented in C/ASM

⚠ Anything that can be done here should be written in Rust and this approach is
documented only for completeness sake.

Most of the C runtime was ported to Rust, but there is tiny bit of C left
including a thin `main` function and a signal handler for managing segmentation
faults. This will also be migrated over at some point.

There will always be an indirect dependency on C for system calls, but it can be
conveniently abstracted away behind a few rust helper functions.

### 4. Primitives implemented naively within the compiler

Compiler primitives get access to everything from the language semantics to the
lowest level of implementation details like register allocation.

With great power comes great responsibility!

Some functions like [car](primitives::car) or [cdr](primitives::cdr) are at the
very core of this implementation and it makes sense to leave them as primitives
even though they maybe implemented in Rust. Specific cases like inlining strings
and symbols are best implemented here. You don't really have to implement `+`
and `*` as a primitive, but it allows the compiler to not treat them as function
calls and emit a single efficient instruction immediately. I'd consider a
compiler performing basic math during compilation as form of interpretation -
inc doesn't do this, but is fairly trivial to implement.

Some of the type checks are implemented as primitives for now, but they need not
be.

These functions are the highest overhead to maintain since there is no static
analysis of any kind.

# Debugging with GDB

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
Reading /lib64/5df711.debug from remote target...
Reading /lib64/.debug/5dfd7711.debug from remote target...
Reading /usr/local/Cellar/gdb/8.3/lib/debug//lib64/5df711.debug from remote target...
Reading /usr/local/Cellar/gdb/8.3/lib/debug/lib64//5df711.debug from remote target...
Reading target:/usr/local/Cellar/gdb/8.3/lib/debug/lib64//5df711.debug from remote target...
0x00007ffff7fd6090 in ?? () from target:/lib64/ld-linux-x86-64.so.2
(gdb)
```

![Screenshot of GDB running in Emacs over remote protocol][screenshot]

[screenshot]:  https://raw.githubusercontent.com/jaseemabid/inc/master/docs/gdb.png

 */

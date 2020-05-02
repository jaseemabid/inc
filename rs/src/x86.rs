//! A general purpose x86 library.
//!
//! # Getting started
//!
//! The easiest way to learn assembly is to write very simple C programs and
//! look at the generated code. Clang and GCC support `-S` flag to output the
//! assembly instead of a binary executable.
//!
//! Some reasonably good tutorials are:
//!
//! 1. [x86 Assembly Guide 1](https://www.cs.virginia.edu/~evans/cs216/guides/x86.html)
//! 2. [x86 Assembly Guide 2](http://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html)
//! 3. Ops like `.p2align` are not x86 instructions but GNU assembly directives.
//! See [GNU assembler docs](https://sourceware.org/binutils/docs-2.32/as/).
//!
//! # Syntax
//!
//! Intel syntax is used everywhere instead of AT&T, which is so much more
//! painful to read.
//!
//! 1. [x86 assembly language | Syntax](https://en.wikipedia.org/wiki/X86_assembly_language#Syntax)
//! 2. [AT&T Syntax versus Intel Syntax](https://www.cs.cmu.edu/afs/cs/academic/class/15213-f01/docs/gas-notes.txt)
//!
//! # Portability
//!
//! This should work on osx and Linux. Platform specific code is annotated and
//! picked at compile time where possible.
//!
//! 1. [Writing 64 Bit Assembly on Mac OS X](https://www.idryman.org/blog/2014/12/02/writing-64-bit-assembly-on-mac-os-x)
//!
//! # Calling conventions on x86
//!
//! [The history of calling conventions, part 1][history] is a really good read
//! on this subject.
//!
//! ## C Declaration (cdecl)
//!
//! [cdecl] mandates that all arguments are passed in stack in reverse order
//! (right to left) *above* the stack pointer. The caller cleans up the stack
//! because the callee might not know how many arguments were present (hint:
//! variadic arguments). See [history] for details.
//!
//! Most literature and documentation suggests that this is the default
//! convention used by Clang and GCC on Linux, but in practice compilers seems
//! to default to System V instead.
//!
//! ## Inc style
//!
//! This paper implements a variation of cdecl for simplicity - mainly because
//! register allocation is a fairly hard problem. This is incidentally very
//! similar to the Pascal style.
//!
//! All arguments are passed in stack left to right below the stack pointer.
//! Stack space is reclaimed by subsequent calls and is not implicitly cleaned.
//!
//! ### System V AMD64 ABI
//!
//! GCC on x86-64 seems to be *actually* using System V AMD64 ABI. User defined
//! functions, primitives as well as FFI into C must use this convention for
//! simplicity when possible.
//!
//! Arguments are passed in the registers RDI, RSI, RDX, RCX, R8, R9 and the
//! return value is passed back in RAX. Registers RBX, RSP, RBP, R12, R13, R14,
//! and R15 are preserved by the callee - these are saved on the callee's stack
//! and restored before the function returns and any changes made to the
//! registers in the function will be lost after it returns. Registers RAX, RDI,
//! RSI, RDX, RCX, R8, R9, R10, R11 are similarly saved by the caller if needed.
//!
//! ## Reference Reading
//!
//! 1. [x86 calling conventions](https://en.wikipedia.org/wiki/X86_calling_conventions)
//! 1. [System V ABI](https://wiki.osdev.org/System_V_ABI)
//! 1. [System V Application Binary Interface](https://github.com/jaseemabid/inc/blob/master/docs/Sys%20V%20ABI.pdf)
//! 1. [What are callee and caller saved registers?](https://stackoverflow.com/questions/9268586/what-are-callee-and-caller-saved-registers)
//1 1. [Understanding how function call works](https://zhu45.org/posts/2017/Jul/30/understanding-how-function-call-works/)
//! 1. [Slides: Assembly Language: Function Calls](https://www.cs.princeton.edu/courses/archive/spr11/cos217/lectures/15AssemblyFunctions.pdf)
//! 1. [Inline Assembly](https://wiki.osdev.org/Inline_assembly)
//! 1. [Rust book on inline assembly](https://doc.rust-lang.org/1.8.0/book/inline-assembly.html)
//!
//! [cdecl]: https://en.wikipedia.org/wiki/X86_calling_conventions#cdecl
//! [history]: https://devblogs.microsoft.com/oldnewthing/?p=41213
use std::fmt;
use std::ops::{Add, AddAssign, Sub};

/// Word size of the architecture
pub const WORDSIZE: i64 = 8;

/// An x86 instruction
///
/// This is a simple `newtype` wrapper over string, with a bunch of helpers to
/// make the caller's API clean.
#[derive(Debug, PartialEq, Clone)]
pub struct Ins(pub String);

/// ASM represents a list of instructions
#[derive(Default, Clone)]
pub struct ASM(pub Vec<Ins>);

/// A Reference is a valid address to an x86 instruction.
///
/// A large number of instructions (for example add and mov) takes both
/// registers, addresses and constants as operands and an explicit type that
/// covers it all is quite useful.
///
/// A prior version used trait objects for this. Even though trait objects made
/// the API a lot cleaner for the callers, (`x86::mov(RAX, 42)` instead of
/// `x86::mov(RAX.into(), 42.into())`, it turned out to be a complex thing to
/// implement. Dynamic trait objects are quite painful to work with - its tricky
/// to manage the lifetimes, the errors aren't easy to understand and in general
/// its a fairly complicated under the hood.
///
/// This is an explicit trade off of simplicity vs verbosity that can be
/// revisited later.
///
/// # Examples
///
/// ```
/// # use inc::x86::{self, Register::*, *};
/// assert_eq!(Ins::from("add rax, rax"), add(RAX.into(), RAX.into()))
/// ```
///
/// Numeric literals work as constants.
/// ```
/// # use inc::x86::{self, Register::*, *};
/// assert_eq!(Ins::from("add rdx, 7"), add(RDX.into(), 7.into()))
/// ```
///
/// Arithmetic on registers can be used for relative addresses.
/// ```
/// # use inc::x86::{self, Register::*, *};
/// assert_eq!(Ins::from("add rax, [rsi - 16]"), add(RAX.into(), Reference::from(RSI - 16)))
/// ```
#[derive(PartialEq, Debug, Clone)]
pub enum Reference {
    Register(Register),
    Relative(Relative),
    Const(i64),
}

/// An x86 register
///
/// See [X86 Assembly/X86 Architecture][docs] for docs.
///
/// [docs]: https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Register {
    /// Accumulator (AX)
    // Used in arithmetic operations and returning values from functions.
    RAX,
    /// Base Register (BX)
    RBX,
    /// Counter register (CX)
    RCX,
    /// Data register (DX)
    RDX,
    /// Stack Pointer (SP)
    RSP,
    /// Stack Base Pointer (BP)
    RBP,
    /// Source Index register
    RSI,
    /// Destination Index register
    RDI,
    /// New 8 64-bit registers (R8 - R15)
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

/// Registers for argument passing
pub const SYS_V: [Register; 6] =
    [Register::RDI, Register::RSI, Register::RDX, Register::RCX, Register::R8, Register::R9];

/// Relative addressing modes for memory access
///
/// ```
/// # use inc::x86::{self, Register::*, *};
/// assert_eq!("[rsi - 16]", (RSI - 16).to_string());
///
/// assert_eq!("[rsi + 16]", (RSI + 16).to_string());
///
/// assert_eq!("[rbx]", (RBX + 0 ).to_string());
/// ```

#[derive(PartialEq, Debug, Clone)]
pub struct Relative {
    pub register: Register,
    pub offset: i64,
}

// ¶ Codegen functions

/// Add `v` to register `r`
pub fn add(r: Reference, v: Reference) -> Ins {
    Ins(format!("add {}, {}", r, v))
}

/// Logical and of `v` to register `r`
pub fn and(r: Reference, v: Reference) -> Ins {
    Ins(format!("and {}, {}", r, v))
}

/// Unconditional function call
pub fn call(f: &str) -> Ins {
    Ins(format!("call \"{}\"", f))
}

/// Compares the first source operand with the second source operand and sets
/// the status flags in the EFLAGS register.
// The comparison is performed by subtracting the second operand from the first
// operand and then setting the status flags in the same manner as the SUB
// instruction. When an immediate value is used as an operand, it is
// sign-extended to the length of the first operand. The condition codes used by
// the Jcc, CMOVcc, and SETcc instructions are based on the results of a CMP
// instruction.
pub fn cmp(a: Reference, b: Reference) -> Ins {
    Ins(format!("cmp {}, {}", a, b))
}

/// x86 function preamble
pub fn enter() -> ASM {
    Ins::from("push rbp") + Ins::from("mov rbp, rsp")
}

/// Jump to the specified label if last comparison resulted in equality
pub fn je(l: &str) -> Ins {
    Ins(format!("je {}", l))
}

/// Unconditionally jump to the specified label
pub fn jmp(l: &str) -> Ins {
    Ins(format!("jmp {}", l))
}

/// A label is a target to jump to
pub fn label(l: &str) -> Ins {
    Ins(format!("\"{}\":", l))
}

/// Exit a function and clean up. See `Enter`
pub fn leave() -> ASM {
    Ins::from("pop rbp") + Ins::from("ret")
}

/// Load effective address `of` a label into register `r` with an `offset`
pub fn lea(r: Register, of: &str, offset: i64) -> Ins {
    Ins(format!("lea {}, [rip + {} + {}]", r, offset, of))
}

/// Load a value at stack index `si` to register `r`
pub fn load(r: Register, si: i64) -> Ins {
    Ins(format!("mov {}, {}", r, Register::RBP + si))
}

/// Mov! At least one of the operands must be a register, moving from
/// RAM to RAM isn't a valid op.
pub fn mov(to: Reference, from: Reference) -> Ins {
    match (&to, &from) {
        (Reference::Register(_), _) => Ins(format!("mov {}, {}", to, from)),
        _ => Ins(format!("mov qword ptr {}, {}", to, from)),
    }
}

/// Multiply register AX with value `v` and move result to register RAX
// The destination operand is of `mul` is an implied operand located in register
// AX. GCC throws `Error: ambiguous operand size for `mul'` without size
// quantifier
pub fn mul(v: Reference) -> Ins {
    Ins(format!("mul qword ptr {}", v))
}

/// Logical or of `v` to register `r`
pub fn or(r: Reference, v: Reference) -> Ins {
    Ins(format!("or {}, {}", r, v))
}

/// Pop a register `r` from stack
pub fn pop(r: Reference) -> Ins {
    Ins(format!("pop {}", r))
}

/// Push a register `r` to stack
pub fn push(r: Reference) -> Ins {
    Ins(format!("push {}", r))
}

/// Return from the calling function
pub fn ret() -> Ins {
    Ins::from("ret")
}

/// Save a reference `r` to stack at index `si`.
pub fn save(r: Reference, si: i64) -> Ins {
    mov((Register::RBP + si).into(), r)
}

// Shift Operations fall into `arithmetic` (`SAR` & `SAL`) and `logical`
// (`SHR` & `SHL`) types and they differ in the way signs are preserved.
//
// Shifting left works the same for both because multiplying by 2^n wont
// change the sign, but logical right shifting a negative number with
// `SHR` will throw away the sign while `SAR` will preserve it. Prior
// versions of this compiler and paper used both, but unless there is a
// very good reason use shift arithmetic right (`SAR`) instead of shift
// logical right (`SHR`) everywhere.

/// Shift register `r` left by `v` bits; `r = r * 2^v`
pub fn sal(r: Reference, v: Reference) -> Ins {
    Ins(format!("sal {}, {}", r, v))
}

/// Shift register `r` right by `v` bits; `r = r / 2^v`
pub fn sar(r: Reference, v: Reference) -> Ins {
    Ins(format!("sar {}, {}", r, v))
}

/// Sub `k` from register `r`
pub fn sub(r: Reference, v: Reference) -> Ins {
    Ins(format!("sub {}, {}", r, v))
}

/// The base address of the heap is passed in RDI and we reserve reg R12 for it.
pub fn init_heap() -> ASM {
    Ins::from("# Store heap index to R12") + Ins::from("mov r12, rdi")
}

/// Init is the target called from C.
#[cfg(target_os = "macos")]
pub fn init() -> String {
    String::from("_init")
}

#[cfg(target_os = "linux")]
pub fn init() -> String {
    String::from("init")
}

/// Emit code for a function header
#[cfg(target_os = "macos")]
pub fn func(name: &str) -> ASM {
    Ins::from("") + Ins(format!(".globl \"{}\"", &name)) + label(name)
}

#[cfg(target_os = "linux")]
pub fn func(name: &str) -> ASM {
    Ins::from("")
        + Ins(format!(".globl \"{}\"", &name))
        + Ins(format!(".type \"{}\", @function", &name))
        + label(name)
}

/// Prelude at the start of generated ASM
#[cfg(target_os = "macos")]
pub fn prelude() -> ASM {
    Ins::from(".section __TEXT,__text") + Ins::from(".intel_syntax noprefix")
}

#[cfg(target_os = "linux")]
pub fn prelude() -> ASM {
    Ins::from(".text") + Ins::from(".intel_syntax noprefix")
}

// ¶ Trait implementations

impl Add<i64> for Register {
    type Output = Relative;

    fn add(self, offset: i64) -> Relative {
        Relative { register: self, offset }
    }
}

impl Sub<i64> for Register {
    type Output = Relative;

    fn sub(self, offset: i64) -> Relative {
        Relative { register: self, offset: -offset }
    }
}

impl From<&str> for Ins {
    fn from(s: &str) -> Self {
        Ins(s.to_string())
    }
}

/// Concat Ins to get ASM; `asm = op + op`
impl Add<Ins> for Ins {
    type Output = ASM;

    fn add(self, op: Ins) -> ASM {
        ASM { 0: vec![self, op] }
    }
}

impl Add<ASM> for Ins {
    type Output = ASM;

    fn add(self, mut asm: ASM) -> ASM {
        let mut v = vec![self];
        v.append(&mut asm.0);
        ASM { 0: v }
    }
}

/// Add operations with a easy to read `asm += op` short hand.
///
/// This is pretty efficient at the cost of owning the value.
impl AddAssign<Ins> for ASM {
    fn add_assign(&mut self, op: Ins) {
        self.0.push(op)
    }
}

/// Syntax sugar for concatenating two ASM objects Ex: `asm += asm`
impl AddAssign<ASM> for ASM {
    fn add_assign(&mut self, asm: ASM) {
        self.0.extend(asm.0)
    }
}

/// Add operations to ASM with overloaded `asm' = asm + op`.
///
/// NOTE: This is pretty inefficient due to copying of self.
impl Add<Ins> for ASM {
    type Output = Self;

    fn add(mut self, op: Ins) -> Self {
        self.0.push(op);
        self
    }
}

/// Concat ASM; `asm + asm`
///
/// NOTE: This is pretty inefficient due to copying both arguments.
impl Add<ASM> for ASM {
    type Output = Self;

    fn add(mut self, mut asm: ASM) -> Self {
        self.0.append(&mut asm.0);
        self
    }
}

/// Convert a single operation to ASM
impl From<Ins> for ASM {
    fn from(op: Ins) -> Self {
        ASM { 0: vec![op] }
    }
}

impl From<Register> for Reference {
    fn from(r: Register) -> Self {
        Reference::Register(r)
    }
}

impl From<Relative> for Reference {
    fn from(r: Relative) -> Self {
        Reference::Relative(r)
    }
}

impl From<i64> for Reference {
    fn from(i: i64) -> Self {
        Reference::Const(i)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl fmt::Display for Relative {
    #[allow(clippy::comparison_chain)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.offset < 0 {
            write!(f, "{}", format!("[{} - {}]", self.register, -self.offset))
        } else if self.offset > 0 {
            write!(f, "{}", format!("[{} + {}]", self.register, self.offset))
        } else {
            write!(f, "{}", format!("[{}]", self.register))
        }
    }
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reference::Register(r) => write!(f, "{}", r),
            Reference::Relative(r) => write!(f, "{}", r),
            Reference::Const(i) => write!(f, "{}", i),
        }
    }
}

/// `Display` trait converts `ASM` to valid x86 assembly that can be compiled
/// and executed.
///
/// For now this is pretty dumb, but over time this could be made into something
/// a lot smarter and safe rather than concatenating so many tiny strings
/// together.
impl fmt::Display for ASM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut ctx = String::new();
        let mut comment: Option<&Ins> = None;

        for op in &self.0 {
            // Comments must be appended to the next instruction
            if op.0.starts_with('#') {
                comment = Some(op);
                continue;
            }

            // Indent every line except labels by 4 spaces
            if op.0.ends_with(':') {
                ctx.push_str(&format!("{}\n", &op.0));
            } else {
                match comment {
                    Some(s) => {
                        ctx.push_str(&format!("    {:32}{}\n", &op.0, s.0));
                        comment = None
                    }
                    None => ctx.push_str(&format!("    {}\n", &op.0)),
                }
            }
        }
        write!(f, "{}", ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::{Ins, Reference, Register::*};
    use pretty_assertions::assert_eq;

    #[test]
    fn mov() {
        assert_eq!(Ins::from("mov rax, 16"), super::mov(RAX.into(), 16.into()));
        assert_eq!(
            Ins::from("mov qword ptr [rbp + 8], 16"),
            super::mov(Reference::from(RBP + 8), 16.into())
        )
    }
}

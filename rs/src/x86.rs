//! A thin wrapper around x86 assembly.
//!
//! A general purpose x86 library without importing anything else from the rest
//! of the compiler.
//!
//! # Getting started
//!
//! The easiest way to learn assembly is to write very simple C programs and
//! look at the generated code. Clang and GCC support `-S` flag to output the
//! assembly instead of a binary executable.
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
use std::fmt;
use std::ops::{Add, AddAssign};

/// Word size of the architecture
pub const WORDSIZE: i64 = 8;

/// ASM represents a list of instructions
#[derive(Clone)]
pub struct ASM(pub Vec<Ins>);

/// An x86 register
///
/// See [X86 Assembly/X86
/// Architecture](https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture)
/// for docs.
#[derive(Debug, PartialEq, Clone)]
pub enum Register {
    /// Accumulator (AX)
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
}

/// Operand is a register, address or a constant; the argument to instructions
/// that work with memory references
///
/// Mirrors the equivalent LLVM construct. This is an *extremely* simplified
/// view of the reality; `mov` alone with the address access semantics x86
/// supports is Turning Complete. https://esolangs.org/wiki/Mov
#[derive(Debug, Clone)]
pub enum Operand {
    Const(i64),
    Reg(Register),
    Stack(i64),
}

/// Each x86 instruction this compiler understands
///
/// This type fundamentally limits what code can be generated and ideally no
/// other part of the compiler should generate ASM with strings.
#[derive(Debug, Clone)]
pub enum Ins {
    /// Add `v` to register `r`
    Add { r: Register, v: Operand },

    /// Logical and of `v` to register `r`
    And { r: Register, v: Operand },

    /// Unconditional function call
    Call(String),

    /// Compare the value to register RAX
    Cmp { r: Register, with: Operand },

    /// x86 function preamble
    ///
    /// Not really a single instruction but having this as a single
    /// operation makes it easier for callers of this module.
    Enter,

    /// Jump to the specified label if last comparison resulted in equality
    Je(String),

    /// Unconditionally jump to the specified label
    Jmp(String),

    /// A label is a target to jump to
    Label(String),

    /// Exit a function and clean up. See `Enter`
    Leave,

    /// Save a register `r` to stack at index `si`
    Save { r: Register, si: i64 },

    /// Load a value at stack index `si` to register `r`
    Load { r: Register, si: i64 },

    /// Mov! At least one of the operands must be a register, moving from
    /// RAM to RAM isn't a valid op.
    Mov { from: Operand, to: Operand },

    /// Multiply register AX with value `v` and move result to register RAX
    // The destination operand is of `mul` is an implied operand located in
    // register AX. GCC throws `Error: ambiguous operand size for `mul'`
    // without size quantifier
    Mul { v: Operand },

    /// Pop a register `r` from stack
    Pop(Register),

    /// Push a register `r` to stack
    Push(Register),

    /// Return from the calling function
    Ret,

    // Shift Operations fall into `arithmetic` (`SAR` & `SAL`) and `logical`
    // (`SHR` & `SHL`) types and they differ in the way signs are preserved.
    //
    // Shifting left works the same for both because multiplying by 2^n wont
    // change the sign, but logical right shifting a negative number with
    // `SHR` will throw away the sign while `SAR` will preserve it. Prior
    // versions of this compiler and paper used both, but unless there is a
    // very good reason use shift arithmetic right (`SAR`) instead of shift
    // logical right (`SHR`) everywhere.
    /// Shift register `r` right by `v` bits; `r = r / 2^v`
    Sar { r: Register, v: i64 },

    /// Shift register `r` left by `v` bits; `r = r * 2^v`
    Sal { r: Register, v: i64 },

    /// Sub `k` from register `r`
    Sub { r: Register, v: Operand },

    /// Raw slices for compatibility
    ///
    /// Often it can be just convenient to hand write some assembly and
    /// eventually port it to a sensible type here. Till then this Variant
    /// is a goods stop gap.
    Slice(String),
}

/// Convert a single operation to ASM
impl From<Ins> for ASM {
    fn from(op: Ins) -> Self {
        ASM { 0: vec![op] }
    }
}

/// Convert a String to ASM
impl From<String> for ASM {
    fn from(s: String) -> Self {
        ASM { 0: vec![Ins::Slice(s)] }
    }
}

/// Convert a string literal to ASM
impl From<&str> for ASM {
    fn from(s: &str) -> Self {
        ASM { 0: vec![Ins::Slice(s.to_string())] }
    }
}

/// Display for a register is the same as Debug
impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

/// Display an Operand
impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Operand::Const(i) => write!(f, "{}", i),
            Operand::Reg(r) => write!(f, "{}", r),
            Operand::Stack(si) => writeln!(f, "{}", &stack(*si)),
        }
    }
}

/// Pretty print a single ASM instruction.
impl fmt::Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ins::Add { r, v } => writeln!(f, "    add {}, {}", r, v),
            Ins::And { r, v } => writeln!(f, "    and {}, {}", r, v),
            Ins::Call(l) => writeln!(f, "    call {}", l),
            Ins::Cmp { r, with } => writeln!(f, "    cmp {}, {}", r, with),
            Ins::Enter => {
                let op = Ins::Push(Register::RBP)
                    + Ins::Mov {
                        from: Operand::Reg(Register::RSP),
                        to: Operand::Reg(Register::RBP),
                    };
                write!(f, "{}", op)
            }
            Ins::Je(l) => writeln!(f, "    je {}", label(l)),
            Ins::Jmp(l) => writeln!(f, "    jmp {}", label(l)),
            Ins::Label(l) => writeln!(f, "{}:", label(l)),
            Ins::Leave => {
                let op = Ins::Pop(Register::RBP) + Ins::Ret;
                writeln!(f, "{}", op)
            }
            Ins::Load { r, si } => {
                writeln!(f, "    mov {}, {}", r, &stack(*si))
            }
            Ins::Mov { from, to } => writeln!(f, "    mov {}, {}", to, from),
            Ins::Mul { v } => writeln!(f, "    mul qword ptr {}", v),
            Ins::Pop(r) => writeln!(f, "    pop {}", r),
            Ins::Push(r) => writeln!(f, "    push {}", r),
            Ins::Ret => writeln!(f, "    ret"),
            Ins::Save { r, si } => {
                writeln!(f, "    mov {}, {}", &stack(*si), r)
            }
            Ins::Sal { r, v } => writeln!(f, "    sal {}, {}", r, v),
            Ins::Sar { r, v } => writeln!(f, "    sar {}, {}", r, v),
            Ins::Sub { r, v } => writeln!(f, "    sub {}, {}", r, v),
            Ins::Slice(s) => write!(f, "{}", s),
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
        for op in self.0.iter() {
            ctx.push_str(&op.to_string());
        }
        writeln!(f, "{}", ctx)
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
        self.0.extend(asm.0.clone())
    }
}

/// Add operations to ASM with overloaded `asm' = asm + op`.
///
/// NOTE: This is pretty inefficient due to copying of self.
impl Add<Ins> for ASM {
    type Output = Self;

    fn add(self, op: Ins) -> Self {
        let mut t = self.clone();
        t.0.push(op);
        t
    }
}

/// Concat ASM; `asm + asm`
///
/// NOTE: This is pretty inefficient due to copying both arguments.
impl Add<ASM> for ASM {
    type Output = Self;

    fn add(self, asm: ASM) -> Self {
        let mut rhs = self.clone();
        let mut lhs = asm.0.clone();
        rhs.0.append(&mut lhs);
        rhs
    }
}

/// Concat Ins to get ASM; `asm = op + op`
///
/// NOTE: This is pretty inefficient due to copying both arguments.
impl Add<Ins> for Ins {
    type Output = ASM;

    fn add(self, op: Ins) -> ASM {
        ASM { 0: vec![self, op] }
    }
}

// ¶ Module helpers

/// Label is a target to jump to
#[cfg(target_os = "macos")]
fn label(label: &str) -> String {
    format!("_{}", label)
}

#[cfg(target_os = "linux")]
fn label(label: &str) -> String {
    format!("{}", label)
}

/// Stack address relative to base pointer
pub fn stack(si: i64) -> String {
    match si {
        index if index > 0 => format!("[rbp + {}]", index),
        index if index < 0 => format!("[rbp - {}]", (-index)),
        _ => panic!("Effective stack index cannot be 0"),
    }
}

#[cfg(target_os = "macos")]
pub fn function_header(name: &str) -> ASM {
    let mut ctx = String::new();

    ctx.push_str("    .section __TEXT,__text\n");
    ctx.push_str("    .intel_syntax noprefix\n");
    ctx.push_str(&format!("    .globl _{}\n", &name));
    ctx.push_str(&Ins::Label(String::from(name)).to_string());
    ctx.into()
}

#[cfg(target_os = "linux")]
pub fn function_header(name: &str) -> ASM {
    let mut ctx = String::new();

    ctx.push_str("    .text\n");
    ctx.push_str("    .intel_syntax noprefix\n");
    ctx.push_str(&format!("    .globl {}\n", &name));
    ctx.push_str(&format!("    .type {}, @function\n", &name));
    ctx.push_str(&Ins::Label(String::from(name)).to_string());
    ctx.into()
}

#[cfg(test)]
mod tests {
    use crate::x86::{Ins::*, Operand::*, Register::*};

    #[test]
    fn mov() {
        assert_eq!(
            String::from("    mov rax, 16\n"),
            Mov { from: Const(16), to: Reg(RAX) }.to_string()
        );
    }
}

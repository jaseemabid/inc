//! A string is a blob of UTF-8 encoded bytes prefixed with the length if it.
//!
//! Strings can be stack or heap allocated but static strings found in the
//! source code is retained as it is in the data section.
//!
//! Example memory layout:
//!
//! A literal "hello world" gets statically allocated at offset 4000 along with
//! the length. There is no extra allocation required for the string object
//! after immediate tagging the address as 4005 `(4000 | strtag)`.
//!
//! ```txt
//!  -------------------------
//! | Address | Value         |
//!  -------------------------
//! | 4000    | 11            |
//! | 4000    | "hello world" |
//! |         |               |
//! | 8000    | 4005          |
//!  -------------------------
//! ```
//!
//! The C runtime would get the value 4005 and would identify it as a string
//! with the tag `(8005 & mask == strtag)`. The raw pointer is obtained by
//! removing the tag `(p = val - strtag)` and length is found at the base
//! addresses `(*p)` and the data at `(*p + 1)`. fwrite can safely print the
//! exact number of bytes using the length and pointer.
//!
//! TODO: Consider switching to SDS. https://github.com/antirez/sds

use crate::{
    compiler::state::State,
    immediate,
    x86::{
        self, Ins, Reference,
        Register::{R12, RAX},
        ASM,
    },
};

/// Evaluate a string object
pub fn eval(s: &State, data: &str) -> ASM {
    let index = s
        .symbols
        .get(data)
        .unwrap_or_else(|| panic!("String `{}` not found in symbol table", data));

    x86::lea(RAX, &label(*index), immediate::STR).into()
}

/// Inline static strings in source directly into the binary
pub fn inline(s: &State) -> ASM {
    let mut asm = ASM(vec![]);

    for (symbol, index) in &s.symbols {
        // `.p2align 3` aligns the address of the following target to 8
        // bytes by setting the 3 low order bits to 0. This is necessary for
        // the immediate tagging scheme to work correctly.
        //
        // https://sourceware.org/binutils/docs-2.32/as/P2align.html
        asm += Ins::from("");
        asm += Ins::from(".p2align 3");
        asm += x86::label(&label(*index));
        asm += Ins(format!(".quad  {}", symbol.len()));
        asm += Ins(format!(".ascii \"{}\"", symbol))
    }

    asm
}

/// Label for inlining symbol
fn label(index: usize) -> String {
    format!("inc_str_{}", index)
}

/// Allocate a string object in heap with a specific size
#[allow(clippy::identity_op)]
pub fn make(_: &State, size: i64) -> ASM {
    let aligned = ((size as i64 + 7) / 8) * 8;

    x86::mov(Reference::from(R12 + 0), size.into())
        + x86::mov(RAX.into(), R12.into())
        + x86::or(RAX.into(), immediate::STR.into())
        + x86::add(R12.into(), aligned.into())
}

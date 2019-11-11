//! A symbol is a blob of UTF-8 encoded bytes prefixed with the length and an
//! unique identifier.
//!
//! Specification: https://www.scheme.com/tspl4/objects.html#./objects:h11
//!
//! See strings module for more docs since these modules are very similar.
//!
//! Example memory layout:
//!
//! A literal "hello" gets statically allocated at offset 4000 along with an ID
//! 7 and the length 5.
//!
//! ```txt
//!  -----------------
//! | Address | Value |
//!  -----------------
//! | 4000    | 7     |
//! | 4000    | 5     |
//! | 4000    | hello |
//! |         |       |
//! | 8000    | 4005  |
//!  -----------------
//! ```

use crate::{
    compiler::state::State,
    immediate,
    x86::{self, Ins, Register::RAX, ASM},
};

/// Evaluate a symbols object
pub fn eval(s: &State, data: &str) -> ASM {
    let index = s
        .symbols
        .get(data)
        .unwrap_or_else(|| panic!("Symbol `{}` not found in symbol table", data));

    x86::lea(RAX, &label(*index), immediate::SYM).into()
}

/// Inline static symbols in source directly into the binary
pub fn inline(s: &State) -> ASM {
    let mut asm = ASM(vec![]);

    for (symbol, index) in &s.symbols {
        asm += Ins::from("");
        asm += Ins::from(".p2align 3");
        asm += x86::label(&label(*index));
        asm += Ins(format!(".quad  {}", index));
        asm += Ins(format!(".quad  {}", symbol.len()));
        asm += Ins(format!(".asciz \"{}\"", symbol))
    }

    asm
}

/// Label for inlining symbol
fn label(index: usize) -> String {
    format!("inc_sym_{}", index)
}

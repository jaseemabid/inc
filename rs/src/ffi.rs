//! Foreign Function Interface for Inc
//!
//! This module enable Scheme functions to call into Rust/C functions
//! seamlessly.
//!
//! Foreign functions are called using "System V AMD64 ABI" calling convention,
//! which passes all arguments in stack and expects return value in RAX
//! register. See [x86 module documentation](crate::x86) for more details.
//!
//! This [guide to linux syscalls][guide] provides a lot of background on the
//! subject.
//!
//! [guide]: https://blog.packagecloud.io/eng/2016/04/05/the-definitive-guide-to-linux-system-calls

use crate::{
    compiler::{emit::eval, state::State},
    core::Core,
    core::Ident,
    immediate,
    x86::{self, Reference::*, Register::*, ASM, WORDSIZE},
};

/// Call a foreign function defined in Rust/C
pub fn call(s: &mut State, name: &Ident, args: &[Core]) -> ASM {
    let mut asm = ASM(vec![]);

    if args.len() > 6 {
        panic!("foreign function {} called with more than 6 arguments: {:?}", &name, args)
    }

    for (i, arg) in args.iter().enumerate() {
        let target = x86::SYS_V[i];

        asm += match immediate::to(arg) {
            Some(c) => x86::mov(Register(target), Const(c)).into(),
            None => eval(s, &arg) + x86::mov(Register(target), Register(RAX)),
        }
    }

    // Translate scheme names into runtime names
    // 1. On macos, function names must be prefixed an underscore like _init
    // 2. Replace =? into _eq (symbol=? -> symbol_eq)
    #[cfg(target_os = "linux")]
    fn rename(name: &str) -> String {
        name.replace("-", "_").replace("=?", "_eq")
    }

    #[cfg(target_os = "macos")]
    fn rename(name: &str) -> String {
        format!("_{}", name.replace("-", "_").replace("=?", "_eq"))
    }

    let name = rename(&name.mangle());

    // See docs in `lambda:call` for details on how this works.
    if s.si != -WORDSIZE {
        asm += x86::sub(RSP.into(), Const(-s.si));
        asm += x86::call(&name);
        asm += x86::add(RSP.into(), Const(-s.si));
    } else {
        asm += x86::call(&name)
    }

    asm
}

//! Runtime functions implemented in C or ASM
use crate::{
    compiler::{emit::eval, state::State},
    core::Expr,
    x86::{
        self,
        Reference::*,
        Register::{self, *},
        ASM, WORDSIZE,
    },
};

const SYMBOLS: [&str; 3] = ["string-length", "symbol=?", "exit"];

/// Call a function with System V calling convention
///
/// See x86 module docs for details. Probably a very good idea to read this
/// [guide to linux syscalls][guide] twice before jumping into this.
///
/// [guide]: https://blog.packagecloud.io/eng/2016/04/05/the-definitive-guide-to-linux-system-calls
pub fn ffi(s: &mut State, name: &str, args: &[Expr]) -> ASM {
    let mut asm = ASM(vec![]);

    if args.len() > 6 {
        panic!("foreign function {} called with more than 6 arguments: {:?}", &name, args)
    }

    for (i, arg) in args.iter().enumerate() {
        let target: Register = x86::SYS_V[i];

        asm += match arg.unbox() {
            Some(c) => x86::mov(Register(target), Const(c)).into(),
            None => eval(s, &arg) + x86::mov(Register(target), Register(RAX)),
        }
    }

    let name = rename(name);

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

pub fn contains(name: &str) -> bool {
    SYMBOLS.contains(&name)
}

/// Translate scheme names into runtime names
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

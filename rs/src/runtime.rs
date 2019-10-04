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

const SYMBOLS: [&str; 2] = ["string-length", "exit"];

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

    let name = name.replace("-", "_");

    for (i, arg) in args.iter().enumerate() {
        let target: Register = x86::SYS_V[i];

        asm += match arg.unbox() {
            Some(c) => x86::mov(Register(target), Const(c)).into(),
            None => eval(s, &arg) + x86::mov(Register(target), Register(RAX)),
        }
    }

    // Extend stack to hold the current local variables before creating a
    // new frame for the function call. `si` is the next available empty
    // slot, `(+ si wordsize)` is the current usage. Add this to `RSP` to
    // reserve this space before the function gets called. Not doing this
    // will result in the called function to override this space with its
    // local variables and corrupt the stack.
    let locals = s.si + WORDSIZE;
    if locals != 0 {
        asm += x86::add(RSP.into(), locals.into());
        asm += x86::call(&name);
        asm += x86::sub(RSP.into(), locals.into());
    } else {
        asm += x86::call(&name)
    }

    asm
}

pub fn contains(name: &str) -> bool {
    SYMBOLS.contains(&name)
}

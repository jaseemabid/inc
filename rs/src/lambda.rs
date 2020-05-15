//! Scheme functions
//!
//! Scheme lambdas are heavily overloaded and this is one of the most
//! sophisticated parts of this compiler.
//!
//! A scheme lambda creates a callable function and a heap allocated closure
//! object that captures the free variables. This module also implements a
//! calling convention - which is a set of rules agreed by the caller of a
//! function and its definition regarding how the arguments are passed in and
//! how a result is returned.
//!
//! Closure conversion aims to break down scheme lambdas into something simpler
//! for code generation and is a well known technique in several functional
//! compilers. All lambdas are lifted to top level with a unique name and an
//! explicit closure object is passed as the first argument which captures the
//! environment in which the function was defined.
//!
//! See [Closure conversion: How to compile lambda][cc] for a detailed
//! explanation.
//!
//! [cc]: http://matt.might.net/articles/closure-conversion/
//!
//! The paper uses a calling convention that passes all arguments in stack. This
//! is harder to implement than the default calling calling convention used by
//! GCC on x86-64 - System V AMD64 ABI, in which arguments are passed in the
//! registers RDI, RSI, RDX, RCX, R8, R9 and the return value is passed back in
//! RAX.
//!
//! ⚠ This module implements the stack version for now, but must be migrated to
//! SysV at some point.
use crate::{
    compiler::{emit::eval, state::State},
    core::{Code, Expr, Ident},
    x86::{self, Reference, Register::*, Relative, ASM, WORDSIZE},
};

/// Emit machine code for all top level functions
pub fn emit(s: &mut State, exprs: &[Expr]) -> ASM {
    let mut asm = ASM(vec![]);

    for expr in exprs.iter() {
        if let Expr::Define { name, val: box Expr::Lambda(c) } = expr {
            asm += emit1(s, &name, c)
        }
    }
    asm
}

/// Emit unction body for the simplest C style functions
///
/// ⚠ A lot of required sanity and safety checks are missing.
///
/// The calling convention expected by the function is kind of odd and needs to
/// be standardized. Arguments are pushed to stack in order (unlike cdecl, which
/// pushes in reverse order). System V AMD64 ABI would be perfect since all args
/// are passed in registers and is a lot cleaner and is already used for
/// `init()`
///
/// The caller of the function emits arguments at `RSP - 24`, then `RSP - 32`
/// etc. The function preamble effectively decrements the base pointer by `0x10`
/// such that the such that the first argument can be accessed at `RBP - 8`, the
/// next one at `RBP - 16` etc.
fn emit1(s: &mut State, name: &Ident, code: &Code) -> ASM {
    let mut asm = ASM(vec![]);

    asm += x86::func(&name.to_string());

    // Start a new lexical environment for the function, add the formal
    // arguments and leave when it is evaluated. The first argument is available
    // at `RBP - 8`, next at `RBP - 16` etc.
    //
    // TODO: `alloc()` and `dealloc()` doesn't understand `enter()` and
    // `leave()`, so there is a fair bit of duplication here.
    s.enter();

    for (i, arg) in code.formals.iter().enumerate() {
        s.set(
            Ident::new(arg),
            Relative { register: RBP, offset: -(i as i64 + 1) * WORDSIZE }.into(),
        );
    }

    for b in &code.body {
        asm += x86::enter();
        asm += eval(s, &b);
        asm += x86::leave()
    }

    s.leave();

    asm
}

/// Emit code for a function application. See `code` for details.
pub fn call(s: &mut State, name: &Ident, args: &[Expr]) -> ASM {
    // Evaluate and push the arguments into stack; 2 words below SI. See
    // `code` docs for a detailed description of how this works.
    //
    // si  832 -> ...
    //     816 -> ...
    //     808 -> arg 1
    //     800 -> arg 2
    let mut asm = ASM(vec![]);

    // The original stack index - used for remembering index after recursive
    // calls. This wouldn't be required if we could pass an immutable copy into
    // recursive calls of eval.
    let si = s.si;

    // Lack of persistent state makes this code fairly difficult to understand
    // and this is a whole lot more complex than it looks like. The recursive
    // definition in scheme with persistent `s` is significantly cleaner.
    for (i, arg) in args.iter().enumerate() {
        s.si = si - ((i as i64 + 2) * WORDSIZE);
        asm += eval(s, arg);
        asm += x86::save(RAX.into(), s.si);
    }

    // Set stack index back to where it used to be after evaluating all args
    s.si = si;

    // Extend the current stack frame to hold the local variables before
    // creating a new one. The called function might clobber the stack
    // corrupting local variables in previous scopes.
    //
    // This is undeniably brittle and errors here can be hard to understand and
    // debug. The behavior varies when the arguments are passed in stack for
    // calling scheme functions and when calling runtime functions defined in C
    // which expects args in registers.
    //
    // In case of stack calls, the aliment must be perfect or the called
    // function would access the wrong local variables. For register calls, the
    // stack only have to be bigger than a minimum size.
    //
    // In a very curious case, macOS segfaults when the stack is grown by 8
    // bytes and I really cannot tell why because of the complete lack of
    // debuggers - both GDB and valgrind. Allocating at least 16bytes seems to
    // work on both targets.
    let locals = -(s.si + WORDSIZE);
    if locals != 0 {
        asm += x86::sub(RSP.into(), Reference::Const(locals));
        asm += x86::call(&name.to_string());
        asm += x86::add(RSP.into(), Reference::Const(locals));
    } else {
        asm += x86::call(&name.to_string())
    }

    // NOTE: This is one of those big aha moments.
    //
    // There is no need to reclaim/cleanup space used for function arguments
    // because the memory would just get overridden by next variable allocation.
    // This makes keeping track of that stack index unnecessary and life so much
    // simpler.
    asm
}

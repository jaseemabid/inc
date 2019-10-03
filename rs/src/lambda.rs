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
    core::{
        Code,
        Expr::{self, *},
    },
    x86::{self, Register::*, Relative, ASM, WORDSIZE},
};

/// Scan through the source and lift lambdas into top level.
//
// TODO:
// 1. Ensure labels are unique
//
pub fn lift(s: &mut State, prog: &Vec<Expr>) -> (Vec<Code>, Vec<Expr>) {
    let mut codes: Vec<Code> = vec![];
    let mut lifted: Vec<Expr> = Default::default();

    for expr in prog {
        let (c, e) = lift1(s, &expr);

        codes.extend(c);
        lifted.push(e);
    }

    (codes, lifted)
}

/// Lift an expression to top level
///
/// Lift returns a list of definitions and an expression with inline lambdas and
/// function definitions replaced with unique names.
fn lift1(s: &mut State, prog: &Expr) -> (Vec<Code>, Expr) {
    // Codes are all the function bodies found within this expression
    let mut codes: Vec<Code> = vec![];

    // Original expression with inline lambdas and declarations replaced
    let lifted: Expr = match prog {
        Let { bindings, body } => {
            // Rest is all the name bindings that are not functions
            let mut rest: Vec<(String, Expr)> = vec![];

            for (name, value) in bindings {
                match value {
                    Lambda(Code { formals, free, body, .. }) => {
                        s.functions.insert(name.to_string());
                        codes.push(Code {
                            name: Some(name.to_string()),
                            formals: formals.to_vec(),
                            free: free.to_vec(),
                            body: body.to_vec(),
                        });
                    }

                    _ => rest.push((name.clone(), value.clone())),
                }
            }

            Let { bindings: rest, body: body.to_vec() }
        }

        // A literal lambda must be in an inline calling position
        Lambda(Code { .. }) => unimplemented!("inline λ"),

        _ => prog.clone(),
    };

    (codes, lifted)
}

/// Function body for the simplest C style functions
///
/// A lot of required sanity and safety checks are missing.
///
/// The calling convention expected by the function is kind of odd and needs
/// to be standardized. Arguments are pushed to stack in order (unlike
/// cdecl, which pushes in reverse order). System V AMD64 ABI would be
/// perfect since all args are passed in registers and is a lot cleaner and
/// is already used for `init()`
///
/// The caller of the function emits arguments at `RSP - 24`, then `RSP -
/// 32` etc. The function preamble effectively decrements the base pointer
/// by `0x10` such that the such that the first argument can be accessed at
/// `RBP - 8`, the next one at `RBP - 16` etc.
pub fn code(s: &mut State, codes: Vec<Code>) -> ASM {
    let mut asm = ASM(vec![]);

    for Code { name, formals, body, .. } in &codes {
        asm += x86::func(name.as_ref().unwrap().as_str());

        // Start a new lexical environment for the function, add the
        // formal arguments and leave when it is evaluated. The first
        // argument is available at `RBP - 8`, next at `RBP - 16` etc.
        //
        // TODO: `alloc()` and `dealloc()` doesn't understand `enter()` and
        // `leave()`, so there is a fair bit of duplication here.
        s.enter();

        for (i, arg) in formals.iter().enumerate() {
            s.set(&arg, Relative { register: RBP, offset: -(i as i64 + 1) * WORDSIZE }.into());
        }

        for b in body {
            asm += x86::enter();
            asm += eval(s, b);
            asm += x86::leave()
        }

        s.leave()
    }
    asm
}

/// Emit code for a function application. See `code` for details.
pub fn call(s: &mut State, name: &str, args: &Vec<Expr>) -> ASM {
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

    // Extend stack to hold the current local variables before creating a
    // new frame for the function call. `si` is the next available empty
    // slot, `(+ si wordsize)` is the current usage. Add this to `RSP` to
    // reserve this space before the function gets called. Not doing this
    // will result in the called function to override this space with its
    // local variables and corrupt the stack.
    let locals = s.si + WORDSIZE;
    if locals != 0 {
        asm += x86::add(RSP.into(), locals.into());
        asm += x86::call(name);
        asm += x86::sub(RSP.into(), locals.into());
    } else {
        asm += x86::call(name)
    }

    // NOTE: This is one of those big aha moments.
    //
    // There is no need to reclaim/cleanup space used for function arguments
    // because the memory would just get overridden by next variable allocation.
    // This makes keeping track of that stack index unnecessary and life so much
    // simpler.
    asm
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let prog = r"(let ((id (lambda (x) x))) (id 42))";
        let mut s: State = Default::default();

        let expr = match parser::parse(prog) {
            Ok(r) => r,
            Err(e) => panic!(e),
        };

        let (codes, e) = lift(&mut s, &expr);

        assert_eq!(
            codes,
            vec![Code {
                name: Some("id".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Identifier("x".into())],
            }]
        );

        assert_eq!(
            e[0],
            Let { bindings: vec![], body: vec![List(vec![Identifier("id".into()), Number(42)])] }
        );
    }

    #[test]
    fn recursive() {
        let prog = r"(let ((e (lambda (x) (if (zero? x) #t (o (dec x)))))
                           (o (lambda (x) (if (zero? x) #f (e (dec x))))))
                       (e 25)))";

        let mut s: State = Default::default();

        let expr = match parser::parse(prog) {
            Ok(r) => r,
            Err(e) => panic!(e),
        };

        let (codes, e) = lift(&mut s, &expr);

        assert_eq!(
            codes.get(0).unwrap(),
            &Code {
                name: Some("e".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Cond {
                    pred: Box::new(List(vec![Identifier("zero?".into()), Identifier("x".into())])),
                    then: Box::new(Boolean(true)),
                    alt: Some(Box::new(List(vec![
                        Identifier("o".into()),
                        List(vec![Identifier("dec".into()), Identifier("x".into())])
                    ])))
                }]
            }
        );

        assert_eq!(
            codes.get(1).unwrap(),
            &Code {
                name: Some("o".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Cond {
                    pred: Box::new(List(vec![Identifier("zero?".into()), Identifier("x".into())])),
                    then: Box::new(Boolean(false)),
                    alt: Some(Box::new(List(vec![
                        Identifier("e".into()),
                        List(vec![Identifier("dec".into()), Identifier("x".into())])
                    ])))
                }]
            }
        );

        assert_eq!(
            e[0],
            Let { bindings: vec![], body: vec![List(vec![Identifier("e".into()), Number(25)])] }
        );
    }
}

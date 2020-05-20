//! Entry point for the Inc compiler

/// State for the code generator
pub mod state {
    use crate::core::Ident;
    use crate::x86::{Reference, ASM, WORDSIZE};
    use std::collections::{HashMap, HashSet};

    /// State for the code generator; easier to bundle it all into a struct than
    /// pass several arguments in.
    ///
    /// Stack index points to the current available empty slot. Use and then
    /// decrement the index to add a new variable. Default to `-word size`
    ///
    /// `li` is label index, a counter used to generate unique labels. See
    /// `gen_label`
    ///
    /// `symbols` is a list of all strings known at compile time, so that they
    /// can be allocated in the binary instead of heap.
    ///
    /// `functions` are all user defined functions
    ///
    /// State should also implement some form of register allocation.
    pub struct State {
        pub si: i64,
        pub asm: ASM,
        li: u64,
        pub strings: HashMap<String, usize>,
        pub symbols: HashMap<String, usize>,
        pub functions: HashSet<Ident>,
        env: Env,
    }

    impl Default for State {
        fn default() -> Self {
            State {
                si: -WORDSIZE,
                asm: Default::default(),
                li: 0,
                strings: HashMap::new(),
                symbols: HashMap::new(),
                functions: HashSet::new(),
                env: Default::default(),
            }
        }
    }

    impl State {
        pub fn enter(&mut self) {
            self.env.enter();
        }

        pub fn leave(&mut self) {
            let unwind = self.env.0.first().expect("unexpected empty env").len() as i64 * WORDSIZE;
            self.si += unwind;
            self.env.leave()
        }

        pub fn get(&self, i: &Ident) -> Option<&Reference> {
            self.env.get(i)
        }

        // Set a new binding in the current local environment
        pub fn set(&mut self, i: Ident, r: Reference) {
            self.env.set(i, r);
            self.alloc();
        }

        /// Allocate a word on the stack & return reference to existing empty slot
        ///
        /// Since stack index points to existing free memory, it is useful to be
        /// able to use it and increment in one go.
        ///
        /// # Example:
        ///
        /// ```ignore
        /// Save { r: RAX, si: s.alloc() }
        /// ```
        pub fn alloc(&mut self) -> i64 {
            let current = self.si;
            self.si -= WORDSIZE;
            current
        }

        /// Explicitly free `n` words of memory from stack
        pub fn dealloc(&mut self, count: i64) {
            self.si += count * WORDSIZE;
        }

        /// Generate a unique label for jump targets.
        pub fn gen_label(&mut self, prefix: &str) -> String {
            self.li += 1;
            format!("{}_{}", prefix, self.li)
        }
    }
    // Environment is an *ordered* list of bindings.
    struct Env(Vec<HashMap<Ident, Reference>>);

    impl Default for Env {
        fn default() -> Self {
            Env(vec![HashMap::new()])
        }
    }

    impl Env {
        pub fn enter(&mut self) {
            self.0.insert(0, HashMap::new());
        }

        pub fn leave(&mut self) {
            self.0.remove(0);
        }

        pub fn set(&mut self, i: Ident, r: Reference) {
            self.0.first_mut().map(|binding| binding.insert(i, r));
        }

        pub fn get(&self, i: &Ident) -> Option<&Reference> {
            for bindings in &self.0 {
                if let Some(t) = bindings.get(i) {
                    return Some(t);
                }
            }
            None
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn t() {
            let mut e: Env = Default::default();
            assert_eq!(e.0.len(), 1);

            // default global scope
            e.set(Ident::new("x"), Reference::from(-8));
            assert_eq!(e.get(&Ident::new("x")), Some(&Reference::from(-8)));

            // overwrite in current scope
            e.set(Ident::new("x"), Reference::from(-16));
            assert_eq!(e.get(&Ident::new("x")), Some(&Reference::from(-16)));

            e.enter();
            assert_eq!(e.0.len(), 2);
            // read variables from parent scope
            assert_eq!(e.get(&Ident::new("x")), Some(&Reference::from(-16)));

            e.set(Ident::new("y"), Reference::from(-24));
            // local variable shadows global
            e.set(Ident::new("x"), Reference::from(-32));
            assert_eq!(e.get(&Ident::new("x")), Some(&Reference::from(-32)));

            e.leave();

            assert_eq!(e.0.len(), 1);
            assert_eq!(e.get(&Ident::new("y")), None);
            assert_eq!(e.get(&Ident::new("x")), Some(&Reference::from(-16)));
        }
    }
}

/// Emit machine code for inc AST.
///
/// This module implements bulk of the compiler and is a good place to start
/// reading code. Platform specific code is annotated with `cfg(target_os)` for
/// both linux and mac. This module implements code gen specific to inc and
/// anything generic goes into `x86` module.
pub mod emit {
    use crate::{
        compiler::state::State,
        core::{Core, Expr::*, Ident, Literal::*, Syntax},
        x86::{self, Ins, Reference, Register::*, Relative, ASM},
        *,
    };

    /// Clear (mask) all except the least significant 3 tag bits
    pub fn mask() -> Ins {
        x86::and(RAX.into(), immediate::MASK.into())
    }

    /// Emit code for a let expression
    ///
    /// A new environment is created to hold the bindings, which map the name to
    /// a stack index. All the space allocated by the let expression for local
    /// variables can be freed at the end of the body. This implies the `si`
    /// stays the same before and after a let expression. There is no need to
    /// keep track of the amount of space allocated inside the let expression
    /// and free it afterwards.
    pub fn vars(s: &mut State, vars: &[(Ident, Core)], body: &[Core]) -> ASM {
        let mut asm = ASM(vec![]);

        s.enter();

        for (ident, expr) in vars {
            match immediate::to(expr) {
                Some(c) => asm += x86::save(Reference::Const(c), s.si),
                None => asm += eval(s, expr) + x86::save(RAX.into(), s.si),
            }

            let r = Relative { register: RBP, offset: s.si };
            s.set(ident.clone(), r.into());
        }

        for b in body {
            asm += eval(s, &b);
        }

        s.leave();
        asm
    }

    /// Emit code for a conditional expression
    pub fn cond(s: &mut State, p: &Core, then: &Core, alt: &Option<Box<Core>>) -> ASM {
        let exit_label = s.gen_label("exit");
        let alt_label = s.gen_label("else");

        // A conditional without an explicit alternate should evaluate to '()
        let t = match alt {
            None => &Literal(Nil),
            Some(t) => t,
        };

        eval(s, p)
            + x86::cmp(RAX.into(), immediate::FALSE.into())
            + x86::je(&alt_label)
            + eval(s, then)
            + x86::jmp(&exit_label)
            + x86::label(&alt_label)
            + eval(s, t)
            + x86::label(&exit_label)
    }

    /// Evaluate an expression into RAX
    ///
    /// If the expression fits in a machine word, immediately return with the
    /// immediate repr, recurse for anything else till the base case.
    ///
    // TODO: eval should dispatch based on first atom alone, not necessarily
    // care about arity here. `let` and other variadic syntax forms won't fit
    // into any specific branch here.

    #[allow(clippy::redundant_pattern)]
    pub fn eval(s: &mut State, prog: &Core) -> ASM {
        match prog {
            Identifier(i) => match s.get(&i) {
                Some(index) => x86::mov(RAX.into(), index.clone()).into(),
                None => panic!("Undefined variable {}", i),
            },

            // Find the symbol index and return and reference in RAX
            Literal(Str(data)) => strings::eval(&s, &data),

            Literal(Symbol(data)) => symbols::eval(&s, &data),

            Let { bindings, body } => vars(s, bindings, body),

            Cond { pred, then, alt } => cond(s, pred, then, alt),

            List(list) => match list.as_slice() {
                [Identifier(name), args @ ..] => {
                    if s.functions.contains(&name) {
                        lambda::call(s, &name, &args)
                    } else if let Some(x) = primitives::call(s, &name, args) {
                        x
                    } else if rt::defined(&name) {
                        ffi::call(s, name, &args)
                    } else {
                        panic!("Unknown function {} called with args: {:?}", name, &args)
                    }
                }
                _ => panic!("Unknown expression: `{}`", prog),
            },

            Lambda(_) => ASM(vec![]),

            Define { .. } => ASM(vec![]),

            _ => match immediate::to(&prog) {
                Some(c) => x86::mov(RAX.into(), c.into()).into(),
                None => panic!("Unknown expression: `{}`", prog),
            },
        }
    }

    /// Top level interface to the emit module
    pub fn program(prog: Vec<Syntax>) -> String {
        let mut s: State = Default::default();

        let prog = lang::rename(prog);
        let prog = lang::lift(&mut s, prog);
        let prog = lang::anf(prog);

        let mut gen = x86::prelude() + x86::func(&x86::init()) + x86::enter() + x86::init_heap();

        for b in &prog {
            gen += eval(&mut s, &b);
        }

        gen += x86::leave();
        gen += strings::inline(&s);
        gen += symbols::inline(&s);
        gen += lambda::emit(&mut s, &prog);

        gen.to_string()
    }
}

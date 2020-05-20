//! Scheme language transformations & idiosyncrasies
//!
//! Home for renaming, lifting, type checks and everything else.
use crate::{
    compiler::state::State,
    core::{Expr::*, Literal::*, *},
};

use {std::clone::Clone, std::collections::HashMap};

/// Rename/mangle all references to unique names
///
/// The type change from Expr<String> to Expr<Ident> conveys the basic idea.
/// Names in the source is transformed into unique identifiers with metadata.
///
/// This is a fairly complicated thing to get right and being able to reuse a
/// well tested existing implementation would be great. See [RFC 2603], its
/// [discussion] and [tracking issue] to learn how rustc does this.
///
/// [RFC 2603]: https://github.com/rust-lang/rfcs/blob/master/text/2603-rust-symbol-name-mangling-v0.md
/// [discussion]: https://github.com/rust-lang/rfcs/pull/2603
/// [tracking issue]: https://github.com/rust-lang/rust/issues/60705
///
pub fn rename(prog: Vec<Syntax>) -> Vec<Core> {
    // TODO: This isn't right, same state should be used for all sub expressions.
    // Test for 2 different functions with the same name.

    prog.into_iter().map(|e| rename1(&HashMap::new(), &Ident::empty(), 0, e)).collect()
}

/// Rename a single expression
///
/// Env maps the current environment, base is the identifier for the current
/// environment that is used to derive all new names and index refers to the
/// free index to name anonymous closures and let bindings.
fn rename1(env: &HashMap<&str, Ident>, base: &Ident, index: u8, prog: Syntax) -> Core {
    match prog {
        // If an identifier is defined already, refer to it, otherwise create a
        // new one in the top level environment since its unbound.
        Identifier(s) => match env.get(s.as_str()) {
            Some(n) => Expr::Identifier(n.clone()),
            None => Ident::expr(s),
        },

        Let { bindings, body } => {
            let base = base.extend(format!("{{let {}}}", index));

            // Collect all the names about to be bound for evaluating body
            let mut all = env.clone();
            for (name, _val) in bindings.iter() {
                all.insert(name.as_str(), base.extend(name));
            }

            // A sub expression in let binding is evaluated with the complete
            // environment including the one being defined only if the subexpresison
            // captures the closure with another let or lambda, otherwise evaluate with
            // only the rest of the bindings.
            Let {
                bindings: bindings
                    .iter()
                    .map(|(current, value)| {
                        // Collect all the names excluding the one being defined now
                        let mut rest = env.clone();
                        for (name, _) in bindings.iter() {
                            if name != current {
                                rest.insert(name.as_str(), base.extend(name));
                            }
                        }

                        let value = match value {
                            Let { .. } => rename1(&all, &base, index + 1, value.clone()),
                            Lambda(c) => {
                                let base = base.extend(current);
                                rename1(&all, &base, index + 1, Lambda(c.clone()))
                            }
                            _ => rename1(&rest, &base, index + 1, value.clone()),
                        };

                        let ident = all.get(current.as_str()).unwrap().clone();

                        (ident, value)
                    })
                    .collect(),

                body: body.into_iter().map(|b| rename1(&all, &base, index + 1, b)).collect(),
            }
        }

        List(list) => List(list.into_iter().map(|l| rename1(env, base, index, l)).collect()),

        Cond { pred, then, alt } => Cond {
            pred: box rename1(env, base, index, *pred),
            then: box rename1(env, base, index, *then),
            alt: alt.map(|u| box rename1(env, base, index, *u)),
        },

        Lambda(Closure { formals, free, body, tail }) => {
            let mut env = env.clone();
            for arg in formals.iter() {
                env.insert(arg, base.extend(arg));
            }

            Lambda(Closure {
                formals: formals.iter().map(|arg| base.extend(arg)).collect(),
                free: free.into_iter().map(|arg| base.extend(arg)).collect(),
                body: body.into_iter().map(|b| rename1(&env, base, 0, b)).collect(),
                tail,
            })
        }

        Define { name, val } => {
            Define { name: base.extend(&name), val: box rename1(env, &base.extend(&name), 0, *val) }
        }

        Vector(list) => Vector(list.into_iter().map(|l| rename1(env, base, index, l)).collect()),

        // All literals and constants evaluate to itself
        Literal(v) => Literal(v),
    }
}

/// Lift all expressions in a program.
//
// Each sub expression may result in multiple expressions and it gets flattened
// at the top level. For example, a let expression binding 2 functions will
// expand to 3 expressions.
//
// Some values must be lifted to the top level to ease certain stages of the
// compiler. Actions are specific to the types - strings and symbols are added
// to a lookup table and lambda definitions are raised to top level.
pub fn lift(s: &mut State, prog: Vec<Core>) -> Vec<Core> {
    prog.into_iter().flat_map(|expr| lift1(s, expr)).collect()
}

// Function names are guaranteed to be unique after mangling, so its safe to
// lift *ALL* lambdas to top level.
//
// NOTE: ☝ this isn't currently right, because mangle doesn't cover all edge
// cases, but lift should be able to assume so.
fn lift1(s: &mut State, prog: Core) -> Vec<Core> {
    match prog {
        Literal(Str(reference)) => {
            if !s.strings.contains_key(&reference) {
                s.strings.insert(reference.clone(), s.strings.len());
            }
            vec![Literal(Str(reference))]
        }

        Literal(Symbol(reference)) => {
            if !s.symbols.contains_key(&reference) {
                s.symbols.insert(reference.clone(), s.symbols.len());
            }
            vec![Literal(Symbol(reference))]
        }

        Let { bindings, body } => {
            // Rest is all the name bindings that are not functions
            let rest: Vec<(Ident, Core)> = bindings
                .iter()
                .filter_map(|(ident, expr)| match expr {
                    Lambda(_) => None,
                    _ => Some((ident.clone(), shrink(lift1(s, expr.clone())))),
                })
                .collect();

            let mut export: Vec<Core> = bindings
                .into_iter()
                .filter_map(|(name, expr)| match expr {
                    Lambda(code) => {
                        let code = Closure { body: lift(s, code.body), ..code };
                        s.functions.insert(name.clone());
                        Some(Define { name, val: box Lambda(code) })
                    }
                    _ => None,
                })
                .collect();

            export.push(Let {
                bindings: rest,
                body: body.into_iter().map(|b| shrink(lift1(s, b))).collect(),
            });

            export
        }

        List(list) => vec![List(list.into_iter().map(|l| shrink(lift1(s, l))).collect())],

        Cond { pred, then, alt } => vec![Cond {
            pred: box shrink(lift1(s, *pred)),
            then: box shrink(lift1(s, *then)),
            alt: alt.map(|e| box shrink(lift1(s, *e))),
        }],

        // Lift named code blocks to top level immediately, since names are manged by now.
        Define { name, val: box Lambda(code) } => {
            s.functions.insert(name.clone());
            let body = (code).body.into_iter().map(|b| lift1(s, b)).flatten().collect();
            vec![Define { name, val: box Lambda(Closure { body, ..code }) }]
        }

        // Am unnamed literal lambda must be in an inline calling position
        // Lambda(Closure { .. }) => unimplemented!("inline λ"),
        e => vec![e],
    }
}

/// Convert an expression into [ANF](https://en.wikipedia.org/wiki/A-normal_form)
///
/// Break down complex expressions into a let binding with locals.
///
/// The generated names are NOT guaranteed to be unique and could be a problem
/// down the line.
pub fn anf(prog: Vec<Core>) -> Vec<Core> {
    prog.into_iter().map(anf1).collect()
}

fn anf1(prog: Core) -> Core {
    match prog {
        List(list) => {
            let (car, cdr) = list.split_at(1);

            // IF all arguments are already in normal form, return as is it
            if cdr.iter().all(|e| e.anf()) {
                List(list)
            } else {
                // Collect variables that will be bound to a new let block
                let bindings = cdr
                    .iter()
                    .enumerate()
                    .map(|(i, e)| (Ident::new(format!("_{}", i)), e.clone()))
                    .filter(|(_, e)| !e.anf());

                // Collect arguments for the function call where complex
                // expressions are replaced with a variable name
                let args: Vec<Core> = cdr
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        if e.anf() {
                            e.clone()
                        } else {
                            Identifier(Ident::new(format!("_{}", i)))
                        }
                    })
                    .collect();

                let body: Core = List(car.iter().chain(args.iter()).cloned().collect());

                Let { bindings: bindings.collect(), body: vec![body] }
            }
        }
        e => e,
    }
}

// Shrink a vector of expressions into a single expression
//
// TODO: Replace with `(begin ...)`, list really isn't the same thing
fn shrink<T: Clone>(es: Vec<Expr<T>>) -> Expr<T> {
    match es.len() {
        0 => Literal(Nil),
        1 => es[0].clone(),
        _ => List(es),
    }
}

// Annotate tail calls with a marker
pub fn tco(exprs: Vec<Core>) -> Vec<Core> {
    fn is_tail(name: &Ident, code: &Closure<Ident>) -> bool {
        // Get the expression in tail call position
        let last = code.body.last().and_then(tail);

        // Check if the tail call is a list and the first elem is an identifier
        match last {
            Some(List(l)) => match l.first() {
                Some(Identifier(id)) => id == name,
                _ => false,
            },
            _ => false,
        }
    }

    exprs
        .into_iter()
        .map(|expr| match expr {
            Define { name, val: box Lambda(code) } => Define {
                name: name.clone(),
                val: box Lambda(Closure { tail: is_tail(&name, &code), ..code }),
            },
            Let { bindings, body } => {
                let bindings = bindings
                    .into_iter()
                    .map(|(name, value)| match value {
                        Lambda(code) => {
                            (name.clone(), Lambda(Closure { tail: is_tail(&name, &code), ..code }))
                        }

                        _ => (name, value),
                    })
                    .collect();

                Let { bindings, body }
            }

            e => e,
        })
        .collect()
}

/// Return the tail position of the expression
///
/// A tail position is defined recursively as follows:
///
/// 1. The body of a procedure is in tail position.
/// 2. If a let expression is in tail position, then the body of the let is in
///    tail position.
/// 3. If the conditional expression (if test conseq altern) is in tail
///    position, then the conseq and altern branches are also in tail position.
/// 4. All other expressions are not in tail position.
fn tail<T: std::clone::Clone>(e: &Expr<T>) -> Option<&Expr<T>> {
    match e {
        // Lambda(Closure { body, .. }) => body.last().map(tail).flatten(),
        Let { body, .. } => body.last().and_then(tail),
        Cond { alt, .. } => {
            // What do I do with 2?
            alt.as_deref().and_then(|e| tail(&e))
        }
        e => Some(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse1;
    use pretty_assertions::assert_eq;

    fn rename(prog: Syntax) -> Core {
        super::rename1(&HashMap::new(), &Ident::empty(), 0, prog)
    }

    /// Mock rename, which blindly converts Strings to Identifiers
    fn mock(prog: Syntax) -> Core {
        match prog {
            Identifier(s) => Expr::Identifier(Ident::new(s)),

            Let { bindings, body } => Let {
                bindings: bindings
                    .iter()
                    .map(|(name, value)| (Ident::new(name), mock(value.clone())))
                    .collect(),

                body: body.into_iter().map(|b| mock(b)).collect(),
            },

            List(list) => List(list.into_iter().map(mock).collect()),

            Cond { pred, then, alt } => Cond {
                pred: box mock(*pred),
                then: box mock(*then),
                alt: alt.map(|u| box mock(*u)),
            },

            Lambda(Closure { formals, free, body, tail }) => Lambda(Closure {
                formals: formals.into_iter().map(Ident::new).collect(),
                free: free.into_iter().map(Ident::new).collect(),
                body: body.into_iter().map(mock).collect(),
                tail,
            }),

            Define { name, val } => Define { name: Ident::new(name), val: box mock(*val) },

            Vector(list) => Vector(list.into_iter().map(mock).collect()),

            // All literals and constants evaluate to itself
            Literal(v) => Literal(v),
        }
    }

    #[test]
    fn nest() {
        let x = rename(parse1(
            "(let ((x 1)
                   (y 2))
               (let ((z 3))
                 (+ x y z)))",
        ));

        let y = mock(parse1(
            "(let (({let 0}::x 1)
                  ({let 0}::y 2))
               (let (({let 0}::{let 1}::z 3))
                 (+ {let 0}::x {let 0}::y {let 0}::{let 1}::z))))",
        ));
        assert_eq!(x, y);
    }

    #[test]
    fn closure() {
        let x = rename(parse1(
            "(let ((add (lambda (x y) (+ x y))))
               (add 10 20))",
        ));

        let y = mock(parse1(
            "(let (({let 0}::add (lambda ({let 0}::add::x
                                          {let 0}::add::y)
                                              (+ {let 0}::add::x {let 0}::add::y))))
                                   ({let 0}::add 10 20))",
        ));

        assert_eq!(x, y);
    }

    #[test]
    fn function() {
        let x = rename(parse1("(define (add x y) (+ x y))"));
        let y = mock(parse1("(define (add add::x add::y) (+ add::x add::y))"));

        assert_eq!(x, y);
    }

    #[test]
    fn anf() {
        let x = parse1("(f (+ 1 2) 7)");
        let y = Let {
            bindings: vec![(
                Ident::new("_0"),
                List(vec![Ident::expr("+"), Literal(Number(1)), Literal(Number(2))]),
            )],
            body: vec![List(vec![Ident::expr("f"), Ident::expr("_0"), Literal(Number(7))])],
        };

        assert_eq!(y, anf1(rename(x)));
    }

    #[test]
    fn letrec() {
        let x = rename(parse1(
            "(let ((f (lambda (x) (g x x)))
                   (g (lambda (x y) (+ x y))))
               (f 12))",
        ));

        let y = mock(parse1(
            "(let (({let 0}::f (lambda ({let 0}::f::x) ({let 0}::g {let 0}::f::x {let 0}::f::x)))
                   ({let 0}::g (lambda ({let 0}::g::x {let 0}::g::y) (+ {let 0}::g::x {let 0}::g::y))))
               ({let 0}::f 12))",
        ));

        assert_eq!(x, y);
    }

    #[test]
    fn recursive() {
        let x = rename(parse1(
            "(let ((f (lambda (x)
               (if (zero? x)
                 1
                 (* x (f (dec x))))))) (f 5))",
        ));

        let y = mock(parse1(
            "(let (({let 0}::f (lambda ({let 0}::f::x)
               (if (zero? {let 0}::f::x)
                 1
                 (* {let 0}::f::x ({let 0}::f (dec {let 0}::f::x))))))) ({let 0}::f 5))",
        ));

        assert_eq!(x, y)
    }

    /// OMG! I'm so happy to finally see these tests this way! Took me years! 😢
    #[test]
    fn lift_simple() {
        let prog = r"(let ((id (lambda (x) x))) (id 42))";
        let expr = lift1(&mut Default::default(), rename(parse1(prog)));

        assert_eq!(expr[0], mock(parse1("(define ({let 0}::id {let 0}::id::x ) {let 0}::id::x)")));
        assert_eq!(expr[1], mock(parse1("(let () ({let 0}::id 42))")));
    }

    #[test]
    fn lift_recursive() {
        let prog = r"(let ((even (lambda (x) (if (zero? x) #t (odd (dec x)))))
                           (odd  (lambda (x) (if (zero? x) #f (even (dec x))))))
                       (even 25)))";

        let expr = lift1(&mut Default::default(), rename(parse1(prog)));

        assert_eq!(
            expr[0],
            mock(parse1(
                "(define ({let 0}::even {let 0}::even::x)
                   (if (zero? {let 0}::even::x) #t ({let 0}::odd (dec {let 0}::even::x))))"
            ))
        );

        assert_eq!(
            expr[1],
            mock(parse1(
                "(define ({let 0}::odd {let 0}::odd::x)
                   (if (zero? {let 0}::odd::x) #f ({let 0}::even (dec {let 0}::odd::x))))"
            ))
        );

        assert_eq!(expr[2], mock(parse1("(let () ({let 0}::even 25))")));
    }

    #[test]
    fn tails() {
        let prog = "(let ((factorial (lambda (x acc)
                                (if (zero? x)
                                  acc
                                  (factorial (dec x) (* x acc))))))
             (factorial 42 1))";

        let expr = parse1(prog);
        let expr = rename(expr);
        let exprs = lift1(&mut Default::default(), expr);

        match &exprs[0] {
            Define { name: _, val: box Lambda(code) } => assert_eq!(code.tail, false),
            _ => panic!(),
        };

        let exprs = tco(exprs);

        match &exprs[0] {
            Define { name: _, val: box Lambda(code) } => assert_eq!(code.tail, true),
            _ => panic!(),
        }
    }
}

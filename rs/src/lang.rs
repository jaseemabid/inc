//! Scheme language transformations & idiosyncrasies
//!
//! Home for renaming, lifting, type checks and everything else.
use crate::{
    compiler::state::State,
    core::{Expr::*, Literal::*, *},
};

use std::collections::HashMap;

/// Rename/mangle all references to unique names.
///
/// This is a fairly complicated thing to get right and being able to reuse a
/// well tested existing implementation would be great. See [RFC 2603], its
/// [discussion] and [tracking issue] to learn how rustc does this.
///
/// [RFC 2603]: https://github.com/rust-lang/rfcs/blob/master/text/2603-rust-symbol-name-mangling-v0.md
/// [discussion]: https://github.com/rust-lang/rfcs/pull/2603
/// [tracking issue]: https://github.com/rust-lang/rust/issues/60705
pub fn rename(prog: Vec<Expr>) -> Vec<Expr> {
    // TODO: This isn't right, same state should be used for all sub expressions.
    // Test for 2 different functions with the same name.
    prog.into_iter().map(|e| mangle(&HashMap::<&str, i64>::new(), e)).collect()
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
pub fn lift(s: &mut State, prog: Vec<Expr>) -> Vec<Expr> {
    prog.into_iter().flat_map(|expr| lift1(s, expr)).collect()
}

fn mangle(env: &HashMap<&str, i64>, prog: Expr) -> Expr {
    match prog {
        Identifier(ident) => Identifier(match env.get(ident.name.as_str()) {
            Some(n) => Ident { name: ident.name.clone(), index: *n },
            None => ident,
        }),

        Let { bindings, body } => mangle1(env, bindings, body),

        List(list) => List(list.into_iter().map(|l| mangle(env, l)).collect()),

        Cond { pred, then, alt } => Cond {
            pred: box mangle(env, *pred),
            then: box mangle(env, *then),
            alt: alt.map(|u| box mangle(env, *u)),
        },

        Lambda(code) => Lambda(Closure {
            body: code.body.into_iter().map(|b| mangle(env, b)).collect(),
            ..code
        }),

        // All literals and constants evaluate to itself
        v => v,
    }
}

fn mangle1(env: &HashMap<&str, i64>, bindings: Vec<(Ident, Expr)>, body: Vec<Expr>) -> Expr {
    // Collect all the names about to be bound for evaluating body
    let mut all = env.clone();
    for (ident, _index) in bindings.iter() {
        all.entry(ident.name.as_str()).and_modify(|e| *e += 1).or_insert(0);
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
                for (ident, _) in bindings.iter() {
                    if ident != current {
                        rest.entry(ident.name.as_str()).and_modify(|e| *e += 1).or_insert(0);
                    }
                }

                let value = match value {
                    Let { .. } => mangle(&all, value.clone()),
                    Lambda(c) => mangle(&all, Lambda(c.clone())),
                    _ => mangle(&rest, value.clone()),
                };

                let ident = Ident {
                    index: *all.get(current.name.as_str()).unwrap(),
                    name: current.name.clone(),
                };

                (ident, value)
            })
            .collect(),

        body: body.into_iter().map(|b| mangle(&all, b)).collect(),
    }
}

// Function names are guaranteed to be unique after mangling, so its safe to
// lift *ALL* lambdas to top level.
//
// NOTE: ☝ this isn't currently right, because mangle doesn't cover all edge
// cases, but lift should be able to assume so.
fn lift1(s: &mut State, prog: Expr) -> Vec<Expr> {
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
            let rest: Vec<(Ident, Expr)> = bindings
                .iter()
                .filter_map(|(ident, expr)| match expr {
                    Lambda(_) => None,
                    _ => Some((ident.clone(), shrink(lift1(s, expr.clone())))),
                })
                .collect();

            let mut export: Vec<Expr> = bindings
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
pub fn anf(prog: Vec<Expr>) -> Vec<Expr> {
    prog.into_iter().map(anf1).collect()
}

fn anf1(prog: Expr) -> Expr {
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
                let args: Vec<Expr> = cdr
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

                let body: Expr = List(car.iter().chain(args.iter()).cloned().collect());

                Let { bindings: bindings.collect(), body: vec![body] }
            }
        }
        e => e,
    }
}

// Shrink a vector of expressions into a single expression
//
// TODO: Replace with `(begin ...)`, list really isn't the same thing
fn shrink(es: Vec<Expr>) -> Expr {
    match es.len() {
        0 => Literal(Nil),
        1 => es[0].clone(),
        _ => List(es),
    }
}

// Annotate tail calls with a marker
pub fn tco(exprs: Vec<Expr>) -> Vec<Expr> {
    fn is_tail(name: &Ident, code: &Closure) -> bool {
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
fn tail(e: &Expr) -> Option<&Expr> {
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

    #[test]
    fn shadow1() {
        let x = parse1("(let ((x 1)) (let ((x 2)) (+ x x)))");
        let y = parse1("(let ((x.0 1)) (let ((x.1 2)) (+ x.1 x.1)))");

        assert_eq!(y, mangle(&HashMap::<&str, i64>::new(), x));
    }

    #[test]
    fn shadow2() {
        let x = parse1("(let ((t (cons 1 2))) (let ((t t)) (let ((t t)) (let ((t t)) t))))");
        let y = parse1(
            "(let ((t.0 (cons 1 2))) (let ((t.1 t.0)) (let ((t.2 t.1)) (let ((t.3 t.2)) t.3))))",
        );

        assert_eq!(y, mangle(&HashMap::<&str, i64>::new(), x));
    }

    #[test]
    fn shadow3() {
        let x = parse1(
            "(let ((x ()))
               (let ((x (cons x x)))
                 (let ((x (cons x x)))
                   (let ((x (cons x x)))
                     (cons x x)))))",
        );

        let y = parse1(
            "(let ((x.0 ()))
               (let ((x.1 (cons x.0 x.0)))
                 (let ((x.2 (cons x.1 x.1)))
                   (let ((x.3 (cons x.2 x.2)))
                     (cons x.3 x.3)))))",
        );

        assert_eq!(y, mangle(&HashMap::<&str, i64>::new(), x));
    }

    #[test]
    fn alias() {
        let x = parse1("(let ((x 1)) (let ((x x)) (+ x x)))");
        let y = parse1("(let ((x.0 1)) (let ((x.1 x.0)) (+ x.1 x.1)))");

        assert_eq!(y, mangle(&HashMap::<&str, i64>::new(), x));
    }

    #[test]
    fn anf() {
        let x = parse1("(f (+ 1 2) 7)");
        let y = parse1("(let ((_0 (+ 1 2))) (f _0 7))");

        assert_eq!(y, anf1(x));
    }

    #[test]
    fn letrec() {
        let expr = mangle(
            &HashMap::<&str, i64>::new(),
            parse1(
                "(let ((f (lambda (x) (g x x)))
                       (g (lambda (x y) (+ x y))))
                   (f 12))",
            ),
        );

        match expr {
            Let { bindings, body } => {
                assert_eq!(bindings[0].0, Ident::new("f"));
                assert_eq!(
                    bindings[0].1,
                    Lambda(Closure {
                        formals: vec!["x".into()],
                        body: vec![List(vec![
                            Expr::ident("g"),
                            Expr::ident("x"),
                            Expr::ident("x")
                        ])],
                        ..Default::default()
                    })
                );

                assert_eq!(bindings[1].0, Ident::new("g"));
                assert_eq!(
                    bindings[1].1,
                    Lambda(Closure {
                        formals: vec!["x".to_string(), "y".to_string()],
                        body: vec![List(vec![
                            Expr::ident("+"),
                            Expr::ident("x"),
                            Expr::ident("y")
                        ])],
                        ..Default::default()
                    })
                );

                assert_eq!(body, vec![List(vec![Expr::ident("f"), Expr::from(12)])]);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn recursive() {
        let x = Let {
            bindings: vec![(
                Ident::new("f"),
                Lambda(Closure {
                    formals: vec!["x".into()],
                    free: vec![],
                    body: vec![Cond {
                        pred: box List(vec![Expr::ident("zero?"), Expr::ident("x")]),
                        then: box Expr::from(1),
                        alt: Some(box List(vec![
                            Expr::ident("*"),
                            Expr::ident("x"),
                            List(vec![
                                Expr::ident("f"),
                                List(vec![Expr::ident("dec"), Expr::ident("x")]),
                            ]),
                        ])),
                    }],
                    tail: false,
                }),
            )],
            body: vec![List(vec![Expr::ident("f"), Expr::from(5)])],
        };

        let y = parse1(
            "(let ((f.0 (lambda (x)
                          (if (zero? x)
                            1
                            (* x (f.0 (dec x))))))) (f.0 5))",
        );

        assert_eq!(x, mangle(&HashMap::<&str, i64>::new(), y))
    }

    #[test]
    fn lift_simple() {
        let mut s: State = Default::default();
        let expr = lift1(&mut s, parse1(r"(let ((id (lambda (x) x))) (id 42))"));

        assert_eq!(
            expr[0],
            Define {
                name: Ident::from("id"),
                val: box Lambda(Closure {
                    formals: vec!["x".into()],
                    body: vec![Expr::ident("x")],
                    ..Default::default()
                })
            }
        );

        assert_eq!(
            expr[1],
            Let { bindings: vec![], body: vec![List(vec![Expr::ident("id"), Expr::from(42)])] }
        );
    }

    #[test]
    fn lift_recursive() {
        let prog = r"(let ((even (lambda (x) (if (zero? x) #t (odd (dec x)))))
                           (odd  (lambda (x) (if (zero? x) #f (even (dec x))))))
                       (e 25)))";

        let expr = lift1(&mut Default::default(), parse1(prog));

        assert_eq!(
            expr[0],
            Define {
                name: Ident::from("even"),
                val: box Lambda(Closure {
                    tail: false,
                    formals: vec!["x".into()],
                    free: vec![],
                    body: vec![Cond {
                        pred: box List(vec![Expr::ident("zero?"), Expr::ident("x")]),
                        then: box Expr::from(true),
                        alt: Some(box List(vec![
                            Expr::ident("odd"),
                            List(vec![Expr::ident("dec"), Expr::ident("x")])
                        ]))
                    }]
                })
            }
        );

        assert_eq!(
            expr[1],
            Define {
                name: Ident::from("odd"),
                val: box Lambda(Closure {
                    tail: false,
                    formals: vec!["x".into()],
                    free: vec![],
                    body: vec![Cond {
                        pred: box List(vec![Expr::ident("zero?"), Expr::ident("x")]),
                        then: box Expr::from(false),
                        alt: Some(box List(vec![
                            Expr::ident("even"),
                            List(vec![Expr::ident("dec"), Expr::ident("x")])
                        ]))
                    }]
                })
            }
        );

        assert_eq!(
            expr[2],
            Let { bindings: vec![], body: vec![List(vec![Expr::ident("e"), Expr::from(25)])] }
        );
    }

    #[test]
    fn tails() {
        let prog = "(let ((factorial (lambda (x acc)
                                (if (zero? x)
                                  acc
                                  (factorial (dec x) (* x acc))))))
             (factorial 42 1))";

        let expr = parse1(prog);
        let expr = mangle(&HashMap::<&str, i64>::new(), expr);
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

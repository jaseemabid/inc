//! Scheme language transformations & idiosyncrasies
//!
//! Home for renaming, lifting, type checks and everything else.
use crate::{
    compiler::state::State,
    core::{
        Code,
        Expr::{self, *},
        Ident,
    },
};

use std::collections::HashMap;

/// Rename/mangle all references to unique names
pub fn rename(prog: &[Expr]) -> Vec<Expr> {
    prog.iter().map(|e| mangle(&HashMap::<String, i64>::new(), e)).collect()
}

/// Lift all expressions in a program.
// Some values must be lifted to the top level to ease certain stages of the
// compiler. Actions are specific to the types - strings and symbols are added
// to a lookup table and lambda definitions are raised to top level.
pub fn lift(s: &mut State, prog: &[Expr]) -> Vec<Expr> {
    prog.iter().map({ |expr| lift1(s, &expr) }).collect()
}

/// Rename/mangle all references to unique names
///
// A sub expression in let binding is evaluated with the complete environment
// including the one being defined only if the subexpresison captures the
// closure with another let or lambda, otherwise evaluate with only the rest of
// the bindings.
fn mangle(env: &HashMap<String, i64>, prog: &Expr) -> Expr {
    match prog {
        Identifier(ident) => match env.get(&ident.name) {
            Some(n) => Identifier(Ident::new(ident.name.clone(), *n)),
            None => Identifier(ident.clone()),
        },

        Let { bindings, body } => {
            // Collect all the names about to be bound for evaluating body
            let mut all = env.clone();
            for (ident, _index) in bindings.iter() {
                all.entry(ident.name.clone()).and_modify(|e| *e += 1).or_insert(0);
            }

            let bindings = bindings.iter().map(|(current, value)| {
                // Collect all the names excluding the one being defined now
                let mut rest = env.clone();
                for (ident, _) in bindings {
                    if ident != current {
                        rest.entry(ident.name.clone()).and_modify(|e| *e += 1).or_insert(0);
                    }
                }

                let value = match value {
                    Let { .. } => mangle(&all, value),
                    Lambda(_) => mangle(&all, value),
                    _ => mangle(&rest, value),
                };

                let ident =
                    Ident { index: *all.get(&current.name).unwrap(), name: current.name.clone() };

                (ident, value)
            });

            Let {
                bindings: bindings.collect(),
                body: body.iter().map(|b| mangle(&all, b)).collect(),
            }
        }

        List(list) => List(list.iter().map(|l| mangle(env, l)).collect()),

        Cond { pred, then, alt } => Cond {
            pred: box mangle(env, pred),
            then: box mangle(env, then),
            alt: alt.as_ref().map(|u| box mangle(env, u)),
        },

        Lambda(Code { name, formals, free, body }) => Lambda(Code {
            name: name.clone(),
            formals: formals.clone(),
            free: free.clone(),
            body: body.iter().map(|b| mangle(env, b)).collect(),
        }),

        // All literals and constants evaluate to itself
        v => v.clone(),
    }
}

fn lift1(s: &mut State, prog: &Expr) -> Expr {
    match prog {
        Str(reference) => {
            if !s.strings.contains_key(reference) {
                s.strings.insert(reference.clone(), s.strings.len());
            }
            Str(reference.clone())
        }

        Symbol(reference) => {
            if !s.symbols.contains_key(reference) {
                s.symbols.insert(reference.clone(), s.symbols.len());
            }
            Symbol(reference.clone())
        }

        Let { bindings, body } => {
            // Rest is all the name bindings that are not functions
            let mut rest: Vec<(Ident, Expr)> = vec![];

            for (ident, expr) in bindings {
                match expr {
                    Lambda(Code { formals, free, body, .. }) => {
                        let code = Code {
                            name: Some(ident.name.to_string()),
                            formals: formals.clone(),
                            free: free.clone(),
                            body: lift(s, body),
                        };
                        s.functions.insert(ident.name.to_string(), code);
                    }

                    _ => rest.push((ident.clone(), lift1(s, expr))),
                };
            }

            let body = body.iter().map({ |b| lift1(s, b) }).collect();

            Let { bindings: rest, body }
        }

        List(list) => List(list.iter().map({ |l| lift1(s, l) }).collect()),

        Cond { pred, then, alt } => Cond {
            pred: box lift1(s, pred),
            then: box lift1(s, then),
            alt: alt.as_ref().map({ |box e| box lift1(s, &e) }),
        },

        // A literal lambda must be in an inline calling position
        Lambda(Code { .. }) => unimplemented!("inline λ"),

        e => e.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser, parser::parse1};
    use pretty_assertions::assert_eq;

    #[test]
    fn shadow1() {
        let x = parse1("(let ((x 1)) (let ((x 2)) (+ x x)))");
        let y = parse1("(let ((x.0 1)) (let ((x.1 2)) (+ x.1 x.1)))");

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn shadow2() {
        let x = parse1("(let ((t (cons 1 2))) (let ((t t)) (let ((t t)) (let ((t t)) t))))");
        let y = parse1(
            "(let ((t.0 (cons 1 2))) (let ((t.1 t.0)) (let ((t.2 t.1)) (let ((t.3 t.2)) t.3))))",
        );

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
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

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn alias() {
        let x = parse1("(let ((x 1)) (let ((x x)) (+ x x)))");
        let y = parse1("(let ((x.0 1)) (let ((x.1 x.0)) (+ x.1 x.1)))");

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn letrec() {
        let x = parse1(
            "(let ((f (lambda (x) (g x x)))
                   (g (lambda (x y) (+ x y))))
               (f 12))",
        );

        let y = parse1(
            "(let ((f.0 (lambda (x) (g.0 x x)))
                   (g.0 (lambda (x y) (+ x y))))
               (f 12))",
        );

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn recursive() {
        let x = parse1(
            "(let ((f (lambda (x)
                        (if (zero? x)
                          1
                          (* x (f (dec x))))))) (f 5))",
        );

        let y = parse1(
            "(let ((f.0 (lambda (x)
                          (if (zero? x)
                            1
                            (* x (f.0 (dec x))))))) (f.0 5))",
        );

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn lift_simple() {
        let mut s: State = Default::default();
        let expr = parse1(r"(let ((id (lambda (x) x))) (id 42))");

        let e = lift(&mut s, &[expr]);

        assert_eq!(
            s.functions.get("id").unwrap(),
            &Code {
                name: Some("id".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec!["x".into()],
            }
        );

        assert_eq!(e[0], Let { bindings: vec![], body: vec![List(vec!["id".into(), Number(42)])] });
    }

    #[test]
    fn lift_recursive() {
        let prog = r"(let ((e (lambda (x) (if (zero? x) #t (o (dec x)))))
                           (o (lambda (x) (if (zero? x) #f (e (dec x))))))
                       (e 25)))";

        let mut s: State = Default::default();

        let expr = match parser::parse(prog) {
            Ok(r) => r,
            Err(e) => panic!(e),
        };

        let e = lift(&mut s, &expr);

        assert_eq!(
            s.functions.get("e").unwrap(),
            &Code {
                name: Some("e".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Cond {
                    pred: box List(vec!["zero?".into(), "x".into()]),
                    then: box Boolean(true),
                    alt: Some(box List(vec!["o".into(), List(vec!["dec".into(), "x".into()])]))
                }]
            }
        );

        assert_eq!(
            s.functions.get("o").unwrap(),
            &Code {
                name: Some("o".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Cond {
                    pred: box List(vec!["zero?".into(), "x".into()]),
                    then: box Boolean(false),
                    alt: Some(box List(vec!["e".into(), List(vec!["dec".into(), "x".into()])]))
                }]
            }
        );

        assert_eq!(e[0], Let { bindings: vec![], body: vec![List(vec!["e".into(), Number(25)])] });
    }

    #[test]
    fn tco() {
        let mut s: State = Default::default();
        let expr = parse1(
            "(let ((factorial (lambda (x acc)
                                (if (zero? x)
                                  acc
                                  (factorial (dec x) (* x acc))))))
             (factorial 42 1))",
        );
        let e = lift1(&mut s, &expr);

        assert_eq!(
            s.functions.get("factorial").unwrap(),
            &Code {
                name: Some("factorial".into()),
                formals: vec!["x".into(), "acc".into()],
                free: vec![],
                body: vec![Cond {
                    pred: box List(vec!["zero?".into(), "x".into()],),
                    then: box "acc".into(),
                    alt: Some(box List(vec![
                        "factorial".into(),
                        List(vec!["dec".into(), "x".into()]),
                        List(vec!["*".into(), "x".into(), "acc".into()]),
                    ],),),
                }]
            }
        );

        assert_eq!(
            e,
            Let {
                bindings: vec![],
                body: vec![List(vec!["factorial".into(), Number(42), Number(1)])]
            }
        );
    }
}

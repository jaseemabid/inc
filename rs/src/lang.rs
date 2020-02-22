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
pub fn rename(prog: Vec<Expr>) -> Vec<Expr> {
    prog.into_iter().map(|e| mangle(&HashMap::<&str, i64>::new(), e)).collect()
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
//
// TODO: Change the type to get an owned copy and avoid all clones in the body
fn mangle(env: &HashMap<&str, i64>, prog: Expr) -> Expr {
    match prog {
        Identifier(ident) => Identifier(match env.get(ident.name.as_str()) {
            Some(n) => Ident::new(ident.name, *n),
            None => ident,
        }),

        Let { bindings, body } => mangle1(env, bindings, body),

        List(list) => List(list.into_iter().map(|l| mangle(env, l)).collect()),

        Cond { pred, then, alt } => Cond {
            pred: box mangle(env, *pred),
            then: box mangle(env, *then),
            alt: alt.map(|u| box mangle(env, *u)),
        },

        Lambda(code) => {
            Lambda(Code { body: code.body.into_iter().map(|b| mangle(env, b)).collect(), ..code })
        }

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
                    Lambda(c) => {
                        let mut c = c.clone();
                        c.name = Some(current.to_string());
                        mangle(&all, Lambda(c))
                    }
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
                    Lambda(Code { formals, free, body, tail, .. }) => {
                        let code = Code {
                            name: Some(ident.name.to_string()),
                            tail: *tail,
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
            alt: alt.as_deref().map({ |e| box lift1(s, &e) }),
        },

        // Am unnamed literal lambda must be in an inline calling position
        Lambda(Code { name: None, .. }) => unimplemented!("inline λ"),

        // Lift named code blocks to top level immediately, since names are manged by now.
        Lambda(code @ Code { name: Some(n), .. }) => {
            s.functions.insert(n.to_string(), code.clone());
            Expr::Nil
        }

        e => e.clone(),
    }
}

// Annotate tail calls with a marker
pub fn tco(s: &mut State) {
    /// Check if a code block can be tail call optimized
    fn is_tail(code: &Code) -> bool {
        // Get the expression in tail call position
        let exp = code.body.last().and_then(tail);

        // Check if the tail call is a list and the first elem is an identifier
        match exp {
            Some(List(l)) => match l.first() {
                Some(Identifier(Ident { name, .. })) => code.name.as_ref() == Some(name),
                _ => false,
            },
            _ => false,
        }
    }

    for (_, code) in s.functions.iter_mut() {
        code.tail = is_tail(code)
    }
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
        // Lambda(Code { body, .. }) => body.last().map(tail).flatten(),
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
    use crate::{parser, parser::parse1};
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

    // The mangle tests are so verbose and ugly!
    #[test]
    fn letrec() {
        let x = Let {
            bindings: vec![
                (
                    Ident { name: "f".to_string(), index: 0 },
                    Lambda(Code {
                        name: Some(String::from("f.0")),
                        formals: vec!["x".to_string()],
                        free: vec![],
                        body: vec![List(vec![
                            Identifier(Ident { name: "g".to_string(), index: 0 }),
                            Identifier(Ident { name: "x".to_string(), index: 0 }),
                            Identifier(Ident { name: "x".to_string(), index: 0 }),
                        ])],
                        tail: false,
                    }),
                ),
                (
                    Ident { name: "g".to_string(), index: 0 },
                    Lambda(Code {
                        name: Some(String::from("g.0")),
                        formals: vec!["x".to_string(), "y".to_string()],
                        free: vec![],
                        body: vec![List(vec![
                            Identifier(Ident { name: "+".to_string(), index: 0 }),
                            Identifier(Ident { name: "x".to_string(), index: 0 }),
                            Identifier(Ident { name: "y".to_string(), index: 0 }),
                        ])],
                        tail: false,
                    }),
                ),
            ],
            body: vec![List(vec![
                Identifier(Ident { name: "f".to_string(), index: 0 }),
                Number(12),
            ])],
        };

        let y = parse1(
            "(let ((f.0 (lambda (x) (g.0 x x)))
                   (g.0 (lambda (x y) (+ x y))))
               (f 12))",
        );

        assert_eq!(x, mangle(&HashMap::<&str, i64>::new(), y));
    }

    #[test]
    fn recursive() {
        let x = Let {
            bindings: vec![(
                Ident { name: String::from("f"), index: 0 },
                Lambda(Code {
                    name: Some(String::from("f.0")),
                    formals: vec!["x".into()],
                    free: vec![],
                    body: vec![Cond {
                        pred: box List(vec![
                            Identifier(Ident { name: String::from("zero?"), index: 0 }),
                            Identifier(Ident { name: String::from("x"), index: 0 }),
                        ]),
                        then: box Number(1),
                        alt: Some(box List(vec![
                            Identifier(Ident { name: String::from("*"), index: 0 }),
                            Identifier(Ident { name: String::from("x"), index: 0 }),
                            List(vec![
                                Identifier(Ident { name: String::from("f"), index: 0 }),
                                List(vec![
                                    Identifier(Ident { name: String::from("dec"), index: 0 }),
                                    Identifier(Ident { name: String::from("x"), index: 0 }),
                                ]),
                            ]),
                        ])),
                    }],
                    tail: false,
                }),
            )],
            body: vec![List(vec![
                Identifier(Ident { name: String::from("f"), index: 0 }),
                Number(5),
            ])],
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
        let expr = parse1(r"(let ((id (lambda (x) x))) (id 42))");

        let e = lift(&mut s, &[expr]);

        assert_eq!(
            s.functions.get("id").unwrap(),
            &Code {
                name: Some("id".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec!["x".into()],
                tail: false
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
                tail: false,
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

                tail: false,
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
    fn tails() {
        let mut s: State = Default::default();

        let expr = parse1(
            "(let ((factorial (lambda (x acc)
                                (if (zero? x)
                                  acc
                                  (factorial (dec x) (* x acc))))))
             (factorial 42 1))",
        );

        lift1(&mut s, &expr);

        let code = s.functions.get("factorial").unwrap().clone();

        assert_eq!(code.tail, false);
        assert_eq!(
            code.body.last().and_then(tail).unwrap(),
            &List(vec![
                "factorial".into(),
                List(vec!["dec".into(), "x".into()]),
                List(vec!["*".into(), "x".into(), "acc".into()]),
            ])
        );

        tco(&mut s);
        assert_eq!(s.functions.get("factorial").unwrap().clone().tail, true);
    }
}

//! Core lang transformations that understand Scheme language idiosyncrasies
//!
//! Home for renaming, lifting, type checks and everything else.

use crate::core::{
    Code,
    Expr::{self, *},
};
use std::collections::HashMap;

/// Rename/mangle all references to unique names
pub fn rename(prog: &Vec<Expr>) -> Vec<Expr> {
    prog.iter().map(|e| mangle(&HashMap::<String, i64>::new(), e)).collect()
}

/// Mangle a single expression with letrec support.
///
// A sub expression in let binding is evaluated with the complete environment
// including the one being defined only if the subexpresison captures the
// closure with another let or lambda, otherwise evaluate with only the rest of
// the bindings.
fn mangle(env: &HashMap<String, i64>, prog: &Expr) -> Expr {
    match prog {
        Identifier(i) => match env.get(i.as_str()) {
            Some(n) => Identifier(format!("{}.{}", i, n)),
            None => Identifier(i.to_string()),
        },

        Let { bindings, body } => {
            // Collect all the names about to be bound for evaluating body
            let mut all = env.clone();
            for (name, _) in bindings {
                all.entry(name.into()).and_modify(|e| *e += 1).or_insert(0);
            }

            let bindings = bindings.iter().map(|(name, value)| {
                // Collect all the names excluding the one being defined now
                let mut rest = env.clone();
                for (n, _) in bindings {
                    if n != name {
                        rest.entry(n.into()).and_modify(|e| *e += 1).or_insert(0);
                    }
                }

                let value = match value {
                    Let { .. } => mangle(&all, value),
                    Lambda(_) => mangle(&all, value),
                    _ => mangle(&rest, value),
                };

                let index = all.get(name).unwrap();
                let name = format!("{}.{}", name, index);

                (name, value)
            });

            Let {
                bindings: bindings.collect(),
                body: body.iter().map(|b| mangle(&all, b)).collect(),
            }
        }

        List(list) => List(list.iter().map(|l| mangle(env, l)).collect()),

        Cond { pred, then, alt } => Cond {
            pred: Box::new(mangle(env, pred)),
            then: Box::new(mangle(env, then)),
            alt: alt.as_ref().map(|u| Box::new(mangle(env, u))),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::{Error, Expr},
        parser::parse,
    };
    use pretty_assertions::assert_eq;

    fn one(x: Result<Vec<Expr>, Error>) -> Expr {
        x.unwrap()[0].clone()
    }

    #[test]
    fn shadow1() {
        let x = one(parse("(let ((x 1)) (let ((x 2)) (+ x x)))"));
        let y = one(parse("(let ((x.0 1)) (let ((x.1 2)) (+ x.1 x.1)))"));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn shadow2() {
        let x = one(parse("(let ((t (cons 1 2))) (let ((t t)) (let ((t t)) (let ((t t)) t))))"));
        let y = one(parse(
            "(let ((t.0 (cons 1 2))) (let ((t.1 t.0)) (let ((t.2 t.1)) (let ((t.3 t.2)) t.3))))",
        ));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn shadow3() {
        let x = one(parse(
            "(let ((x ()))
               (let ((x (cons x x)))
                 (let ((x (cons x x)))
                   (let ((x (cons x x)))
                     (cons x x)))))",
        ));

        let y = one(parse(
            "(let ((x.0 ()))
               (let ((x.1 (cons x.0 x.0)))
                 (let ((x.2 (cons x.1 x.1)))
                   (let ((x.3 (cons x.2 x.2)))
                     (cons x.3 x.3)))))",
        ));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }
    #[test]
    fn alias() {
        let x = one(parse("(let ((x 1)) (let ((x x)) (+ x x)))"));
        let y = one(parse("(let ((x.0 1)) (let ((x.1 x.0)) (+ x.1 x.1)))"));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }
    #[test]
    fn letrec() {
        let x = one(parse(
            "(let ((f (lambda (x) (g x x)))
                   (g (lambda (x y) (+ x y))))
               (f 12))",
        ));

        let y = one(parse(
            "(let ((f.0 (lambda (x) (g.0 x x)))
                   (g.0 (lambda (x y) (+ x y))))
               (f.0 12))",
        ));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }

    #[test]
    fn recursive() {
        let x = one(parse(
            "(let ((f (lambda (x)
                        (if (zero? x)
                          1
                          (* x (f (dec x))))))) (f 5))",
        ));

        let y = one(parse(
            "(let ((f.0 (lambda (x)
                          (if (zero? x)
                            1
                            (* x (f.0 (dec x))))))) (f.0 5))",
        ));

        assert_eq!(y, mangle(&HashMap::<String, i64>::new(), &x));
    }
}

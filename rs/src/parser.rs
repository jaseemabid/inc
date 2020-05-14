//! A scheme parser in nom.
//!
//! Describes the [formal BNF grammar][grammar] in Rust as closely as possible
//! using the nom parser combinator library.
//!
//! ✏ This module is heavily documented and the order of declaration follow the
//! grammar; so the source best read sequentially in the declared order rather
//! than alphabetically here.
//!
//! See [lisper][lisper] for a similar Haskell implementation.
//!
//! [grammar]: http://www.scheme.com/tspl2d/grammar.html
//! [lisper]: https://github.com/jaseemabid/lisper/blob/master/src/Lisper/Parser.hs
use super::core::*;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{multispace0 as space0, multispace1 as space1, *},
    combinator::{map, opt, value},
    multi::*,
    sequence::*,
    IResult,
};
use std::str;

/// A program consists of a sequence of definitions and expressions.
///
/// ```BNF
/// <program>  → <form>*
/// <form>     → <definition> | <expression>
/// ```
fn program(i: &str) -> IResult<&str, Vec<Expr>> {
    many1(delimited(space0, form, space0))(i)
}

fn form(i: &str) -> IResult<&str, Expr> {
    alt((definition, expression))(i)
}

/// Definitions include various forms of declarations
///
/// Definitions include variable and syntax definitions, begin forms containing
/// zero or more definitions, let-syntax and letrec-syntax forms expanding into
/// zero or more definitions, and derived definitions. Derived definitions are
/// syntactic extensions that expand into some form of definition. A transformer
/// expression is a syntax-rules form or some other expression that produces a
/// transformer.
///
/// ```BNF
///
/// <definition> → <variable definition>
///              | <syntax definition>
///              | (begin <definition>*)
///              | (let-syntax (<syntax binding>*) <definition>*)
///              | (letrec-syntax (<syntax binding>*) <definition>*)
///              | <derived definition>
///
/// <variable definition> → (define <variable> <expression>)
///                       | (define (<variable> <variable>*) <body>)
///                       | (define (<variable> <variable>* . <variable>) <body>)
///
/// <variable>          → <identifier>
/// <body>              → <definition>* <expression>+
/// <syntax definition> → (define-syntax <keyword> <transformer expression>)
/// <keyword>           → <identifier>
/// <syntax binding>    → (<keyword> <transformer expression>)
/// ```
fn definition(i: &str) -> IResult<&str, Expr> {
    define_syntax(i) // | begin_syntax
}

/// ✓ (define <variable> <expression>) |
/// ✓ (define (<variable> <variable>*) <body>) |
/// ✓ (define (<variable> <variable>* . <variable>) <body>)
fn define_syntax(i: &str) -> IResult<&str, Expr> {
    alt((define_variable, define_lambda, define_variadic_fn))(i)
}

fn define_variable(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tuple((open, tag("define"), space1))(i)?;
    let (i, (name, _)) = tuple((identifier, space1))(i)?;
    let (i, body) = expression(i)?;
    let (i, _) = close(i)?;

    let name = Some(Ident::from(name));

    Ok((
        i,
        Expr::Lambda(Code { name, tail: false, formals: vec![], body: vec![body], free: vec![] }),
    ))
}

fn define_lambda(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tuple((open, tag("define"), space1))(i)?;
    let (i, mut params) = delimited(open, identifiers, close)(i)?;
    let (i, body) = delimited(space0, many1(terminated(expression, space0)), space0)(i)?;
    let (i, _) = close(i)?;

    let name = Some(Ident::from(params[0].clone()));
    let formals = params.split_off(1);

    Ok((i, Expr::Lambda(Code { name, tail: false, formals, body, free: vec![] })))
}

fn define_variadic_fn(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tuple((open, tag("define"), space1))(i)?;
    let (i, mut params) = delimited(open, identifiers, tag("."))(i)?;
    let (i, rest_param) = delimited(space0, identifier, close)(i)?;
    let (i, body) = delimited(space0, many1(terminated(expression, space0)), space0)(i)?;
    let (i, _) = close(i)?;

    let name = Some(Ident::from(params[0].clone()));
    let mut formals = params.split_off(1);
    formals.push(rest_param);

    Ok((i, Expr::Lambda(Code { name, tail: false, formals, body, free: vec![] })))
}

/// Core expressions
///
/// Expressions include core expressions, let-syntax or letrec-syntax forms
/// expanding into a sequence of one or more expressions, and derived
/// expressions. The core expressions are self-evaluating constants, variable
/// references, applications, and quote, lambda, if, and set! expressions.
/// Derived expressions include and, begin, case, cond, delay, do, let, let*,
/// letrec, or, and quasiquote expressions plus syntactic extensions that expand
/// into some form of expression.
///
/// ```BNF
/// <expression>  → <constant>
///               | <variable>
///               | (quote <datum>) | ' <datum>
///               | (lambda <formals> <body>)
///               | (if <expression> <expression> <expression>)
///               | (if <expression> <expression>)
///               | (set! <variable> <expression>)
///               | <application>
///               | (let-syntax (<syntax binding>*) <expression>+)
///               | (letrec-syntax (<syntax binding>*) <expression>+)
///               | <derived expression>
///
/// <constant>    → <boolean> | <number> | <character> | <string>
/// <formals>     → <variable> | (<variable>*) | (<variable>+ . <variable>)
/// <application> → (<expression> <expression>*)
/// ```
fn expression(i: &str) -> IResult<&str, Expr> {
    alt((constant, variable, quote, lambda_syntax, if_syntax, let_syntax, application))(i)
}

/// `(let-syntax (<syntax binding>*) <expression>+)`
fn let_syntax(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tuple((open, tag("let"), space1))(i)?;
    let (i, bindings) = delimited(open, many0(binding), close)(i)?;
    let (i, body) = delimited(space0, many1(terminated(expression, space0)), space0)(i)?;
    let (i, _) = close(i)?;

    Ok((i, Expr::Let { bindings, body }))
}

/// `named → (name value)`
fn binding(i: &str) -> IResult<&str, (Ident, Expr)> {
    let (i, (_, name, _, value, _, _)) =
        tuple((open, identifier, space1, expression, close, space0))(i)?;

    Ok((i, (Ident::from(name), value)))
}

/// `(lambda <formals> <body>)`
fn lambda_syntax(i: &str) -> IResult<&str, Expr> {
    let (i, (_, _, _, formals, _, body, _, _)) =
        tuple((open, tag("lambda"), space1, formals, space0, body, space0, close))(i)?;

    Ok((i, Expr::Lambda(Code { name: None, tail: false, formals, body, free: vec![] })))
}

/// `(if <expression> <expression> <expression>) | (if <expression> <expression>)`
fn if_syntax(i: &str) -> IResult<&str, Expr> {
    let (i, (_, _, _, pred, _, then, alt, _, _)) = tuple((
        open,
        tag("if"),
        space1,
        expression,
        space1,
        expression,
        opt(tuple((space1, expression))),
        space0,
        close,
    ))(i)?;

    Ok((i, Expr::Cond { pred: box pred, then: box then, alt: alt.map(|(_, a)| box a) }))
}

/// variable is an identifier
fn variable(i: &str) -> IResult<&str, Expr> {
    map(identifier, |name| Expr::Identifier(Ident::from(name)))(i)
}

/// `<formals>     → <variable> | (<variable>*) | (<variable>+ . <variable>)`
fn formals(i: &str) -> IResult<&str, Vec<String>> {
    alt((
        map(identifier, |s| vec![s]),
        delimited(open, many0(terminated(identifier, space0)), close),
    ))(i)
}

/// `<body> → <definition>* <expression>+`
fn body(i: &str) -> IResult<&str, Vec<Expr>> {
    let (i, mut es) = many1(expression)(i)?;

    let mut v = Vec::new();
    v.append(&mut es);
    Ok((i, v))
}

/// (quote <datum>) | '<datum>
// Note: This parser only handles simple quoted symbols for now
fn quote(i: &str) -> IResult<&str, Expr> {
    map(tuple((tag("\'"), identifier)), |(_, i)| Expr::Symbol(i))(i)
}

/// `<constant> → <boolean> | <number> | <character> | <string>`
fn constant(i: &str) -> IResult<&str, Expr> {
    alt((
        (map(tag("()"), |_| Expr::Nil)),
        (map(ascii, Expr::Char)),
        (map(boolean, Expr::Boolean)),
        (map(number, Expr::Number)),
        (map(string, Expr::Str)),
    ))(i)
}

/// `<application> → (<expression> <expression>*)`
fn application(i: &str) -> IResult<&str, Expr> {
    let (i, (_, a, _, mut b, _)) =
        tuple((open, expression, space0, many0(terminated(expression, space0)), close))(i)?;

    let mut v = vec![a];
    v.append(&mut b);

    Ok((i, Expr::List(v)))
}

/// Identifiers may denote variables, keywords, or symbols depending upon
/// context.
///
/// They are formed from sequences of letters, digits, and special
/// characters. With three exceptions, identifiers cannot begin with a
/// character that can also begin a number, i.e., they cannot begin with .,
/// +, -, or a digit. The three exceptions are the identifiers ..., +, and -.
/// Case is insignificant in symbols so that, for example, newspaper,
/// NewsPaper, and NEWSPAPER all represent the same identifier.
///
/// ```BNF
/// <identifier> → <initial> <subsequent>* | + | - | ...
/// <initial>    → <letter> | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
/// <subsequent> → <initial> | <digit> | . | + | -
/// <letter>     → a | b | ... | z
/// <digit>      → 0 | 1 | ... | 9
/// ```
fn identifier(i: &str) -> IResult<&str, String> {
    alt((
        value(String::from("+"), tag("+")),
        value(String::from("-"), tag("-")),
        value(String::from("..."), tag("...")),
        map(tuple((initial, many0(subsequent))), |(i, s)| {
            // Convert a vector of chars to string
            // https://doc.rust-lang.org/stable/core/iter/trait.Iterator.html#method.collect
            format!("{}{}", i, s.iter().collect::<String>())
        }),
    ))(i)
}

fn identifiers(i: &str) -> IResult<&str, Vec<String>> {
    many1(terminated(identifier, space0))(i)
}

fn initial(i: &str) -> IResult<&str, char> {
    alt((letter, symbol))(i)
}

fn subsequent(i: &str) -> IResult<&str, char> {
    alt((initial, digit, one_of(".+-")))(i)
}

fn symbol(i: &str) -> IResult<&str, char> {
    one_of("!$%&*/:<=>?~_^")(i)
}

fn letter(i: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(i)
}

fn digit(i: &str) -> IResult<&str, char> {
    one_of("0123456789")(i)
}

/// Data include booleans, numbers, characters, strings, symbols, lists, and
/// vectors.
///
/// Case is insignificant in the syntax for booleans, numbers, and
/// character names, but it is significant in other character constants and
/// in strings. For example, #T is equivalent to #t, #E1E3 is equivalent to
/// #e1e3, #X2aBc is equivalent to #x2abc, and #\NewLine is equivalent to
/// #\newline; but #\A is distinct from #\a and "String" is distinct from
/// string".
///
/// ```BNF
/// <datum>            → <boolean> | <number> | <character> | <string> | <symbol> | <list> | <vector>
/// <boolean>          → #t | #f
/// <number>           → <num 2> | <num 8> | <num 10> | <num 16>
/// <character>        → #\ <any character> | #\newline | #\space
/// <string>           → " <string character>* "
/// <string character> → \" | \\ | <any character other than" or \>
/// <symbol>           →  <identifier>
/// <list>             →  (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
/// <abbreviation>     →  ' <datum> | ` <datum> | , <datum> | ,@ <datum>
/// <vector>           → #(<datum>*)
/// ```
#[cfg(test)]
fn datum(i: &str) -> IResult<&str, Expr> {
    alt((
        (map(tag("()"), |_| Expr::Nil)),
        (map(boolean, Expr::Boolean)),
        (map(ascii, Expr::Char)),
        (map(number, Expr::Number)),
        (map(identifier, |name| Expr::Identifier(Ident::from(name)))),
        (map(string, Expr::Str)),
        list,
    ))(i)
}

fn boolean(i: &str) -> IResult<&str, bool> {
    alt((value(true, tag("#t")), value(false, tag("#f"))))(i)
}

fn sign(i: &str) -> IResult<&str, i64> {
    alt((value(-1, tag("-")), value(1, tag("+"))))(i)
}

fn number(i: &str) -> IResult<&str, i64> {
    let (i, s) = opt(sign)(i)?;
    let (i, n) = digit1(i)?;

    // TODO: Propagate this error up rather than panic
    let n = n.parse::<i64>().expect(&format!("Failed to parse digits into i64: `{:?}`\n", n)[..]);

    Ok((i, s.unwrap_or(1) * n))
}

/// ASCII Characters for now
fn ascii(i: &str) -> IResult<&str, u8> {
    // $ man ascii
    alt((
        value(9 as u8, tag(r"#\tab")),
        value(10 as u8, tag(r"#\newline")),
        value(13 as u8, tag(r"#\return")),
        value(32 as u8, tag(r"#\space")),
        // Picking the first byte is quite unsafe, fix for UTF8
        preceded(tag(r"#\"), map(anychar, |c: char| c as u8)),
    ))(i)
}

fn string(i: &str) -> IResult<&str, String> {
    let q = "\"";
    let (i, s) = delimited(tag(q), opt(is_not(q)), tag(q))(i)?;

    Ok((i, s.map_or(String::from(""), |s| s.to_string())))
}

/// `<list> → (<datum>*) | (<datum>+ . <datum>) | <abbreviation>`
#[cfg(test)]
fn list(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tuple((char('('), space0))(i)?;
    let (i, elems) = separated_list(space1, datum)(i)?;
    let (i, _) = tuple((space0, char(')')))(i)?;

    if elems.is_empty() {
        Ok((i, Expr::Nil))
    } else {
        Ok((i, Expr::List(elems)))
    }
}

fn open(i: &str) -> IResult<&str, ()> {
    let (i, _) = tuple((char('('), space0))(i)?;
    Ok((i, ()))
}

fn close(i: &str) -> IResult<&str, ()> {
    let (i, _) = tuple((space0, char(')')))(i)?;
    Ok((i, ()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Expr::*;
    use pretty_assertions::assert_eq;

    // OK consumes all of the input and succeeds
    const fn ok<T, E>(t: T) -> IResult<&'static str, T, E> {
        Ok(("", t))
    }

    // Partial consumes some of the input and succeeds
    const fn partial<T, E>(rest: &str, t: T) -> IResult<&str, T, E> {
        Ok((rest, t))
    }

    // Fail denotes a parser failing without consuming any of its input
    const fn fail<T>(i: &str) -> IResult<&str, T, (&str, nom::error::ErrorKind)> {
        Err(nom::Err::Error((i, nom::error::ErrorKind::Tag)))
    }

    #[test]
    fn assorted() {
        assert_eq!(ok(true), boolean("#t"));
        assert_eq!(ok(false), boolean("#f"));
        assert_eq!(fail("A"), boolean("A"));

        assert_eq!(ok('?'), symbol("?"));

        assert_eq!(ok(42), number("42"));
        assert_eq!(ok(-42), number("-42"));

        assert_eq!(ok(b'j'), ascii("#\\j"));
        assert_eq!(ok(b'^'), ascii("#\\^"));

        // Character parser must not consume anything unless it starts with
        // an explicit tag.
        assert_eq!(fail("test"), ascii("test"));
    }

    #[test]
    fn identifiers() {
        assert_eq!(ok(String::from("x")), identifier("x"));
        assert_eq!(ok(String::from("one")), identifier("one"));
        assert_eq!(ok(String::from("!bang")), identifier("!bang"));
        assert_eq!(ok(String::from("a->b")), identifier("a->b"));
        assert_eq!(ok(String::from("+")), identifier("+"));
        assert_eq!(ok(String::from("-")), identifier("-"));
        assert_eq!(ok(String::from("i64")), identifier("i64"));

        // -> is not an identifier, consume the - as an id and return the >
        assert_eq!(partial(">", String::from("-")), identifier("->"));

        // Identifiers must split at space and not consume anything
        // afterwards
        assert_eq!(partial(" b", String::from("a")), identifier("a b"));

        // Quoted symbols are not identifiers
        assert_eq!(partial("'woo", String::from("a")), identifier("a'woo"));

        // Internal hack, allow shadowing with .
        assert_eq!(ok(Identifier(Ident { name: String::from("foo"), index: 4 })), datum("foo.4"));
    }

    // #[test]
    // fn unicode() {
    //     assert_eq!(fail(("അ")), identifier(("അ")))
    // }

    #[test]
    fn data() {
        assert_eq!(ok(Nil), datum("()"));
        assert_eq!(ok("one".into()), datum("one"));
        assert_eq!(ok(42.into()), datum("42"));
    }

    #[test]
    fn strings() {
        assert_eq!(ok(Str("hello world".into())), datum("\"hello world\""));
        assert_eq!(ok(Str("മലയാളം".into())), datum("\"മലയാളം\""));

        assert_eq!(ok(Str("Unicode 😱 ⌘".into())), datum("\"Unicode 😱 ⌘\""));

        assert_eq!(ok(Str("".into())), datum("\"\""));
    }

    #[test]
    fn lists() {
        assert_eq!(ok(List(vec!["+".into(), 1.into()])), list("(+ 1)"));

        assert_eq!(
            ok(List(vec![1.into(), 2.into(), 3.into(), "a".into(), "b".into(), "c".into()])),
            list("(1 2 3 a b c)")
        );

        assert_eq!(
            ok(List(vec!["inc".into(), List(vec!["inc".into(), 42.into()]),],)),
            list("(inc (inc 42))")
        );

        // Lists should throw away all spaces in between
        assert_eq!(program("(   +   1 )"), program("(+ 1)"));
    }

    #[test]
    fn binary() {
        assert_eq!(ok(List(vec!["+".into(), "x".into(), 1776.into()])), list("(+ x 1776)"));

        assert_eq!(
            ok(List(
                vec!["+".into(), "x".into(), List(vec!["*".into(), "a".into(), "b".into()],),],
            )),
            list("(+ x (* a b))")
        );
    }

    #[test]
    fn top() {
        assert_eq!(ok(vec![true.into()]), program("#t"));
        assert_eq!(ok(vec![false.into()]), program("#f"));

        assert_eq!(ok(vec!['?'.into()]), program("#\\?"));

        assert_eq!(ok(vec![42.into()]), program("42"));
        assert_eq!(ok(vec![(-42).into()]), program("-42"));

        assert_eq!(ok(vec!['j'.into()]), program("#\\j"));
        assert_eq!(ok(vec!['^'.into()]), program("#\\^"));
    }

    #[test]
    fn let_syntax() {
        let p1 = "(let ((x 1) (y 2)) (+ x y))";
        let p2 = "(let ((x 1)) (let ((x 2)) #t) x)";

        let e1 = Let {
            bindings: vec![(Ident::from("x"), Number(1)), (Ident::from("y"), Number(2))],
            body: vec![List(vec![("+".into()), (Expr::from("x")), (Expr::from("y"))])],
        };

        let e2 = Let {
            bindings: vec![(Ident::from("x"), Number(1))],
            body: vec![
                Let { bindings: vec![(Ident::from("x"), Number(2))], body: vec![true.into()] },
                Expr::from("x"),
            ],
        };

        assert_eq!(ok(e1), super::let_syntax(p1));
        assert_eq!(ok(e2), super::let_syntax(p2));

        assert!(program("(let ((x (let ((y (+ 1 2))) (* y y)))) (cons x (+ x x)))").is_ok());

        assert!(program("(let ((x (let ((y 3)) (* y y)))) (cons x (+ x x)))").is_ok());
    }

    #[test]
    fn if_syntax() {
        let prog = "(if #t 12 13)";
        let exp = Cond { pred: box true.into(), then: box 12.into(), alt: Some(box 13.into()) };

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(if #t 14)";
        let exp = Cond { pred: box true.into(), then: box 14.into(), alt: None };

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(if (zero? x) 1 (* x (f (dec x))))";
        let exp = Cond {
            pred: box List(vec!["zero?".into(), "x".into()]),
            then: box 1.into(),
            alt: Some(box List(vec![
                "*".into(),
                "x".into(),
                List(vec!["f".into(), List(vec!["dec".into(), "x".into()])]),
            ])),
        };

        assert_eq!(ok(vec![exp]), program(prog));
    }

    #[test]
    fn application() {
        assert_eq!(ok(List(vec!["f".into(), "x".into()])), super::application("(f x)"));
        assert_eq!(ok(List(vec!["f".into()])), super::application("(f)"));
    }

    #[test]
    fn quotes() {
        let p = super::program("(symbol=? 'one 'two)");
        let e = vec![List(vec!["symbol=?".into(), Symbol("one".into()), Symbol("two".into())])];

        assert_eq!(ok(e), p);
    }

    #[test]
    fn define_syntax() -> Result<(), nom::Err<(&'static str, nom::error::ErrorKind)>> {
        let prog = "(define (id x) x)";
        let exp = Lambda(Code {
            name: Some(Ident::new("id")),
            tail: false,
            formals: vec!["x".into()],
            body: vec![("x".into())],
            free: vec![],
        });

        let (rest, x) = super::define_syntax(prog)?;
        assert_eq!(rest, "");
        assert_eq!(exp, x);
        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(define (pi) 42)";
        let exp = Lambda(Code {
            name: Some(Ident::new("pi")),
            tail: false,
            formals: vec![],
            body: vec![42.into()],
            free: vec![],
        });

        let (rest, x) = super::define_syntax(prog)?;
        assert_eq!(rest, "");
        assert_eq!(exp, x);
        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(define pi 42)";
        let exp = Lambda(Code {
            name: Some(Ident::new("pi")),
            tail: false,
            formals: vec![],
            body: vec![42.into()],
            free: vec![],
        });

        let (rest, x) = super::define_syntax(prog)?;
        assert_eq!(rest, "");
        assert_eq!(exp, x);
        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(define pi 42)";
        let exp = Lambda(Code {
            name: Some(Ident::new("pi")),
            tail: false,
            formals: vec![],
            body: vec![42.into()],
            free: vec![],
        });

        let (rest, x) = super::define_syntax(prog)?;
        assert_eq!(rest, "");
        assert_eq!(exp, x);
        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(define (add a b) (+ a b))";
        let exp = Lambda(Code {
            name: Some(Ident::new("add")),
            tail: false,
            formals: vec!["a".into(), "b".into()],
            body: vec![Expr::List(vec!["+".into(), "a".into(), "b".into()])],
            free: vec![],
        });

        let (rest, x) = super::define_syntax(prog)?;
        assert_eq!(rest, "");
        assert_eq!(exp, x);
        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(define (add x y . args) (reduce + 0 args))";
        let exp = Lambda(Code {
            name: Some(Ident::new("add")),
            tail: false,
            formals: vec!["x".into(), "y".into(), "args".into()],
            body: vec![Expr::List(vec!["reduce".into(), "+".into(), 0.into(), "args".into()])],
            free: vec![],
        });

        assert_eq!(ok(vec![exp]), program(prog));

        Ok(())
    }

    #[test]
    fn lambda_syntax() {
        let prog = "(lambda () 1)";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            formals: vec![],
            body: vec![Number(1)],
            free: vec![],
        });

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(lambda (a b ) a)";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            formals: vec!["a".into(), "b".into()],
            free: vec![],
            body: vec![("a".into())],
        });

        assert_eq!(ok(vec![exp]), program(prog));
        // assert_eq!(ok(exp), super::lambda_syntax(prog));

        let prog = "(lambda (a b) (+ b a))";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            free: vec![],
            formals: vec!["a".into(), "b".into()],
            body: vec![Expr::List(vec!["+".into(), "b".into(), "a".into()])],
        });

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(lambda a a)";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            formals: vec!["a".into()],
            free: vec![],
            body: vec![("a".into())],
        });

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(lambda (x) (if #t 1 2))";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            formals: vec!["x".into()],
            free: vec![],
            body: vec![Cond { pred: box true.into(), then: box 1.into(), alt: Some(box 2.into()) }],
        });

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(lambda (x) (if (zero? x) 1 (* x (f (dec x)))))";
        let exp = Lambda(Code {
            name: None,
            tail: false,
            formals: vec!["x".into()],
            free: vec![],
            body: vec![Cond {
                pred: box List(vec![("zero?".into()), ("x".into())]),
                then: box 1.into(),
                alt: Some(box List(vec![
                    "*".into(),
                    "x".into(),
                    List(vec!["f".into(), List(vec!["dec".into(), "x".into()])]),
                ])),
            }],
        });

        assert_eq!(ok(vec![exp]), program(prog));
    }
}

/// Parse a single expression for testing, return or panic
#[cfg(test)]
pub fn parse1(i: &str) -> Expr {
    match form(i) {
        Ok((_rest, e)) => e,
        Err(e) => panic!("Failed to parse `{}`: {:?}", i, e),
    }
}

/// Parse the whole program
pub fn parse<'a>(i: &'a str) -> Result<Vec<Expr>, Error<'a>> {
    match program(i) {
        Ok((_rest, expressions)) => Ok(expressions),
        Err(e) => Err(Error::Parser(e)),
    }
}

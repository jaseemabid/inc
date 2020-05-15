//! Core types shared by most of the program
use self::Expr::*;
use colored::Colorize;
use std::fmt;

/// Abstract Syntax Tree for a single expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // An empty list `()`
    Nil,
    // 61b number with a 3bit tag
    Number(i64),
    // #t & #f
    Boolean(bool),
    // A unicode char encoded in UTF-8 can take upto 4 bytes and won't fit in a
    // word; so this implementation makes sense only for ASCII.
    Char(u8),
    // UTF-8 Strings
    Str(String),
    // Scheme Identifiers
    Identifier(Ident),
    // Symbols
    Symbol(String),
    // Since Rust needs to know the size of the Expr type upfront, we need an
    // indirection here with `Vec<>` for recursive types. In this context, Vec
    // is just a convenient way to have a `Box<[Expr]>`
    List(Vec<Expr>),
    // Vectors
    Vector(Vec<Expr>),
    // Conditional
    Cond { pred: Box<Expr>, then: Box<Expr>, alt: Option<Box<Expr>> },
    // Variable bindings
    Let { bindings: Vec<(Ident, Expr)>, body: Vec<Expr> },
    // Variable definitions. Similar to let but could be at the top level
    Define { name: Ident, val: Box<Expr> },
    // Functions
    Lambda(Closure),
}

/// Ident is a refinement type for an identifier
// Rather than rely on a symbol table to store the metadata of an
// identifier, store them inline within the AST.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident {
    //  User defined name of the variable
    pub name: String,
    // Disambiguate the same name. Eg, a0, a1, a2
    pub index: i64,
}

impl Ident {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self { name: name.into(), index: 0 }
    }

    pub fn inc(self: &Self) -> Self {
        Self { name: self.name.clone(), index: self.index + 1 }
    }

    pub fn from<S: Into<String>>(name: S) -> Self {
        match name.into().split('.').collect::<Vec<&str>>().as_slice() {
            [name, index] => {
                Self { name: (*name).to_string(), index: index.parse::<i64>().unwrap() }
            }
            [name] => Self { name: (*name).to_string(), index: 0 },
            _ => unreachable!(),
        }
    }
}

/// Closure is a refinement type for Expression specialized for lambdas
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Closure {
    // Formal arguments to the function, filled in by the parser
    pub formals: Vec<String>,
    // Free variables, added post closure conversion
    pub free: Vec<String>,
    // A body is a list of expressions evaluated in order
    pub body: Vec<Expr>,
    // Is this a tail call?
    pub tail: bool,
}

impl Expr {
    /// Checks if an expression is in [A-Normal Form](https://en.wikipedia.org/wiki/A-normal_form)
    pub fn anf(&self) -> bool {
        match self {
            Nil | Number(..) | Boolean(..) | Char(..) => true,
            Str(..) | Identifier(..) | Symbol(..) | Vector(..) => true,
            List(..) | Cond { .. } | Let { .. } | Lambda { .. } | Define { .. } => false,
        }
    }
}

/// Pretty print an Expr
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Ident { name, index } = self;
        if *index == 0 {
            write!(f, "{}", name)
        } else {
            write!(f, "{}.{}", name, index)
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Boolean(t) => write!(f, "{}", if *t { "#t" } else { "#f" }),
            Expr::Char(c) => {
                let p = match *c as char {
                    '\t' => "#\\tab".into(),
                    '\n' => "#\\newline".into(),
                    '\r' => "#\\return".into(),
                    ' ' => "#\\space".into(),
                    _ => format!("#\\{}", *c as char),
                };
                write!(f, "{}", &p)
            }

            Expr::Str(s) => write!(f, "\"{}\"", s),
            Expr::Identifier(Ident { name, index }) => write!(f, "{}.{}", name, index),
            Expr::Symbol(i) => write!(f, "'{}", i),
            Expr::List(l) => {
                write!(f, "(")?;
                let mut l = l.iter().peekable();
                while let Some(elem) = l.next() {
                    if l.peek().is_some() {
                        write!(f, "{} ", elem)?;
                    } else {
                        write!(f, "{}", elem)?;
                    }
                }
                write!(f, ")")
            }

            // TODO: Pretty print ports differently from other vectors
            // Example: #<input/output port stdin/out> | #<output port /tmp/foo.txt>
            Expr::Vector(l) => {
                write!(f, "[")?;
                let mut l = l.iter().peekable();
                while let Some(elem) = l.next() {
                    if l.peek().is_some() {
                        write!(f, "{} ", elem)?;
                    } else {
                        write!(f, "{}", elem)?;
                    }
                }
                write!(f, "]")
            }
            Expr::Cond { pred, then, alt } => match alt {
                None => write!(f, "(if {} {})", pred, then),
                Some(t) => write!(f, "(if {} {} {})", pred, then, t),
            },
            Expr::Let { bindings, body } => {
                write!(f, "(let (")?;
                bindings.iter().for_each(|(a, b)| write!(f, "({} {})", a, b).unwrap());
                write!(f, ") ")?;
                body.iter().for_each(|b| write!(f, "{}", b).unwrap());
                write!(f, ")")
            }
            Expr::Lambda(Closure { formals, body, tail, .. }) => {
                if *tail {
                    write!(f, "(^λ^ (")?;
                } else {
                    write!(f, "(λ (")?;
                }

                formals.iter().for_each(|arg| write!(f, "{}", arg).unwrap());
                write!(f, ") ")?;
                body.iter().for_each(|b| write!(f, "{}", b).unwrap());
                write!(f, ")")
            }
            Expr::Define { name, val } => write!(f, "(define {} {})", name, val),
        }
    }
}

/// Idiomatic type conversions from the primitive types to Expr
///
/// https://doc.rust-lang.org/rust-by-example/conversion/from_into.html
/// https://ricardomartins.cc/2016/08/03/convenient_and_idiomatic_conversions_in_rust
impl From<i64> for Expr {
    fn from(i: i64) -> Self {
        Expr::Number(i)
    }
}

impl From<bool> for Expr {
    fn from(b: bool) -> Self {
        Expr::Boolean(b)
    }
}

impl From<char> for Expr {
    fn from(c: char) -> Self {
        Expr::Char(c as u8)
    }
}

impl From<&str> for Expr {
    fn from(i: &str) -> Self {
        Expr::Identifier(Ident::from(i))
    }
}

/// Control behavior and external interaction of the program.
pub struct Config {
    /// Program is the input source
    pub program: String,
    /// Name of the generated asm and executable, stdout otherwise
    pub output: String,
}

impl Config {
    pub fn asm(&self) -> String {
        let stdout = String::from("/dev/stdout");
        if self.output == stdout {
            stdout
        } else {
            format!("{}.s", self.output)
        }
    }
}

/// Custom error type for all of inc
// See these links for more context on how custom error types work in Rust.
// - https://learning-rust.github.io/docs/e7.custom_error_types.html
// - https://rust-lang-nursery.github.io/cli-wg/tutorial/errors.html
#[derive(Debug)]
pub enum Error<'a> {
    // Errors returned by nom
    Parser(nom::Err<(&'a str, nom::error::ErrorKind)>),
    // Internal errors are unexpected errors within the compiler
    Internal { message: String, e: Option<std::io::Error> },
    // Runtime errors in scheme like an undefined variable
    Runtime(String),
    // Compilation errors in Scheme like missing functions and type errors
    Compilation(String),
}

// Implement std::convert::From for Error; from io::Error
impl<'a> From<std::io::Error> for Error<'a> {
    fn from(error: std::io::Error) -> Self {
        Error::Internal { message: String::from(""), e: Some(error) }
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parser(e) => {
                writeln!(f, "{}\n", "Failed to parse program".red().bold())?;
                writeln!(f, "{:?}", e)
            }
            Self::Internal { message, e } => {
                writeln!(f, "{}\n", "Something went wrong!".red().bold())?;
                writeln!(f, "{}", message)?;
                writeln!(f, "{:?}", e)
            }
            Self::Runtime(e) => {
                writeln!(f, "{}", "Runtime error!".red().bold())?;
                writeln!(f, "{}", e)
            }
            Self::Compilation(e) => {
                writeln!(f, "{}\n", "Failed to compile program".red().bold())?;
                writeln!(f, "{:?}", e)
            }
        }
    }
}

//! Core types shared by most of the program
use colored::Colorize;
use std::{clone::Clone, fmt};

/// Parameterized Abstract Syntax Tree
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<T: Clone> {
    Literal(Literal),
    // Scheme Identifiers, parameterized by T. Could be a String or `Ident`
    Identifier(T),
    List(Vec<Expr<T>>),
    Vector(Vec<Expr<T>>),
    Cond { pred: Box<Expr<T>>, then: Box<Expr<T>>, alt: Option<Box<Expr<T>>> },
    Let { bindings: Vec<(T, Expr<T>)>, body: Vec<Expr<T>> },
    Define { name: T, val: Box<Expr<T>> },
    Lambda(Closure<T>),
}

/// Syntax representation or Stage 0 of the AST
///
/// Parser generates one of these values
pub type Syntax = Expr<String>;

/// Core AST or Stage 1+
pub type Core = Expr<Ident>;

/// Literal types of Scheme
//
// Literals are a separate type to share code across various stages of AST types
// and to make exhaustive pattern matches more explicit.
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
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
    // Symbols
    Symbol(String),
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

    pub fn expr<S: Into<String>>(name: S) -> Expr<Ident> {
        Expr::Identifier(Ident::from(name))
    }
}

/// Closure is a refinement type for Expression specialized for lambdas
#[derive(Clone, Debug, PartialEq)]
pub struct Closure<T: Clone> {
    // Formal arguments to the function, filled in by the parser
    pub formals: Vec<String>,
    // Free variables, added post closure conversion
    pub free: Vec<String>,
    // A body is a list of expressions evaluated in order
    pub body: Vec<Expr<T>>,
    // Is this a tail call?
    pub tail: bool,
}

impl<T: Clone> Expr<T> {
    /// Checks if an expression is in [A-Normal Form](https://en.wikipedia.org/wiki/A-normal_form)
    pub fn anf(&self) -> bool {
        match self {
            Expr::Literal(..) => true,
            _ => false,
        }
    }

    pub fn symbol<S: Into<String>>(name: S) -> Self {
        Expr::Literal(Literal::Symbol(name.into()))
    }

    pub fn string<S: Into<String>>(name: S) -> Self {
        Expr::Literal(Literal::Str(name.into()))
    }
}

impl Expr<String> {
    pub fn name<S: Into<String>>(name: S) -> Self {
        Self::Identifier(name.into())
    }
}

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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(t) => write!(f, "{}", if *t { "#t" } else { "#f" }),
            Self::Nil => write!(f, "()"),
            Self::Char(c) => {
                let p = match *c as char {
                    '\t' => "#\\tab".into(),
                    '\n' => "#\\newline".into(),
                    '\r' => "#\\return".into(),
                    ' ' => "#\\space".into(),
                    _ => format!("#\\{}", *c as char),
                };
                write!(f, "{}", &p)
            }

            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Symbol(i) => write!(f, "'{}", i),
        }
    }
}

impl<T: Clone + fmt::Display> fmt::Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Identifier(i) => write!(f, "{}", i),
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

impl<T: Clone> From<i64> for Expr<T> {
    fn from(i: i64) -> Self {
        Self::Literal(Literal::Number(i))
    }
}

impl<T: Clone> From<bool> for Expr<T> {
    fn from(b: bool) -> Self {
        Self::Literal(Literal::Boolean(b))
    }
}

impl<T: Clone> From<char> for Expr<T> {
    fn from(c: char) -> Self {
        Self::Literal(Literal::Char(c as u8))
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

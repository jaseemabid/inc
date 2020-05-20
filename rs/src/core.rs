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

/// Syntax representation of Scheme
///
/// Parser returns a `Syntax` as one of the first steps.
pub type Syntax = Expr<String>;

/// Intermediate AST
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Identifiers with metadata and namespaces
pub struct Ident {
    name: Vec<String>,
}

/// Closures are code blocks with their environment captured
#[derive(Clone, Debug, PartialEq)]
pub struct Closure<T: Clone> {
    pub formals: Vec<T>,
    pub free: Vec<T>,
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

impl Ident {
    pub const fn empty() -> Self {
        Self { name: vec![] }
    }

    pub fn new<S: Into<String>>(name: S) -> Self {
        Self { name: name.into().split("::").map(|s| s.into()).collect::<Vec<_>>() }
    }

    /// Create a new identifier extending an existing environment
    ///
    /// ```
    /// # use inc::core::Ident;
    /// let base = Ident::new("top");
    /// assert_eq!(Ident::new("top::fn"), base.extend("fn"))
    /// ```
    pub fn extend<S: Into<String>>(&self, s: S) -> Self {
        let mut name = self.name.clone();
        name.push(s.into());
        Self { name }
    }

    pub fn expr<S: Into<String>>(name: S) -> Expr<Ident> {
        Expr::Identifier(Ident::new(name))
    }

    /// Short names
    pub fn short(&self) -> String {
        self.name.last().unwrap().to_string()
    }

    /// Mangled names for code generation
    /// TODO: This is obviously wrong
    pub fn mangle(&self) -> String {
        self.name.last().unwrap().to_string()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = self.name.iter().fold(String::new(), |s, arg| s + &arg + " ");
        result.pop();

        write!(f, "{}", result)
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

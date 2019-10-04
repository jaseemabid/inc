//! Core types shared by most of the program
//!
//! An [`Expr`] is a single statement in the program. The parser returns a list
//! of this type.
//!
//! [`Expr`]: core::Expr
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
    Identifier(String),
    // Since Rust needs to know the size of the Expr type upfront, we need an
    // indirection here with `Vec<>` for recursive types. In this context, Vec
    // is just a convenient way to have a `Box<[Expr]>`
    List(Vec<Expr>),
    // Conditional
    Cond { pred: Box<Expr>, then: Box<Expr>, alt: Option<Box<Expr>> },
    // Variable bindings
    Let { bindings: Vec<(String, Expr)>, body: Vec<Expr> },
    // Functions
    Lambda(Code),
}

/// Code is a refinement type for Expression specialized for lambdas
#[derive(Debug, PartialEq, Clone)]
pub struct Code {
    // A rose by any other name would smell as sweet
    pub name: Option<String>,
    // Formal arguments to the function, filled in by the parser
    pub formals: Vec<String>,
    // Free variables, added post closure conversion
    pub free: Vec<String>,
    // A body is a list of expressions evaluated in order
    pub body: Vec<Expr>,
}

impl Expr {
    /// Is this expression a constant that would fit in a word?
    /// This is useful for FFI into C
    pub fn unbox(self: &Expr) -> Option<i64> {
        match self {
            Expr::Number(n) => Some(*n),
            Expr::Boolean(true) => Some(1),
            Expr::Boolean(false) => Some(0),
            Expr::Char(c) => Some(i64::from(*c)),
            _ => None,
        }
    }
}

/// Pretty print an Expr
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Boolean(t) => write!(f, "{}", if *t { "#t" } else { "#f" }),
            Expr::Char(c) => write!(f, "{}", c),
            Expr::Str(s) => write!(f, "\"{}\"", s),
            Expr::Identifier(i) => write!(f, "{}", i),
            Expr::List(l) => {
                write!(f, "(")?;
                for i in l {
                    write!(f, "{} ", i)?;
                }
                write!(f, ")")
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
            Expr::Lambda(Code { formals, body, .. }) => {
                write!(f, "(λ (")?;
                formals.iter().for_each(|arg| write!(f, "{}", arg).unwrap());
                write!(f, ") ")?;
                body.iter().for_each(|b| write!(f, "{}", b).unwrap());
                write!(f, ")")
            }
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
        Expr::Identifier(String::from(i))
    }
}

/// Control behavior and external interaction of the program.
pub struct Config {
    /// Program is the input source
    pub program: String,
    /// Name of the generated asm and executable, stdout otherwise
    pub output: String,
    /// Execute?
    pub exec: bool,
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

/// Custom error type for all of inc.
// This might not be idiomatic Rust, revisit later.
// https://doc.rust-lang.org/std/error/trait.Error.html
// https://learning-rust.github.io/docs/e7.custom_error_types.html
// https://doc.rust-lang.org/beta/rust-by-example/error/multiple_error_types/define_error_type.html
// https://medium.com/@fredrikanderzon/custom-error-types-in-rust-and-the-operator-b499d0fb2925
#[derive(Debug)]
pub struct Error {
    pub message: String,
}

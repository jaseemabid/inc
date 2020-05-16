//! Runtime representation of scheme objects
//!
//! Immediate values (values that can be fit in one machine word) are tagged for
//! distinguising them from heap allocated pointers. The last 3 bits serve as
//! the runtime type of the value. Always using 3 bits is a simpler approach
//! than the multi bit technique the paper uses. This is a very efficient and
//! low overhead technique at the cost of losing precision - completely
//! acceptable for types like characters and booleans but having to live with
//! 61bit numerics instead of native 64 and some overhead for operations like
//! multiplication & division.
//!
//! See the paper for details. See tests for examples.

use crate::core::{
    Expr::{self, *},
    Literal::*,
};

pub const NUM: i64 = 0;
pub const BOOL: i64 = 1;
pub const CHAR: i64 = 2;
pub const PAIR: i64 = 3;
pub const NIL: i64 = 4;
pub const STR: i64 = 5;
pub const SYM: i64 = 6;
pub const VEC: i64 = 7;

pub const SHIFT: i64 = 3;
pub const MASK: i64 = 0b0000_0111;

pub const FALSE: i64 = (0 << SHIFT) | BOOL;
pub const TRUE: i64 = (1 << SHIFT) | BOOL;

/// Immediate representation of an expression.
pub fn to(prog: &Expr) -> Option<i64> {
    match prog {
        Literal(Number(i)) => Some((i << SHIFT) | NUM),
        Literal(Boolean(true)) => Some(TRUE),
        Literal(Boolean(false)) => Some(FALSE),
        // An ASCII char is a single byte, so most of these shifts should be
        // OK. This is going to go wrong pretty badly with Unicode.
        Literal(Char(c)) => {
            // Expand u8 to i64 before shifting right, this will easily
            // overflow and give bogus results otherwise. Unit testing FTW!
            Some((i64::from(*c) << SHIFT) | CHAR)
        }
        Literal(Nil) => Some(NIL),
        _ => None,
    }
}

// Immediate representation of numbers is required so often a helper is
// useful.
pub const fn n(i: i64) -> i64 {
    (i << SHIFT) | NUM
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn numbers() {
        assert_eq!(to(&Expr::from(0)), Some(0));
        assert_eq!(to(&Expr::from(1)), Some(8));
    }

    #[test]
    fn chars() {
        assert_eq!(to(&Literal(Char(b'A'))), Some((65 << SHIFT) + CHAR))
    }
}

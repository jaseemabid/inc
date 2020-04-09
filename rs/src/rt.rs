//! Scheme runtime for Incremental
//!
//! Runtime implements primitives for memory management, IO etc and all other
//! low level nuances. Functions defined here are available at runtime to be
//! called from scheme functions.

use crate::{immediate::*, x86::WORDSIZE};
use std::{ffi::CStr, io::Write, os::raw::c_char};

/// Checks if a function is defined in the built in runtime
pub fn defined(name: &str) -> bool {
    SYMBOLS.contains(&name)
}

// All symbols exported to the runtime from this module
const SYMBOLS: [&str; 9] = [
    "string-length",
    "symbol=?",
    "exit",
    "rt-open-write",
    "rt-current-input-port",
    "rt-current-output-port",
    "rt-current-error-port",
    "writeln",
    "type",
];

#[no_mangle]
pub extern "C" fn print(val: i64, nested: bool) {
    match val & MASK {
        NUM => print!("{}", val >> SHIFT),
        BOOL => print!("{}", if val == TRUE { "#t" } else { "#f" }),
        CHAR => {
            let c = ((val >> SHIFT) as u8) as char;
            let p = match c {
                '\t' => "#\\tab".into(),
                '\n' => "#\\newline".into(),
                '\r' => "#\\return".into(),
                ' ' => "#\\space".into(),
                _ => format!("#\\{}", c),
            };
            print!("{}", &p)
        }
        PAIR => {
            let pcar = car(val);
            let pcdr = cdr(val);

            if !nested {
                print!("(")
            };

            print(pcar, false);

            if pcdr != NIL {
                if (pcdr & MASK) != PAIR {
                    print!(" . ");
                    print(pcdr, false);
                } else {
                    print!(" ");
                    print(pcdr, true);
                }
            }
            if !nested {
                print!(")")
            };
        }
        NIL => {
            print!("()");
        }
        STR => print!("\"{}\"", str_str(val)),
        SYM => print!("'{}", sym_name(val)),

        // TODO: Pretty print ports differently from other vectors
        // Example: #<input/output port stdin/out> | #<output port /tmp/foo.txt>
        VEC => {
            print!("[");

            for i in 0..vec_len(val) {
                print(vec_nth(val, i), false);

                if i != vec_len(val) - 1 {
                    print!(" ");
                }
            }

            print!("]");

            std::io::stdout().flush().unwrap();
        }
        _ => panic!("Unexpected value returned by runtime: {}", val),
    }

    std::io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn car(val: i64) -> i64 {
    assert!((val & MASK) == PAIR);

    unsafe { *((val - PAIR) as *mut i64) }
}

#[no_mangle]
pub extern "C" fn cdr(val: i64) -> i64 {
    assert!((val & MASK) == PAIR);

    unsafe { *((val - PAIR + 8) as *mut i64) }
}

#[no_mangle]
pub extern "C" fn string_length(val: i64) -> usize {
    assert!((val & MASK) == STR);

    let len = unsafe { *((val - STR) as *mut usize) };
    len << SHIFT
}

#[no_mangle]
pub extern "C" fn symbol_eq(a: i64, b: i64) -> i64 {
    if (a == b) && ((a & MASK) == SYM) {
        TRUE
    } else {
        FALSE
    }
}

// Get a string pointer from a string object
fn str_str(val: i64) -> String {
    assert!((val & MASK) == STR);

    let s = unsafe { CStr::from_ptr((val - STR + 8) as *const c_char) };
    s.to_string_lossy().into_owned()
}

fn sym_name(val: i64) -> String {
    assert!((val & MASK) == SYM);

    let s = unsafe { CStr::from_ptr((val - SYM + 16) as *const c_char) };
    s.to_string_lossy().into_owned()
}

fn vec_len(val: i64) -> i64 {
    assert!((val & MASK) == VEC);

    unsafe { *((val - VEC) as *const i64) }
}

fn vec_nth(val: i64, n: i64) -> i64 {
    assert!((val & MASK) == VEC);

    unsafe { *((val - VEC + WORDSIZE + (n * WORDSIZE)) as *const i64) }
}

/// IO Primitives for Inc
pub mod io {
    use super::*;
    use std::{
        fs::{self, File},
        os::unix::io::AsRawFd,
    };

    /// Open a file and return immediate encoded file descriptor
    #[no_mangle]
    pub extern "C" fn rt_open_write(fname: i64) -> i64 {
        let f = File::create(str_str(fname)).unwrap().as_raw_fd();
        i64::from(f << SHIFT)
    }

    /// Write a new line terminated string to a port object
    #[no_mangle]
    pub extern "C" fn writeln(data: i64, port: i64) -> i64 {
        let path = str_str(vec_nth(port, 1));
        let s = format!("{}\n", str_str(data));

        fs::write(&path, s).unwrap_or_else(|_| panic!("Failed to write to {}", &path));

        NIL
    }

    // Standard ports can be overridden in Scheme, but these constants would do
    // for now
    #[no_mangle]
    pub extern "C" fn rt_current_input_port() -> i64 {
        i64::from(0 << SHIFT)
    }

    #[no_mangle]
    pub extern "C" fn rt_current_output_port() -> i64 {
        i64::from(1 << SHIFT)
    }

    #[no_mangle]
    pub extern "C" fn rt_current_error_port() -> i64 {
        i64::from(2 << SHIFT)
    }
}

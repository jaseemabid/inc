//! Runtime functions implemented in Rust!
//!
//! All functions in this module are available to the C/ASM runtime and ideally
//! all runtime functions and helpers must be exported via this module.

use crate::{immediate::*, x86::WORDSIZE};
use std::{
    ffi::CStr,
    fs::{self, File},
    io::{self, Write},
    os::{raw::c_char, unix::io::AsRawFd},
};

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

        VEC => {
            print!("[");

            for i in 0..vec_len(val) {
                print(vec_nth(val, i), false);

                if i != vec_len(val) - 1 {
                    print!(" ");
                }
            }

            print!("]");

            io::stdout().flush().unwrap();
        }
        _ => panic!("Unexpected value returned by runtime: {}", val),
    }

    io::stdout().flush().unwrap();
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

#[no_mangle]
pub extern "C" fn rt_open_write(fname: i64) -> i64 {
    let f = File::create(str_str(fname)).unwrap().as_raw_fd();
    i64::from(f << SHIFT)
}

#[no_mangle]
pub extern "C" fn writeln(data: i64, port: i64) -> i64 {
    let path = str_str(vec_nth(port, 1));
    let s = format!("{}\n", str_str(data));

    fs::write(&path, s).unwrap_or_else(|_| panic!("Failed to write to {}", &path));

    NIL
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

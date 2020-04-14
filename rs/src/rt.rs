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
const SYMBOLS: [&str; 11] = [
    "exit",
    "rt-current-error-port",
    "rt-current-input-port",
    "rt-current-output-port",
    "rt-open-write",
    "rt-open-read",
    "rt-read",
    "rt-write",
    "string-length",
    "symbol=?",
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

/// Read current heap pointer from r12
///
/// See [Exploring ARM inline assembly in
/// Rust](http://embed.rs/articles/2016/arm-inline-assembly-rust) for an intro
/// into inline asm.
pub fn heap() -> usize {
    let r12: usize;
    unsafe {
        asm!("nop" : "={r12}"(r12) ::: "intel");
    }
    r12
}

/// Allocate space on the scheme heap
///
/// In terms of lines of machine code vs time taken to write, this function tops
/// the chart for me by a huge factor. Took about 2 days to write something that
/// would run for 2 or 3ns.
///
/// This function compiles down to something like this:
///
/// ```asm
/// inc::rt::allocate:
///     push    rax
///     add     rdi, 7
///     and     rdi, -8
///     mov     qword, ptr, [rsp], rdi
///     add     r12, qword, ptr, [rsp]
///     pop     rax
///     ret
/// ```
///
/// The *correct* way to write this function is to mark r12 as clobbered - tell
/// the compiler explicitly that register r12 will be modified. This is done
/// with the 4th argument to
/// [std::asm!](https://doc.rust-lang.org/std/macro.asm.html) macro.
///
/// ```rs
/// asm!("add r12, $0" :: "m"(aligned) : "r12" : "intel");
/// ```
///
/// As per [System V Calling Convention](crate::x86), `r12` is a callee saved
/// register and a function is supposed to leave it as it is found. Rust
/// compiler explicitly adds a `push r12` at the beginning of the function and
/// restores it before returning with explicit clobbers.
///
/// ```asm
/// inc::rt::allocate:
///     push    r12                     <- Save r12
///     push    rax
///     add     rdi, 7
///     and     rdi, -8
///     mov     qword, ptr, [rsp], rdi
///     add     r12, qword, ptr, [rsp]
///     add     rsp, 8
///     pop     r12                     <- Restore r12
///     ret
/// ```
///
/// As far I understand, only the second version is correct as per the calling
/// convention. Rust compiler should have added the 2 additional instructions
/// even in the first version. Maybe this is the responsibility of the inline
/// asm block, I'm not sure.
///
/// This silly runtime use r12 as a global value and needs it to be mutated (and
/// I designed it that way before I learned what I just wrote here), so I need
/// rustc to generate the first version even though its not quite right.
///
/// I really don't know if I'm relying on some rustc bug or if this is OK. Here
/// is a [minimal reproduction example](https://godbolt.org/z/MM6ezC).
///
/// Know better? Please let me know!
pub fn allocate(size: usize) {
    let aligned = ((size + 7) / 8) * 8;

    unsafe {
        // Increment r12 to allocate space
        asm!("add r12, $0" :: "m"(aligned) :: "intel");
    }
}

/// IO Primitives for Inc
pub mod io {
    use super::*;
    use std::{
        fs::{self, File},
        os::unix::io::AsRawFd,
    };

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

    /// Open a file for writing and return the immediate encoded file descriptor
    /// Creates file if it doesn't exist already
    #[no_mangle]
    pub extern "C" fn rt_open_write(fname: i64) -> i64 {
        let f = File::create(str_str(fname)).unwrap().as_raw_fd();
        i64::from(f << SHIFT)
    }

    /// Open a file for reading return the immediate encoded file descriptor
    /// Fails if file doesn't exist already
    #[no_mangle]
    pub extern "C" fn rt_open_read(fname: i64) -> i64 {
        let f = File::open(str_str(fname)).unwrap().as_raw_fd();
        i64::from(f << SHIFT)
    }

    /// Write a string object to a port
    #[no_mangle]
    pub extern "C" fn rt_write(data: i64, port: i64) -> i64 {
        let path = str_str(vec_nth(port, 1));
        fs::write(&path, str_str(data)).unwrap_or_else(|_| panic!("Failed to write to {}", &path));

        NIL
    }

    /// Read string from a port object
    //
    // ⚠️ This is so far away from the spec and should be called something else.
    //
    // This is honestly making me wonder WTH I'm really doing. There is no need
    // to really do this in assembly, what I need is a custom allocator in Rust.
    // See `strings::make` as well.
    //
    // This is legit cursed!
    #[no_mangle]
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    pub extern "C" fn rt_read(port: i64) -> i64 {
        let path = str_str(vec_nth(port, 1));
        let data = fs::read(&path).unwrap_or_else(|e| panic!("Failed to read {}: {:?}", &path, e));

        let r12 = heap();

        let plen = r12 as *mut usize;
        let pstr = (r12 + 8) as *mut u8;

        allocate(8 + data.len());

        unsafe {
            // Write prefix length and then null terminated data
            std::ptr::write(plen, data.len());
            std::ptr::copy(data.as_ptr(), pstr, data.len());
        }

        // Return immediate encoded string object
        plen as i64 | STR
    }
}

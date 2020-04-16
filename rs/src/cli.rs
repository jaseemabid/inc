//! Command line interface for inc

use crate::{
    compiler::emit,
    core::{Config, Error, Expr},
    parser::parse,
};

use std::{fs::File, io::Write, path::PathBuf, process::Command};

#[derive(Copy, Clone)]
pub enum Action {
    Parse,
    GenASM,
    Run,
}

pub fn run(config: &Config, action: Action) -> Result<Option<String>, Error> {
    let prelude = parse(include_str!("prelude.ss"))?;
    let prog = parse(&config.program)?;
    let prog = prelude.into_iter().chain(prog.into_iter()).collect();

    match action {
        Action::Parse => {
            for e in prog {
                println!("{:?}", e);
            }

            Ok(None)
        }
        Action::GenASM => {
            gen(config, prog)?;

            Ok(None)
        }
        Action::Run => {
            gen(config, prog)?;
            build(&config)?;
            exec(&config)
        }
    }
}

pub fn gen<'a>(config: &'a Config, prog: Vec<Expr>) -> Result<(), Error<'a>> {
    let mut handler = File::create(&config.asm()).or_else(|e| {
        Err(Error::Internal { message: format!("Failed to create {}", &config.asm()), e: Some(e) })
    })?;

    handler.write_all(emit::program(prog).as_bytes()).or_else(|e| {
        Err(Error::Internal {
            message: format!("Failed to write to {}", &config.asm()),
            e: Some(e),
        })
    })?;

    Ok(())
}

/// Build the generated ASM with clang into executable binary
pub fn build(config: &Config) -> Result<(), Error> {
    let exe = Command::new("gcc")
        .arg("-m64")
        .arg("-g3")
        .arg("-ggdb3")
        .arg("-fomit-frame-pointer")
        .arg("-fno-asynchronous-unwind-tables")
        .arg("-L./target/debug")
        .arg("-O0")
        .arg("runtime.c")
        .arg(&config.asm())
        .arg("-linc")
        .arg("-ldl")
        .arg("-lpthread")
        .arg("-o")
        .arg(&config.output)
        .output()
        .expect("Failed to execute C compiler");

    if exe.status.success() {
        Ok(())
    } else {
        Err(Error::Internal {
            message: format!(
                "Failed to compile generated machine code. \n{}",
                String::from_utf8_lossy(&exe.stderr)
            ),
            e: None,
        })
    }
}

/// Run the generated binary and return output
// Cargo automatically sets the LD_LIBRARY_PATH, which is really convenient here
// because the generated binary is dynamically linked to an artifact in the
// target folder.
pub fn exec(config: &Config) -> Result<Option<String>, Error> {
    use std::os::unix::process::ExitStatusExt;

    let path = PathBuf::from(&config.output).canonicalize()?;

    // Command::output() returns an error only when spawning the process fails,
    // not for failed executions. When the child process segfaults, output
    // returns `Ok(empty stdout, empty stdin)` instead. Explicitly check for
    // status and construct an error. See
    // https://github.com/rust-lang/rust/issues/67391
    let exe = Command::new(&path).output()?;

    if exe.status.success() {
        Ok(Some(
            String::from_utf8_lossy(&exe.stdout).trim().to_string()
                + String::from_utf8_lossy(&exe.stderr).trim(),
        ))
    } else if exe.status.signal() == Some(6) {
        // SIGABRT is #defined as 6 in /usr/include/asm/signal.h
        //
        // Defined as `libc::SIGABRT` already, but no need to pull in a new crate
        // for just one constant.
        Err(Error::Runtime(
            String::from("Child program aborted with SIGABRT\n")
                + String::from_utf8_lossy(&exe.stdout).trim()
                + String::from_utf8_lossy(&exe.stderr).trim(),
        ))
    } else {
        Err(Error::Runtime(format!(
            "Child process failed with code: `{:?}` & signal: {:?}",
            exe.status.code(),
            exe.status.signal()
        )))
    }
}

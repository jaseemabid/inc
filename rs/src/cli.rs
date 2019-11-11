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
    let prog = parse(&config.program)?;

    match action {
        Action::Parse => {
            for e in prog {
                println!("{:?}", e);
            }

            Ok(None)
        }
        Action::GenASM => {
            gen(config, &prog)?;

            Ok(None)
        }
        Action::Run => {
            gen(config, &prog)?;
            build(&config)?;
            exec(&config)
        }
    }
}

pub fn gen<'a>(config: &'a Config, prog: &[Expr]) -> Result<(), Error<'a>> {
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
        .arg("-linc")
        .arg("-ldl")
        .arg("-lpthread")
        .arg("-O0")
        .arg("runtime.c")
        .arg(&config.asm())
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
    let path = PathBuf::from(&config.output).canonicalize()?;
    let exe = Command::new(&path).output()?;

    Ok(Some(String::from_utf8_lossy(&exe.stdout).trim().to_string()))
}

//! Command line interface for inc

use crate::{
    compiler::emit,
    core::{Config, Expr},
    parser,
};

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

/// Parse the program and return an Expr
pub fn parse(config: &Config) -> Vec<Expr> {
    parser::parse(&config.program).unwrap_or_else(|e| {
        panic!("Failed to parse input program `{}`: {:?}", config.program, e.message)
    })
}

/// Compile the program and write the assembly to target
pub fn compile(config: &Config) -> Result<(), std::io::Error> {
    let prog = parse(config);

    let mut handler = File::create(&config.asm())
        .unwrap_or_else(|_| panic!("Failed to create {}", &config.asm()));

    handler.write_all(emit::program(prog).as_bytes())
}

/// Build the generated ASM with clang into executable binary
pub fn build(config: &Config) -> bool {
    Command::new("gcc")
        .arg("-m64")
        .arg("-g3")
        .arg("-ggdb3")
        .arg("-fomit-frame-pointer")
        .arg("-fno-asynchronous-unwind-tables")
        .arg("-O0")
        .arg("stdlib.c")
        .arg("runtime.c")
        .arg(&config.asm())
        .arg("-o")
        .arg(&config.output)
        .status()
        .expect("Failed to compile binary")
        .success()
}

/// Run the generated binary and return output
pub fn run(config: &Config) -> Result<String, std::io::Error> {
    let path = PathBuf::from(&config.output).canonicalize().unwrap();
    let proc = Command::new(&path)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run binary `{}`; error: `{}`", &path.display(), e));

    Ok(String::from_utf8(proc.stdout).unwrap().trim().to_string())
}

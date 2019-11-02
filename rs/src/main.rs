extern crate getopts;
extern crate inc;

use getopts::Options;
use inc::{
    cli::{run, Action::*},
    core::Config,
};
use std::{
    env,
    io::{self, Read},
    process::exit,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let bin = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("o", "", "Output file name", "FILE");
    opts.optflag("S", "", "Print generated asm");
    opts.optflag("p", "", "Print parse tree");
    opts.optflag("h", "help", "print this help menu");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    let help = matches.opt_present("h");
    let parse = matches.opt_present("p");
    let asm = matches.opt_present("S");

    if help {
        print!("{}", opts.usage(&format!("Usage: {} [options]", bin)));
        return;
    }

    let output = matches
        .opt_str("o")
        .unwrap_or_else(|| String::from(if asm { "/dev/stdout" } else { "inc" }));

    let mut program = String::new();
    io::stdin().read_to_string(&mut program).expect("Expected a program in stdin");

    let config = Config { program, output };

    let action = if parse {
        Parse
    } else if asm {
        GenASM
    } else {
        Run
    };

    // Run the entire CLI with config
    match run(&config, action) {
        Err(e) => {
            println!("{}", e);
            exit(1)
        }
        Ok(Some(out)) => println!("{}", out),
        Ok(None) => {}
    }
}

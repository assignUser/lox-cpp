use std::env;
use std::process;

fn main() {
    let result = rox::cli::cli(env::args());

    match result {
        Ok(()) => (),
        Err(rox::LoxError::Interpreter(e)) => {
            eprintln!("{e}");
            process::exit(70)
        }
        Err(rox::LoxError::Parser(e)) => {
            eprintln!("{e}");
            process::exit(65)
        }
        Err(_e) => {
            // eprintln!("Application error: {e:#?}");
            process::exit(65);
        }
    }
}

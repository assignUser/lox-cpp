use std::env;
use std::backtrace::Backtrace;
use std::process;

fn main() {
    let result = rox::cli::cli(env::args());

    match result {
        Ok(()) => (),
        Err(e) => {
            eprintln!("Application error: {e:#?}");
            process::exit(1);
        }
    }
}

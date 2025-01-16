use std::env;
use std::process;

fn main() {
    let result = rox::cli::run(env::args());

    match result {
        Ok(()) => (),
        Err(e) => {
            eprintln!("Application error: {e}");
            process::exit(1);
        }
    }
}

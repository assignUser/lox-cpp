mod scanner;
use scanner::ScannerError;

#[derive(Debug)]
pub enum LoxError {
    Scanner(ScannerError),
    Io(std::io::Error),
}

pub mod cli {
    use crate::LoxError;

    use super::scanner;
    use std::io::{self, stdout, Write};

    pub fn cli(mut args: std::env::Args) -> Result<(), LoxError> {
        // skip bin path
        args.next();
        let files: Vec<String> = args.collect();

        if files.is_empty() {
            run_prompt()?;
        } else {
            for file in files {
                run_file(&file)?;
            }
        }
        Ok(())
    }

    /// REPL
    /// panics if it can't write to stdout or read stdin
    fn run_prompt() -> Result<(), LoxError> {
        let print_prompt = || {
            print!("> ");
            stdout().flush().expect("stdout should be writeable!");
        };

        for line in io::stdin().lines() {
            print_prompt();
            run(&line.expect("stdin should be readable!"))?;
            print_prompt();
        }

        Ok(())
    }

    fn run_file(path: &str) -> Result<(), LoxError> {
        let content = std::fs::read_to_string(path).map_err(LoxError::Io)?;

        run(&content)
    }

    fn run(source: &str) -> Result<(), LoxError> {
        let tokens = scanner::scan(source);
        for token in tokens.map_err(LoxError::Scanner)? {
            println!("{}", token.map_err(LoxError::Scanner)?);
        }

        println!("{}", scanner::Token::Eof);
        Ok(())
    }
}

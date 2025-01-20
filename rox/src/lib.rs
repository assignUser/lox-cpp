mod interpreter;
mod parser;
mod scanner;
use parser::ParserError;
use scanner::ScannerError;

#[derive(Debug)]
pub enum LoxError {
    Exit,
    Parser(ParserError),
    Scanner(ScannerError),
    Io(std::io::Error),
}

pub mod cli {
    use crate::interpreter::interpret;
    use crate::parser::Statement;
    use crate::scanner::Token;
    use crate::{interpreter, LoxError};
    use crate::{parser::Parser, scanner::ScannerError};

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

        print_prompt();
        for line in io::stdin().lines() {
            let _ = run(&line.expect("stdin should be readable!"));
            print_prompt();
        }

        Ok(())
    }

    fn run_file(path: &str) -> Result<(), LoxError> {
        let content = std::fs::read_to_string(path).map_err(LoxError::Io)?;

        run(&content)
    }

    fn run(source: &str) -> Result<(), LoxError> {
        use crate::interpreter::Interpretable;
        let tokens = scanner::scan(source).map_err(LoxError::Scanner)?;

        let errors: Vec<ScannerError> = tokens
            .iter()
            .filter_map(|e| Result::err(e.clone()))
            .collect();
        let tokens: Vec<Token> = tokens.into_iter().filter_map(Result::ok).collect();

        if !errors.is_empty() {
            errors.iter().for_each(|e| eprintln!("{}", &e));
            return Err(LoxError::Exit);
        }

        let mut parser = Parser::new(&tokens);
        let res = parser.parse().map_err(LoxError::Parser)?;
        for stmt in res {
           interpreter::interpret(stmt);
        }

        Ok(())
    }
}

use std::error::Error;
use std::io;

pub fn run(mut args: std::env::Args) -> Result<(), Box<dyn Error>> {
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

fn run_prompt() -> Result<(), Box<dyn Error>> {
    use std::io::{stdout, Write};
    let mut out = stdout();

    print!("> ");
    out.flush()?;

    for line in io::stdin().lines() {
        println!(">>> {}", line?);
        print!("> ");
        out.flush()?;
    }

    Ok(())
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let content = std::fs::read_to_string(path)?;
    println!(">>> {content}");
    Ok(())
}

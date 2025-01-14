#![allow(dead_code)]
// TODO remove
use logos::Logos;
use std::error::Error;
use std::io::{self, stdout, Write};
use std::num::ParseFloatError;

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
    let mut out = stdout();

    print!("> ");
    out.flush()?;

    for line in io::stdin().lines() {
        scan(&line?);
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

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // ignore white space
enum Token {
    // Single-character tokens.
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    // One or two character tokens.
    #[token("!")]
    Bang,

    #[token("!=")]
    BangEqual,

    #[token("=")]
    Equal,

    #[token("==")]
    Equalequal,

    #[token(">")]
    Greater,

    #[token(">=")]
    Greaterequal,

    #[token("<")]
    Less,

    #[token("<=")]
    Lessequal,

    // Literals.
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex("\"[^\"]*\"", lex_string)]
    String(String),

    #[regex(r"\d+(\.\d+)?", |lex| lex.slice().parse::<f64>()
        .expect("Number should be parsable!"))]
    Number(f64),

    // Keywords.
    #[token("and")]
    And,

    #[token("class")]
    Class,

    #[token("else")]
    Else,

    #[token("false")]
    False,

    #[token("fun")]
    Fun,

    #[token("for")]
    For,

    #[token("if")]
    If,

    #[token("nil")]
    Nil,

    #[token("or")]
    Or,

    #[token("print")]
    Print,

    #[token("return")]
    Return,

    #[token("super")]
    Super,

    #[token("this")]
    This,

    #[token("true")]
    True,

    #[token("var")]
    Var,

    #[token("while")]
    While,
}

fn lex_string(lexer: &mut logos::Lexer<Token>) -> String {
    let quoted_string = lexer.slice();
    quoted_string
        .strip_prefix('"')
        .expect("Strings have to be quoted to be lexed!")
        .strip_suffix('"')
        .expect("Strings have to be quoted to be lexed!")
        .to_string()
}

fn lex_number(lexer: &mut logos::Lexer<Token>) -> Result<f64, ParseFloatError> {
    lexer.slice().parse::<f64>()
}

enum Literal<'a> {
    String(&'a str),
    Double(i32),
}

pub struct Source<'a> {
    source: &'a str,
    pos: usize,
    line: i32,
    col: i32,
}

struct SourceLocation {
    line: i32,
    col: i32,
}

// pub struct Token<'a> {
//     r#type: TokenType,
//     lexeme: &'a str,
//     literal: Literal<'a>,
//     source: SourceLocation,
// }

fn scan(source: &str) {
    let lex = Token::lexer(source);
    for t in lex {
        dbg!(t.expect("blub"));
    }
}
mod lexer {
    // Lexical Grammar for Lox
    // NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
    // STRING         → "\"" <any char except "\"">* "\"" ;
    // IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
    // ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
    // DIGIT          → "0" ... "9" ;
}

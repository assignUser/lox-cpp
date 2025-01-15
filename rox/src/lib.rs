#![allow(dead_code)]
// TODO remove
use logos::Logos;
use logos::Skip;
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
#[logos(skip "//.*")] // ignore Comments
#[logos(skip r"[ \t\f]+")] // ignore white space
#[logos(extras = LexerPos)]
#[logos(error = LexingError)]
enum Token {
    #[token(r"\n", lex_newline, priority = 100)]
    NewLine,

    // Single-character tokens.
    #[token("(", lex_column)]
    LeftParen,

    #[token(")", lex_column)]
    RightParen,

    #[token("{", lex_column)]
    LeftBrace,

    #[token("}", lex_column)]
    RightBrace,

    #[token(",", lex_column)]
    Comma,

    #[token(".", lex_column)]
    Dot,

    #[token("-", lex_column)]
    Minus,

    #[token("+", lex_column)]
    Plus,

    #[token(";", lex_column)]
    Semicolon,

    #[token("/", lex_column)]
    Slash,

    #[token("*", lex_column)]
    Star,

    // One or two character tokens.
    #[token("!", lex_column)]
    Bang,

    #[token("!=", lex_column)]
    BangEqual,

    #[token("=", lex_column)]
    Equal,

    #[token("==", lex_column)]
    Equalequal,

    #[token(">", lex_column)]
    Greater,

    #[token(">=", lex_column)]
    Greaterequal,

    #[token("<", lex_column)]
    Less,

    #[token("<=", lex_column)]
    Lessequal,

    // Literals.
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", |lex| lex_column(lex); lex.slice().to_string())]
    Identifier(String),

    #[regex(r#""[^"\n]*"?"#, lex_string)]
    String(String),
    // #[regex("\"[^\"\n]*", lex_column)]
    // UnterminatedString,
    #[regex(r"\d+(\.\d+)?", |lex| lex_column(lex); lex.slice().parse::<f64>()
        .expect("Number should be parsable!"))]
    Number(f64),

    #[token("false", |lex| lex_column(lex); false)]
    #[token("true", |lex| lex_column(lex); true)]
    Bool(bool),

    // Keywords.
    #[token("and", lex_column)]
    And,

    #[token("class", lex_column)]
    Class,

    #[token("else", lex_column)]
    Else,

    #[token("fun", lex_column)]
    Fun,

    #[token("for", lex_column)]
    For,

    #[token("if", lex_column)]
    If,

    #[token("nil", lex_column)]
    Nil,

    #[token("or", lex_column)]
    Or,

    #[token("print", lex_column)]
    Print,

    #[token("return", lex_column)]
    Return,

    #[token("super", lex_column)]
    Super,

    #[token("this", lex_column)]
    This,

    #[token("var", lex_column)]
    Var,

    #[token("while", lex_column)]
    While,
}
#[derive(PartialEq, Clone, Debug)]
struct LexerPos {
    line: usize,
    col: usize,
    newline: usize,
}
impl Default for LexerPos {
    fn default() -> Self {
        LexerPos {
            line: 1,
            col: 1,
            newline: 0,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
struct SourcePos {
    line: usize,
    col: usize,
}

impl Default for SourcePos {
    fn default() -> Self {
        SourcePos { line: 1, col: 1 }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum LexingError {
    UnterminatedString(SourcePos),
    UnexpectedCharacter(SourcePos),
}

// impl From<LexingError::UnexpectedCharacter> for LexingError{
// fn from(value: LexingError::UnexpectedCharacter) -> Self {
// value
//     }
// }

impl Default for LexingError {
    fn default() -> Self {
        LexingError::UnexpectedCharacter(SourcePos::default())
    }
}
fn lex_space(lexer: &mut logos::Lexer<Token>) -> Skip {
    lex_column(lexer);
    Skip
}

fn lex_newline(lexer: &mut logos::Lexer<Token>) -> Skip {
    dbg!(&lexer.extras);
    lexer.extras.line += 1;
    lexer.extras.newline = lexer.span().end;
    Skip
}

fn lex_column(lexer: &mut logos::Lexer<Token>) {
    // dbg!(&lexer.extras);
    // dbg!(&lexer.span().start);
    lexer.extras.col = lexer.span().start - lexer.extras.newline;
}

fn lex_string(lexer: &mut logos::Lexer<Token>) -> Result<String, LexingError> {
    lex_column(lexer);
    fn untermintated_string(lexer: &mut logos::Lexer<Token>) -> LexingError {
        LexingError::UnterminatedString(SourcePos {
            line: lexer.extras.line,
            col: lexer.extras.col,
        })
    };

    let quoted_string = lexer.slice();
    Ok(quoted_string
        .strip_prefix('"')
        .expect("Strings have to be quoted to be lexed!")
        .strip_suffix('"')
        .ok_or(untermintated_string(lexer))?
        .to_string())
}

fn lex_number(lexer: &mut logos::Lexer<Token>) -> Result<f64, ParseFloatError> {
    lex_column(lexer);
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

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;
    type ResultToken<'a> = Result<Token, <Token as logos::Logos<'a>>::Error>;

    #[test]
    fn lexer_erros() {
        let unterminated_string = Token::lexer(
            "var words = \"This is an unterminated String\nvar b = 42;\n\"another one",
        )
        .collect::<Vec<ResultToken>>();
        let expected: Vec<ResultToken> = vec![
            Ok(Var),
            Ok(Identifier("words".to_owned())),
            Ok(Equal),
            Err(LexingError::UnterminatedString(SourcePos {
                col: 12,
                line: 1,
            })),
            Ok(Var),
            Ok(Identifier("b".to_owned())),
            Ok(Equal),
            Ok(Number(42.0)),
            Ok(Semicolon),
            Err(LexingError::UnterminatedString(SourcePos {
                col: 1,
                line: 3,
            })),
        ];
        assert_eq!(unterminated_string, expected);
    }

    #[test]
    fn lexer_basic() {
        let lex = Token::lexer("if (true) { print(\"something\");}\n//@ß ignored\nvar a = 5.5;");
        let expected: Vec<ResultToken> = vec![
            Ok(If),
            Ok(LeftParen),
            Ok(Bool(true)),
            Ok(RightParen),
            Ok(LeftBrace),
            Ok(Print),
            Ok(LeftParen),
            Ok(String("something".to_owned())),
            Ok(RightParen),
            Ok(Semicolon),
            Ok(RightBrace),
            Ok(Var),
            Ok(Identifier("a".to_owned())),
            Ok(Equal),
            Ok(Number(5.5)),
            Ok(Semicolon),
        ];
        assert_eq!(
            expected,
            lex.collect::<Vec<Result<Token, <Token as logos::Logos>::Error>>>()
        );
    }
}

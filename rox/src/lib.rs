#![allow(dead_code)]
// TODO remove

pub mod cli {
    use super::scanner;
    use std::error::Error;
    use std::io::{self, stdout, Write};

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
            scanner::scan(&line?);

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
}

mod scanner {
    use std::fmt::Display;

    pub fn scan(source: &str) -> Result<(), ScannerError> {
        let source = AsciiSource::build(source)?;
        dbg!(source);

        Ok(())
    }

    ///
    #[derive(Debug)]
    pub struct AsciiSource<'a> {
        source: &'a [u8],
        curr: usize,
    }
    impl<'a> std::ops::Index<usize> for AsciiSource<'a> {
        type Output = u8;
        fn index(&self, index: usize) -> &Self::Output {
            &self.source[index]
        }
    }

    impl<'a> AsciiSource<'a> {
        /// Cut the first n chars of source
        pub fn shift(&self, n: usize) -> AsciiSource {
            AsciiSource {
                source: &self.source[n..],
                curr: 0,
            }
        }
        pub fn source(&self) -> &[u8] {
            &self.source
        }

        pub fn build(source: &str) -> Result<AsciiSource, ScannerError> {
            source
                .is_ascii()
                .then(|| ())
                .ok_or(ScannerError::NonAsciiCharacer)?;

            Ok(AsciiSource {
                source: source.as_bytes(),
                curr: 0,
            })
        }
    }

    // impl<'a> Iterator for AsciiSource<'a> {
    //     type Item = &'a u8;
    //
    //     fn next(&mut self) -> Option<Self::Item> {
    //         self.curr += 1;
    //         self.source.as_bytes().get(self.curr)
    //     }
    // }

    #[derive(Debug, Default, PartialEq)]
    pub enum ScannerError {
        #[default]
        NonAsciiCharacer,
    }

    // Lexical Grammar for Lox
    // NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
    // STRING         → "\"" <any char except "\"">* "\"" ;
    // IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
    // ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
    // DIGIT          → "0" ... "9" ;

    fn numbers(source: AsciiSource) {}
    /// Identifiers with reserved meaning
    #[derive(Debug, PartialEq)]
    enum Keywords {
        And,
        Class,
        Else,
        False,
        Fun,
        For,
        If,
        Nil,
        Or,
        Print,
        Return,
        Super,
        This,
        True,
        Var,
        While,
    }

    impl Display for Keywords {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Keywords::And => write!(f, "and"),
                Keywords::Class => write!(f, "class"),
                Keywords::Else => write!(f, "else"),
                Keywords::False => write!(f, "false"),
                Keywords::Fun => write!(f, "fun"),
                Keywords::For => write!(f, "for"),
                Keywords::If => write!(f, "if"),
                Keywords::Nil => write!(f, "nil"),
                Keywords::Or => write!(f, "or"),
                Keywords::Print => write!(f, "print"),
                Keywords::Return => write!(f, "return"),
                Keywords::Super => write!(f, "super"),
                Keywords::This => write!(f, "this"),
                Keywords::True => write!(f, "true"),
                Keywords::Var => write!(f, "var"),
                Keywords::While => write!(f, "while"),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Operators {
        Minus,
        Plus,
        Semicolon,
        Slash,
        Star,
        Bang,
        BangEqual,
        Equal,
        Equalequal,
        Greater,
        Greaterequal,
        Less,
        Lessequal,
    }

    impl Display for Operators {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Operators::Minus => write!(f, "-"),
                Operators::Plus => write!(f, "+"),
                Operators::Semicolon => write!(f, ";"),
                Operators::Slash => write!(f, "/"),
                Operators::Star => write!(f, "*"),
                Operators::Bang => write!(f, "!"),
                Operators::BangEqual => write!(f, "!="),
                Operators::Equal => write!(f, "="),
                Operators::Equalequal => write!(f, "=="),
                Operators::Greater => write!(f, ">"),
                Operators::Greaterequal => write!(f, ">="),
                Operators::Less => write!(f, "<"),
                Operators::Lessequal => write!(f, "<="),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Symbols {
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        Comma,
        Dot,
    }
    impl Display for Symbols {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Symbols::LeftParen => write!(f, "("),
                Symbols::RightParen => write!(f, ")"),
                Symbols::LeftBrace => write!(f, "{{"),
                Symbols::RightBrace => write!(f, "}}"),
                Symbols::Comma => write!(f, ","),
                Symbols::Dot => write!(f, "."),
            }
        }
    }

    enum Literals {
        Identifier(String),
        String(String),
        Number(f64),
    }
    impl Display for Literals {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Literals::Identifier(s) | Literals::String(s) => write!(f, "{s}"),
                Literals::Number(n) => write!(f, "{n}"),
            }
        }
    }
}

#[cfg(test)]
mod scanner_tests {
    use super::scanner;

    #[test]
    fn scan_errors() {
        assert_eq!(
            scanner::scan("hallö"),
            Err(scanner::ScannerError::NonAsciiCharacer)
        )
    }

    #[test]
    fn ascii_source() {
        let src = scanner::AsciiSource::build("123456").unwrap();
        assert_eq!(src.shift(2).source(), [b'3', b'4', b'5', b'6']);


        
    }
}

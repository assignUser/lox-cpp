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
    type ScannerResult = Result<Token, ScannerError>;

    pub fn scan(source: &str) -> Result<(), ScannerError> {
        let source = AsciiSource::build(source)?;
        dbg!(source);

        Ok(())
    }

    #[derive(Debug, PartialEq)]
    pub struct SourcePos {
        row: usize,
        col: usize,
    }
    #[derive(Debug)]
    pub struct AsciiSource<'a> {
        source: &'a [u8],
        curr: usize,
        len: usize,
    }

    impl<'a> std::ops::Index<usize> for AsciiSource<'a> {
        type Output = u8;
        fn index(&self, index: usize) -> &Self::Output {
            // adjust index for current 'head'
            let index = self.curr + index;
            if index < self.curr || index >= self.len {
                panic!(
                    "index out of bounds: the len is {len} but the index is {index} ",
                    len = self.len(),
                    index = index - self.curr
                );
            }
            &self.source[index]
        }
    }

    impl<'a> AsciiSource<'a> {
        /// Move the current head n chars towards the end. n will be clamped to len(source)
        /// to prevent panics, this will return an empty slice.
        pub fn shift(&mut self, n: usize) {
            if n == 0 {
                return;
            }

            self.curr = (self.curr + n).clamp(0, self.len);
        }

        pub fn is_empty(&self) -> bool {
            self.curr == self.len
        }

        pub fn source(&self) -> &[u8] {
            &self.source[self.curr.clamp(0, self.len - 1)..]
        }
        pub fn len(&self) -> usize {
            if self.is_empty() {
                0
            } else {
                self.len - self.curr
            }
        }
        fn current_pos(&self) -> SourcePos {
            // 1-indexing is normal for page positions
            let mut row = 1;
            let mut col = 1;

            for (i, b) in self.source.iter().enumerate() {
                if i == self.curr {
                    break;
                }
                if *b == b'\n' {
                    // On a newline, increment the row and reset the column.
                    row += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }

            SourcePos {
                row,
                col: col.clamp(0, self.len),
            }
        }

        pub fn build(source: &str) -> Result<AsciiSource, ScannerError> {
            source
                .is_ascii()
                .then(|| ())
                .ok_or(ScannerError::NonAsciiCharacer)?;

            Ok(AsciiSource {
                source: source.as_bytes(),
                curr: 0,
                len: source.len(),
            })
        }
    }

    pub struct Scanner<'a> {
        source: AsciiSource<'a>,
    }

    impl<'a> Scanner<'a> {
        fn peek(&self, n: usize) -> Option<u8> {
            if n >= self.source.len() {
                return None;
            }

            Some(self.source[n])
        }

        fn space(&mut self) {
            loop {
                match self.peek(0).unwrap_or(b'a') {
                    b' ' | b'\n' | b'\t' | b'\r' => (),
                    _ => break,
                }
                self.source.shift(1);
            }
        }

        fn numbers(&mut self) -> Option<ScannerResult> {
            let mut digits = String::new();
            macro_rules! number_loop {
                () => {
                    loop {
                        match self.peek(0) {
                            Some(d) => {
                                if d.is_ascii_digit() {
                                    self.source.shift(1);
                                    digits.push(d as char);
                                } else {
                                    break;
                                }
                            }
                            None => break,
                        }
                    }
                };
            }

            if self.source.is_empty() {
                return None;
            }

            number_loop!();

            match self.peek(0) {
                Some(b'.') => {
                    digits.push('.');
                    self.source.shift(1);
                    if !self.peek(0).map(|d| d.is_ascii_digit()).unwrap_or(false) {
                        return Some(Err(ScannerError::UnexpectedCharacter(
                            self.source.current_pos(),
                        )));
                    }
                    number_loop!();
                }
                Some(_) => (),
                None => (),
            }

            let a = digits.parse::<f64>();
            Some(
                a.map_err(|_| ScannerError::NumberParsingError)
                    .map(|n| Token::Number(n)),
            )

            // Some(Err(ScannerError::default()))
        }
    }

    impl<'a> Iterator for Scanner<'a> {
        type Item = ScannerResult;
        fn next(&mut self) -> Option<Self::Item> {
            self.space();
            self.numbers() //.or_else(|| self.numbers())
        }
    }

    #[derive(Debug, Default, PartialEq)]
    pub enum ScannerError {
        #[default]
        NonAsciiCharacer,
        NumberParsingError,
        UnexpectedCharacter(SourcePos),
        UnterminatedString(SourcePos),
    }

    // Lexical Grammar for Lox
    // NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
    // STRING         → "\"" <any char except "\"">* "\"" ;
    // IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
    // ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
    // DIGIT          → "0" ... "9" ;
    type ScannerResults = Vec<Result<Token, ScannerError>>;

    #[derive(Debug, PartialEq)]
    pub enum Token {
        // Keywords
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
        // Operators
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
        // Symbols
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        Comma,
        Dot,
        // Literals
        Identifier(String),
        String(String),
        Number(f64),
    }

    impl Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Token::LeftParen => write!(f, "("),
                Token::RightParen => write!(f, ")"),
                Token::LeftBrace => write!(f, "{{"),
                Token::RightBrace => write!(f, "}}"),
                Token::Comma => write!(f, ","),
                Token::Dot => write!(f, "."),
                Token::Minus => write!(f, "-"),
                Token::Plus => write!(f, "+"),
                Token::Semicolon => write!(f, ";"),
                Token::Slash => write!(f, "/"),
                Token::Star => write!(f, "*"),
                Token::Bang => write!(f, "!"),
                Token::BangEqual => write!(f, "!="),
                Token::Equal => write!(f, "="),
                Token::Equalequal => write!(f, "=="),
                Token::Greater => write!(f, ">"),
                Token::Greaterequal => write!(f, ">="),
                Token::Less => write!(f, "<"),
                Token::Lessequal => write!(f, "<="),
                Token::And => write!(f, "and"),
                Token::Class => write!(f, "class"),
                Token::Else => write!(f, "else"),
                Token::False => write!(f, "false"),
                Token::Fun => write!(f, "fun"),
                Token::For => write!(f, "for"),
                Token::If => write!(f, "if"),
                Token::Nil => write!(f, "nil"),
                Token::Or => write!(f, "or"),
                Token::Print => write!(f, "print"),
                Token::Return => write!(f, "return"),
                Token::Super => write!(f, "super"),
                Token::This => write!(f, "this"),
                Token::True => write!(f, "true"),
                Token::Var => write!(f, "var"),
                Token::While => write!(f, "while"),
                Token::Identifier(s) | Token::String(s) => write!(f, "{s}"),
                Token::Number(n) => write!(f, "{n}"),
            }
        }
    }

    #[cfg(test)]
    mod scanner_tests {
        use super::*;
        use ScannerResult as SR;

        #[test]
        fn scan_errors() {
            assert_eq!(scan("hallö"), Err(ScannerError::NonAsciiCharacer))
        }

        #[test]
        fn ascii_source_peek() {
            let mut scanner = Scanner {
                source: AsciiSource::build("abc").unwrap(),
            };
            assert_eq!(scanner.peek(0), Some(b'a'));
            assert_eq!(scanner.peek(2), Some(b'c'));
            assert_eq!(scanner.peek(4), None);
        }

        #[test]
        fn ascii_source_access() {
            let mut empty = AsciiSource::build("").unwrap();
            let mut not_empty = AsciiSource::build("abc").unwrap();
            assert!(empty.is_empty());

            empty.shift(12);
            assert!(empty.is_empty());
            assert_eq!(empty.len(), 0);

            // [a, b, c] curr = 0
            //  ^         shift(1)
            // [a, b, c] curr = 1
            //     ^
            //     should act like [b, c]
            assert!(!not_empty.is_empty());
            assert_eq!(not_empty.len(), 3);
            not_empty.shift(1);
            assert_eq!(not_empty[0], b'b');
            assert_eq!(not_empty.len(), 2);

            not_empty.shift(2);
            assert!(not_empty.is_empty());
            assert_eq!(not_empty.len(), 0);

            let mut not_empty = AsciiSource::build("abc").unwrap();
            not_empty.shift(2);
            assert_eq!(not_empty.len(), 1);
            assert_eq!(not_empty[0], b'c');
            not_empty.shift(1);
            assert!(not_empty.is_empty());
        }

        #[test]
        #[should_panic]
        fn ascii_panic_index() {
            let mut not_empty = AsciiSource::build("abc").unwrap();
            not_empty.shift(3);
            not_empty[0];
        }

        #[test]
        fn ascii_source() {
            let mut src = AsciiSource::build("123456").unwrap();
            src.shift(2);
            assert_eq!(src.source(), [b'3', b'4', b'5', b'6']);
            src.shift(2);
            assert_eq!(src.source(), [b'5', b'6']);
        }

        #[test]
        fn scan_numbers() {
            let mut empty_scanner = Scanner {
                source: AsciiSource::build("").unwrap(),
            };
            assert_eq!(empty_scanner.next(), None);

            let mut scanner = Scanner {
                source: AsciiSource::build("42 3.14 3.0 0.5 0.").unwrap(),
            };
            assert_eq!(scanner.next().unwrap(), SR::Ok(Token::Number(42.0)));
            assert_eq!(scanner.next().unwrap(), SR::Ok(Token::Number(3.14)));
            assert_eq!(scanner.next().unwrap(), SR::Ok(Token::Number(3.0)));
            assert_eq!(scanner.next().unwrap(), SR::Ok(Token::Number(0.5)));
            assert_eq!(
                scanner.next().unwrap(),
                SR::Err(ScannerError::UnexpectedCharacter(SourcePos {
                    row: 1,
                    col: 18
                }))
            );
        }
    }
}

use std::fmt;
use std::fmt::Display;

type ScannerResult = Result<Token, ScannerError>;

pub fn scan(source: &str) -> Result<ScannerResults, ScannerError> {
    let scanner = Scanner::build(source)?;
    Ok(scanner.collect::<ScannerResults>())
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct SourcePos {
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.row, self.col)
    }
}

#[derive(Debug)]
pub struct AsciiSource<'a> {
    source: &'a [u8],
    curr: usize,
    len: usize,
}

impl std::ops::Index<usize> for AsciiSource<'_> {
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

impl AsciiSource<'_> {
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

    pub fn len(&self) -> usize {
        if self.is_empty() {
            0
        } else {
            self.len - self.curr
        }
    }

    fn current_pos(&self, offset: usize) -> SourcePos {
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
            col: (col - offset).clamp(0, self.len),
        }
    }

    pub fn build(source: &str) -> Result<AsciiSource, ScannerError> {
        source
            .is_ascii()
            .then_some(())
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

macro_rules! token_from_str {
       ($input:expr,$pos:expr, $( $Variant:ident(SourcePos) ),+ $(,)?) => {{
            let s = $input;
            let mut found = None;
            $(
                // Construct a temp instance of the variant using `Default`
                // and compare its Display output to the input string.
                if *s == *format!("{}", Token::$Variant(<SourcePos>::default())).split(' ').nth(1).unwrap_or(" ") {
                    found = Some(<Token>::$Variant($pos.clone()));
                }
            )+


            found
        }};
    }

impl Scanner<'_> {
    // Lexical Grammar for Lox
    // NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
    // STRING         → "\"" <any char except "\"">* "\"" ;
    // IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
    // ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
    // DIGIT          → "0" ... "9" ;
    fn build(source: &str) -> Result<Scanner, ScannerError> {
        Ok(Scanner {
            source: AsciiSource::build(source)?,
        })
    }
    fn peek(&self, n: usize) -> Option<u8> {
        if n >= self.source.len() {
            return None;
        }

        Some(self.source[n])
    }

    fn skip_space(&mut self) -> Option<()> {
        let mut res = None;
        while let b' ' | b'\n' | b'\t' | b'\r' = self.peek(0).unwrap_or(b'a') {
            self.source.shift(1);
            res = Some(());
        }
        res
    }

    fn ignore_comments(&mut self) -> Option<()> {
        let mut res = None;

        if self.source.is_empty() || self.source.len() < 2 {
            return res;
        }

        let comment_guard = format!(
            "{}{}",
            self.peek(0).expect("length checked!") as char,
            self.peek(1).expect("length checked!") as char
        );

        if comment_guard == "//" {
            res = Some(());
            loop {
                match self.peek(0) {
                    None => break,
                    Some(b'\n') => {
                        self.source.shift(1);
                        break;
                    }
                    Some(_) => self.source.shift(1),
                }
            }
        }
        res
    }

    fn symbols(&mut self) -> Option<ScannerResult> {
        if self.source.is_empty() {
            return None;
        }

        let mut op = format!(
            "{}{}",
            self.peek(0).expect("length checked!") as char,
            self.peek(1).unwrap_or(b' ') as char
        );

        token_from_str!(
            &op,
            self.source.current_pos(0),
            BangEqual(SourcePos),
            EqualEqual(SourcePos),
            LessEqual(SourcePos),
            GreaterEqual(SourcePos)
        )
        .map(|t| {
            self.source.shift(2);
            Ok(t)
        })
        .or_else(|| {
            op.truncate(1);
            token_from_str!(
                op,
                self.source.current_pos(0),
                Minus(SourcePos),
                Plus(SourcePos),
                Semicolon(SourcePos),
                Slash(SourcePos),
                Star(SourcePos),
                Bang(SourcePos),
                Equal(SourcePos),
                Greater(SourcePos),
                Less(SourcePos),
                LeftParen(SourcePos),
                RightParen(SourcePos),
                LeftBrace(SourcePos),
                RightBrace(SourcePos),
                Comma(SourcePos),
                Dot(SourcePos)
            )
            .map(|t| {
                self.source.shift(1);
                Ok(t)
            })
        })
    }

    fn identifiers(&mut self) -> Option<ScannerResult> {
        fn alpha(d: &u8) -> bool {
            d.is_ascii_alphabetic() || *d == b'_'
        }

        fn alpha_num(d: &u8) -> bool {
            alpha(d) || d.is_ascii_digit()
        }

        if self.source.is_empty() || self.peek(0).map(|d| !alpha(&d)).unwrap_or(false) {
            return None;
        }

        let mut identifier = String::new();

        //Consume first letter
        identifier.push(self.peek(0).expect("Element peeked before!") as char);
        self.source.shift(1);

        loop {
            match self.peek(0) {
                None => break,

                Some(d) => {
                    if alpha_num(&d) {
                        identifier.push(d as char);
                        self.source.shift(1);
                    } else {
                        break;
                    }
                }
            }
        }

        let keyword = token_from_str!(
            &identifier,
            self.source.current_pos(identifier.len()),
            And(SourcePos),
            Class(SourcePos),
            Else(SourcePos),
            False(SourcePos),
            Fun(SourcePos),
            For(SourcePos),
            If(SourcePos),
            Nil(SourcePos),
            Or(SourcePos),
            Print(SourcePos),
            Return(SourcePos),
            Super(SourcePos),
            This(SourcePos),
            True(SourcePos),
            Var(SourcePos),
            While(SourcePos)
        );

        match keyword {
            None => Some(Ok(Token::Identifier {
                pos: self.source.current_pos(identifier.len()),
                ident: identifier,
            })),
            Some(k) => Some(Ok(k)),
        }
    }

    fn strings(&mut self) -> Option<ScannerResult> {
        if self.source.is_empty() || self.peek(0).map(|d| d != b'"').unwrap_or(false) {
            return None;
        }

        //Consume "
        self.source.shift(1);

        let mut string = String::new();

        loop {
            match self.peek(0) {
                Some(b'"') => {
                    self.source.shift(1);
                    break;
                }
                None | Some(b'\n') => {
                    return Some(Err(ScannerError::UnterminatedString(
                        self.source.current_pos(0),
                    )));
                }
                Some(d) => {
                    string.push(d as char);
                    self.source.shift(1);
                }
            }
        }

        Some(Ok(Token::String {
            pos: self.source.current_pos(string.len() + 2), // add 2 for the quotes
            string,
        }))
    }

    fn numbers(&mut self) -> Option<ScannerResult> {
        if self.source.is_empty() || self.peek(0).map(|d| !d.is_ascii_digit()).unwrap_or(false) {
            return None;
        }

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

        number_loop!();

        match self.peek(0) {
            Some(b'.') => {
                if self.peek(1).map(|d| d.is_ascii_digit()).unwrap_or(false) {
                    digits.push('.');
                    self.source.shift(1);
                    number_loop!();
                }
            }
            Some(_) => (),
            None => (),
        }

        if digits.is_empty() {
            return None;
        }

        Some(
            digits
                .parse::<f64>()
                .map_err(|_| ScannerError::NumberParsingError)
                .map(|v| Token::Number {
                    value: v,
                    pos: self.source.current_pos(digits.len()),
                }),
        )
    }
}

impl Iterator for Scanner<'_> {
    type Item = ScannerResult;
    fn next(&mut self) -> Option<Self::Item> {
        let mut skip = || {
            let comments = self.ignore_comments();
            let whitespace = self.skip_space();
            if comments.is_some() || whitespace.is_some() {
                Some(())
            } else {
                None
            }
        };

        while let Some(()) = skip() {}

        self.numbers()
            .or_else(|| self.strings())
            .or_else(|| self.identifiers())
            .or_else(|| self.symbols())
            .or_else(|| {
                if self.source.is_empty() {
                    // if all parsers ran and the result is none the source has to be empty
                    None
                } else {
                    // or unparsable characters where found
                    let res = Some(Err(ScannerError::UnexpectedCharacter(
                        self.source.current_pos(0),
                    )));
                    // consume the character
                    self.source.shift(1);

                    res
                }
            })
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum ScannerError {
    #[default]
    NonAsciiCharacer,
    NumberParsingError,
    UnexpectedCharacter(SourcePos),
    UnterminatedString(SourcePos),
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScannerError::NonAsciiCharacer => {
                write!(f, "Non-ASCII character encountered")
            }
            ScannerError::NumberParsingError => {
                write!(f, "Number parsing error")
            }
            ScannerError::UnexpectedCharacter(pos) => {
                write!(f, "Unexpected character at {}", pos)
            }
            ScannerError::UnterminatedString(pos) => {
                write!(f, "Unterminated string at {}", pos)
            }
        }
    }
}

type ScannerResults = Vec<Result<Token, ScannerError>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Keywords
    And(SourcePos),
    Class(SourcePos),
    Else(SourcePos),
    False(SourcePos),
    Fun(SourcePos),
    For(SourcePos),
    If(SourcePos),
    Nil(SourcePos),
    Or(SourcePos),
    Print(SourcePos),
    Return(SourcePos),
    Super(SourcePos),
    This(SourcePos),
    True(SourcePos),
    Var(SourcePos),
    While(SourcePos),
    // Operators
    Minus(SourcePos),
    Plus(SourcePos),
    Semicolon(SourcePos),
    Slash(SourcePos),
    Star(SourcePos),
    Bang(SourcePos),
    BangEqual(SourcePos),
    Equal(SourcePos),
    EqualEqual(SourcePos),
    Greater(SourcePos),
    GreaterEqual(SourcePos),
    Less(SourcePos),
    LessEqual(SourcePos),
    // Symbols
    LeftParen(SourcePos),
    RightParen(SourcePos),
    LeftBrace(SourcePos),
    RightBrace(SourcePos),
    Comma(SourcePos),
    Dot(SourcePos),
    // Literals
    Identifier { ident: String, pos: SourcePos },
    String { string: String, pos: SourcePos },
    Number { value: f64, pos: SourcePos },
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen(_pos) => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen(_pos) => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace(_pos) => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace(_pos) => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma(_pos) => write!(f, "COMMA , null"),
            Token::Dot(_pos) => write!(f, "DOT . null"),
            Token::Minus(_pos) => write!(f, "MINUS - null"),
            Token::Plus(_pos) => write!(f, "PLUS + null"),
            Token::Semicolon(_pos) => write!(f, "SEMICOLON ; null"),
            Token::Slash(_pos) => write!(f, "SLASH / null"),
            Token::Star(_pos) => write!(f, "STAR * null"),
            Token::Bang(_pos) => write!(f, "BANG ! null"),
            Token::BangEqual(_pos) => write!(f, "BANG_EQUAL != null"),
            Token::Equal(_pos) => write!(f, "EQUAL = null"),
            Token::EqualEqual(_pos) => write!(f, "EQUAL_EQUAL == null"),
            Token::Greater(_pos) => write!(f, "GREATER > null"),
            Token::GreaterEqual(_pos) => write!(f, "GREATER_EQUAL >= null"),
            Token::Less(_pos) => write!(f, "LESS < null"),
            Token::LessEqual(_pos) => write!(f, "LESS_EQUAL <= null"),
            Token::And(_pos) => write!(f, "AND and null"),
            Token::Class(_pos) => write!(f, "CLASS class null"),
            Token::Else(_pos) => write!(f, "ELSE else null"),
            Token::False(_pos) => write!(f, "FALSE false null"),
            Token::Fun(_pos) => write!(f, "FUN fun null"),
            Token::For(_pos) => write!(f, "FOR for null"),
            Token::If(_pos) => write!(f, "IF if null"),
            Token::Nil(_pos) => write!(f, "NIL nil null"),
            Token::Or(_pos) => write!(f, "OR or null"),
            Token::Print(_pos) => write!(f, "PRINT print null"),
            Token::Return(_pos) => write!(f, "RETURN return null"),
            Token::Super(_pos) => write!(f, "SUPER super null"),
            Token::This(_pos) => write!(f, "THIS this null"),
            Token::True(_pos) => write!(f, "TRUE true null"),
            Token::Var(_pos) => write!(f, "VAR var null"),
            Token::While(_pos) => write!(f, "WHILE while null"),
            Token::Identifier { ident, pos: _ } => write!(f, "IDENTIFIER {ident} null"),
            Token::String { string, pos: _ } => write!(f, "STRING \"{string}\" {string}"),
            Token::Number { value, pos: _ } => {
                // This is to match jlox's expectation
                if value.fract() == 0.0 {
                    write!(f, "NUMBER {} {}.0", value, *value as i64)
                } else {
                    write!(f, "NUMBER {value} {value}")
                }
            }
            Token::Eof => write!(f, "EOF  null"),
        }
    }
}

#[cfg(test)]
mod scanner_tests {
    use super::*;
    use ScannerResult as SR;

    #[test]
    fn scan_errors() {
        assert_eq!(scan("hallö"), Err(ScannerError::NonAsciiCharacer));
        let mut errors = Scanner {
            source: AsciiSource::build("@@\n #").unwrap(),
        };
        assert_eq!(
            errors.next(),
            Some(Err(ScannerError::UnexpectedCharacter(SourcePos {
                row: 1,
                col: 1
            })))
        );
        assert_eq!(
            errors.next(),
            Some(Err(ScannerError::UnexpectedCharacter(SourcePos {
                row: 1,
                col: 2
            })))
        );
        assert_eq!(
            errors.next(),
            Some(Err(ScannerError::UnexpectedCharacter(SourcePos {
                row: 2,
                col: 2
            })))
        );
    }

    #[test]
    fn ascii_source_peek() {
        let scanner = Scanner {
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
        let _ = not_empty[0];
    }

    #[test]
    fn scan_numbers() {
        let mut empty_scanner = Scanner {
            source: AsciiSource::build("").unwrap(),
        };
        assert_eq!(empty_scanner.next(), None);

        let mut scanner = Scanner {
            source: AsciiSource::build("42 3.15 3.0 0.5 0.").unwrap(),
        };

        assert_eq!(
            scanner.next(),
            Some(SR::Ok(Token::Number {
                value: 42.0,
                pos: SourcePos { row: 1, col: 1 }
            }))
        );
        assert_eq!(
            scanner.next(),
            Some(SR::Ok(Token::Number {
                value: 3.15,
                pos: SourcePos { row: 1, col: 4 }
            }))
        );
        assert_eq!(
            scanner.next(),
            Some(SR::Ok(Token::Number {
                value: 3.0,
                pos: SourcePos { row: 1, col: 9 }
            }))
        );
        assert_eq!(
            scanner.next(),
            Some(SR::Ok(Token::Number {
                value: 0.5,
                pos: SourcePos { row: 1, col: 13 }
            }))
        );
        assert_eq!(
            scanner.next(),
            Some(SR::Ok(Token::Number {
                value: 0.0,
                pos: SourcePos { row: 1, col: 17 }
            }))
        );
    }

    #[test]
    fn scan_identifiers() {
        let mut empty_scanner = Scanner {
            source: AsciiSource::build("").unwrap(),
        };
        assert_eq!(empty_scanner.next(), None);

        let mut strings = Scanner {
            source: AsciiSource::build("5 \"This is a string\" AvarIable anoter_var556").unwrap(),
        };
        assert!(strings.next().unwrap().is_ok());
        assert!(strings.next().unwrap().is_ok());

        assert_eq!(
            strings.next(),
            Some(SR::Ok(Token::Identifier {
                ident: "AvarIable".to_owned(),
                pos: SourcePos { row: 1, col: 22 }
            }))
        );
        assert_eq!(
            strings.next(),
            Some(SR::Ok(Token::Identifier {
                ident: "anoter_var556".to_owned(),
                pos: SourcePos { row: 1, col: 32 }
            }))
        );
    }

    #[test]
    fn scan_string() {
        let mut empty_scanner = Scanner {
            source: AsciiSource::build("").unwrap(),
        };
        assert_eq!(empty_scanner.next(), None);

        let mut strings = Scanner {
            source: AsciiSource::build("5 \"This is a string\"").unwrap(),
        };
        assert!(strings.next().unwrap().is_ok());

        assert_eq!(
            strings.next(),
            Some(SR::Ok(Token::String {
                string: "This is a string".to_owned(),
                pos: SourcePos { row: 1, col: 3 }
            }))
        );
    }

    #[test]
    fn scan_comments() {
        let mut empty_scanner = Scanner {
            source: AsciiSource::build("").unwrap(),
        };
        assert_eq!(empty_scanner.next(), None);

        let mut comments = Scanner {
            source: AsciiSource::build("// 42  \n   ==// != variable >=\n \r\t//\n\nvar").unwrap(),
        };

        assert_eq!(
            comments.next(),
            Some(SR::Ok(Token::EqualEqual(SourcePos { row: 2, col: 4 })))
        );

        assert_eq!(
            comments.next(),
            Some(SR::Ok(Token::Var(SourcePos { row: 5, col: 1 })))
        );
    }

    #[test]
    fn scan_symbols() {
        let mut empty_scanner = Scanner {
            source: AsciiSource::build("").unwrap(),
        };
        assert_eq!(empty_scanner.next(), None);

        let mut double_ops = Scanner {
            source: AsciiSource::build(" ! != = == < <= >= > / + - * ; , . () {{  }").unwrap(),
        };

        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Bang(SourcePos { row: 1, col: 2 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::BangEqual(SourcePos { row: 1, col: 4 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Equal(SourcePos { row: 1, col: 7 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::EqualEqual(SourcePos { row: 1, col: 9 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Less(SourcePos { row: 1, col: 12 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::LessEqual(SourcePos { row: 1, col: 14 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::GreaterEqual(SourcePos { row: 1, col: 17 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Greater(SourcePos { row: 1, col: 20 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Slash(SourcePos { row: 1, col: 22 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Plus(SourcePos { row: 1, col: 24 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Minus(SourcePos { row: 1, col: 26 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Star(SourcePos { row: 1, col: 28 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Semicolon(SourcePos { row: 1, col: 30 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Comma(SourcePos { row: 1, col: 32 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::Dot(SourcePos { row: 1, col: 34 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::LeftParen(SourcePos { row: 1, col: 36 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::RightParen(SourcePos { row: 1, col: 37 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::LeftBrace(SourcePos { row: 1, col: 39 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::LeftBrace(SourcePos { row: 1, col: 40 })))
        );
        assert_eq!(
            double_ops.next(),
            Some(SR::Ok(Token::RightBrace(SourcePos { row: 1, col: 43 })))
        );
        assert_eq!(double_ops.next(), None);
    }
}

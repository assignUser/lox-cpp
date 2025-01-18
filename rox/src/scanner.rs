use std::fmt::Display;
type ScannerResult = Result<Token, ScannerError>;

pub fn scan(source: &str) -> Result<ScannerResults, ScannerError> {
    let scanner = Scanner::build(source)?;
    Ok(scanner.collect::<ScannerResults>())
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
        ($input:expr, $EnumType:ty, $( $Variant:ident ),+ $(,)?) => {{
            let s = $input;
            let mut found = None;
            $(
                if *s == *format!("{}", <$EnumType>::$Variant).split(' ').nth(1).unwrap_or(" ")
{
                    found = Some(<$EnumType>::$Variant);
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

        token_from_str!(&op, Token, BangEqual, EqualEqual, LessEqual, GreaterEqual)
            .map(|t| {
                self.source.shift(2);
                Ok(t)
            })
            .or_else(|| {
                op.truncate(1);
                token_from_str!(
                    op, Token, Minus, Plus, Semicolon, Slash, Star, Bang, Equal, Greater, Less,
                    LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot
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
            Token,
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
            While
        );

        match keyword {
            None => Some(Ok(Token::Identifier(identifier))),
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
                        self.source.current_pos(),
                    )));
                }
                Some(d) => {
                    string.push(d as char);
                    self.source.shift(1);
                }
            }
        }

        Some(Ok(Token::String(string)))
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
                .map(Token::Number),
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
                        self.source.current_pos(),
                    )));
                    // consume the character
                    self.source.shift(1);

                    res
                }
            })
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
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
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
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::And => write!(f, "AND and null"),
            Token::Class => write!(f, "CLASS class null"),
            Token::Else => write!(f, "ELSE else null"),
            Token::False => write!(f, "FALSE false null"),
            Token::Fun => write!(f, "FUN fun null"),
            Token::For => write!(f, "FOR for null"),
            Token::If => write!(f, "IF if null"),
            Token::Nil => write!(f, "NIL nil null"),
            Token::Or => write!(f, "OR or null"),
            Token::Print => write!(f, "PRINT print null"),
            Token::Return => write!(f, "RETURN return null"),
            Token::Super => write!(f, "SUPER super null"),
            Token::This => write!(f, "THIS this null"),
            Token::True => write!(f, "TRUE true null"),
            Token::Var => write!(f, "VAR var null"),
            Token::While => write!(f, "WHILE while null"),
            Token::Identifier(s) => write!(f, "IDENTIFIER {s} null"),
            Token::String(s) => write!(f, "STRING \"{s}\" {s}"),
            Token::Number(n) => {
                // This is to match jlox's expectation
                if n.fract() == 0.0 {
                    write!(f, "NUMBER {} {}.0", n, *n as i64)
                } else {
                    write!(f, "NUMBER {n} {n}")
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

        assert_eq!(scanner.next(), Some(SR::Ok(Token::Number(42.0))));
        assert_eq!(scanner.next(), Some(SR::Ok(Token::Number(3.15))));
        assert_eq!(scanner.next(), Some(SR::Ok(Token::Number(3.0))));
        assert_eq!(scanner.next(), Some(SR::Ok(Token::Number(0.5))));
        assert_eq!(scanner.next(), Some(SR::Ok(Token::Number(0.0))));
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
            Some(SR::Ok(Token::Identifier("AvarIable".to_owned())))
        );
        assert_eq!(
            strings.next(),
            Some(SR::Ok(Token::Identifier("anoter_var556".to_owned())))
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
            Some(SR::Ok(Token::String("This is a string".to_owned())))
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
        assert_eq!(comments.next(), Some(SR::Ok(Token::EqualEqual)));
        assert_eq!(comments.next(), Some(SR::Ok(Token::Var)));
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

        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Bang)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::BangEqual)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Equal)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::EqualEqual)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Less)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::LessEqual)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::GreaterEqual)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Greater)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Slash)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Plus)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Minus)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Star)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Semicolon)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Comma)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::Dot)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::LeftParen)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::RightParen)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::LeftBrace)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::LeftBrace)));
        assert_eq!(double_ops.next(), Some(SR::Ok(Token::RightBrace)));
        assert_eq!(double_ops.next(), None);
    }
}

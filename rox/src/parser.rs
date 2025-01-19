#![allow(dead_code, unused_imports)]

use core::slice::Iter;
use std::iter::Enumerate;

use crate::scanner::{self, SourcePos, Token};
use crate::LoxError;

struct Identifier {
    name: String,
    pos: SourcePos,
}

enum Declaration {
    Class(Class),
    Function(Function),
    Var(Var),
    Statement(Statement),
}

enum Statement {
    Block,
    Expression(Expression),
    For,
    If,
    Print,
    Return,
    While,
}

struct Class {
    name: Identifier,
    parent: Option<Identifier>,
    body: Vec<Function>,
}

struct Function {
    name: Identifier,
    parameters: Option<Vec<Identifier>>,
    body: Vec<Statement>,
}

struct Var {
    name: Identifier,
    expr: Expression,
}

enum Expression {
    Assign {
        name: String,
        value: Value,
    },
    Binary {
        lhs: Box<Expression>,
        operator: BinaryOperator,
        rhs: Box<Expression>,
    },
    Call {
        callee: Identifier,
        arguments: Option<Vec<Identifier>>,
        pos: SourcePos,
    },
    Get {
        name: Token,
        object: Box<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Set {
        value: Box<Expression>,
        name: Token,
        object: Box<Expression>,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    This {
        keyword: Token,
    },
    Unary {
        operator: UnaryOperator,
        rhs: Box<Expression>,
    },
    Variable {
        name: Token,
    },
    Literal(Value),
}

enum Value {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

enum UnaryOperator {
    And(Token),
    Bang(Token),
    Or(Token),
}

enum BinaryOperator {
    BangEqual(Token),
    Equal(Token),
    EqualEqual(Token),
    Greater(Token),
    GreaterEqual(Token),
    Less(Token),
    LessEqual(Token),
    Minus(Token),
    Plus(Token),
    Slash(Token),
    Star(Token),
}

enum ParserError {
    Error,
    UnexpectedEof,
    UnexepctedToken { token: Token, message: String },
}

struct Parser<'a> {
    tokens: Vec<scanner::Token>,
    iter: Enumerate<Iter<'a, scanner::Token>>,
}

impl Parser<'_> {
    // pub fn new(tokens: Vec<scanner::Token>) -> Parser {
    //     Parser {
    //         curr: 0,
    //         len: tokens.len(),
    //         tokens,
    //     }
    // }

    pub fn parse(tokens: Vec<scanner::Token>) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];

        Ok(statements)
    }

    // declaration    → classDecl
    //                | funDecl
    //                | varDecl
    //                | statement ;
    //
    // classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
    //                  "{" function* "}" ;
    // funDecl        → "fun" function ;
    // varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn declaration(&mut self) -> Result<Statement, ParserError> {
        let (_, token) = self.iter.next().ok_or(ParserError::UnexpectedEof)?;
        match token {
            Token::Var(_) => self.variable(),
            Token::Class(_) => self.class(),
            Token::Fun(_) => self.function(),
            _ => self.statement(),
        }
    }
    fn class(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn function(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn variable(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn statement(&mut self) -> Result<Statement, ParserError> {
        let (_, token) = self.iter.next().ok_or(ParserError::UnexpectedEof)?;
        match token {
            _ => self.expr_stmt(),
        }
    }
    fn expr_stmt(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        match self.iter.next() {
            None => Err(ParserError::UnexpectedEof),
            Some((_, t)) => match t {
                Token::Semicolon(_) => Ok(expr),
                _ => Err(ParserError::UnexepctedToken {
                    token: t.clone(),
                    message: "Expect ';' after expression.".to_owned(),
                }),
            },
        }
    }
    fn for_stmt(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn if_stmt(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn print_stmt(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn return_stmt(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn while_stmt(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn block(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }

    // expression     → assignment ;
    //
    // assignment     → ( call "." )? IDENTIFIER "=" assignment
    //                | logic_or ;
    //
    // logic_or       → logic_and ( "or" logic_and )* ;
    // logic_and      → equality ( "and" equality )* ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    //
    // unary          → ( "!" | "-" ) unary | call ;
    // call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    // primary        → "true" | "false" | "nil" | "this"
    //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    //                | "super" "." IDENTIFIER ;
    fn expression(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn assignment(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn logic_or(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn logic_and(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn equality(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn comparison(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn term(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn factor(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn unary(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn call(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
    fn primary(&mut self) -> Result<Statement, ParserError> {
        Err(ParserError::Error)
    }
}

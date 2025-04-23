use core::slice::Iter;
use std::{fmt::Display, iter::Peekable};

use crate::scanner::{SourcePos, SourcePosition, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub pos: SourcePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Block(Block),
    Expression(Expression),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Print(Expression),
    Return {
        keyword: Token,
        value: Option<Expression>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    Class(Class),
    Function(Function),
    Var(Var),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<Statement>,
    pub key: SourcePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: Identifier,
    pub parent: Option<Identifier>,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Option<Vec<Identifier>>,
    pub body: Block,
}

impl SourcePosition for Function {
    fn get_pos(&self) -> SourcePos {
        self.name.pos.clone()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub name: Identifier,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Assign {
        name: Identifier,
        value: Box<Expression>,
        scope_depth: Option<usize>,
    },
    Binary {
        lhs: Box<Expression>,
        operator: Token,
        rhs: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Option<Vec<Expression>>,
        pos: SourcePos,
    },
    Get {
        name: Identifier,
        object: Box<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Set {
        value: Box<Expression>,
        name: Identifier,
        object: Box<Expression>,
    },
    Super {
        keyword: Token,
        method: Identifier,
    },
    This {
        keyword: Token,
    },
    Unary {
        operator: Token,
        rhs: Box<Expression>,
    },
    Variable {
        name: Identifier,
        scope_depth: Option<usize>,
    },
    Literal(Value),
}

#[derive(Clone, Debug)]
pub enum Value {
    Boolean { value: bool, pos: SourcePos },
    Nil(SourcePos),
    Number { value: f64, pos: SourcePos },
    String { value: String, pos: SourcePos },
}

impl Eq for Value {}

impl SourcePosition for Value {
    fn get_pos(&self) -> SourcePos {
        match self {
            Self::String { value: _, pos } => pos.clone(),
            Self::Boolean { value: _, pos } => pos.clone(),
            Self::Number { value: _, pos } => pos.clone(),
            Self::Nil(pos) => pos.clone(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Value::Boolean {
                    value: self_v,
                    pos: _,
                },
                Value::Boolean {
                    value: other_v,
                    pos: _,
                },
            ) => self_v == other_v,
            (
                Value::Number {
                    value: self_v,
                    pos: _,
                },
                Value::Number {
                    value: other_v,
                    pos: _,
                },
            ) => self_v == other_v,
            (
                Value::String {
                    value: self_v,
                    pos: _,
                },
                Value::String {
                    value: other_v,
                    pos: _,
                },
            ) => self_v == other_v,
            (Value::Nil(_), Value::Nil(_)) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean { value, .. } => write!(f, "{value}"),
            Self::String { value, .. } => write!(f, "{value}"),
            Self::Nil(_) => write!(f, "nil"),
            Self::Number { value, .. } => write!(f, "{value}"),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Error,
    UnexpectedEof,
    UnexpectedToken { token: Token, message: String },
    Syntax { token: Token, message: String },
    UnexpectedStatement { stmt: Statement, message: String },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "Error"),
            Self::UnexpectedEof => write!(f, "Unexpected end of file."),
            Self::UnexpectedToken { token, message } => write!(
                f,
                "[line {0}] Error at '{token}': {message}",
                token.get_pos().row
            ),
            Self::UnexpectedStatement {
                stmt: _,
                message: _,
            } => write!(f, "Unexpected Statement."),
            Self::Syntax {
                token: _,
                message: _,
            } => write!(f, "Syntax error."),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    iter: Peekable<Iter<'a, Token>>,
}

macro_rules! binary_expression {
    ($self:ident, $next:ident, $token_pat:pat) => {{
        let mut lhs = $self.$next()?;

        while let $token_pat = $self.peek() {
            let op = $self.iter.next().expect("Token was peeked!");
            let rhs = $self.$next()?;

            lhs = match lhs {
                Statement::Expression(_) => Statement::Expression(Expression::Binary {
                    lhs: $self.box_expression(lhs)?,
                    operator: op.clone(),
                    rhs: $self.box_expression(rhs)?,
                }),
                _ => {
                    return Err(ParserError::Syntax {
                        token: op.clone(),
                        message: "Expected Expression".to_owned(),
                    })
                }
            }
        }

        Ok(lhs)
    }};
}

macro_rules! consume {
    (
        $self:ident,
        $variant:ident $pattern:tt,
        (),
        $message:expr
    ) => {{
        let token = $self.iter.next();
        match token {
            None => Err(ParserError::UnexpectedEof),
            #[allow(unused_variables)]
            Some(t @ Token::$variant $pattern) => {
                Ok(t)
            }
            Some(t) => Err(ParserError::UnexpectedToken {
                token: t.clone(),
                message: $message.to_string(),
            }),
        }
    }};

    (
        $self:ident,
        $variant:ident $pattern:tt,
        $return_expr:expr,
        $message:expr
    ) => {{
        let token = $self.iter.next();
        match token {
            None => Err(ParserError::UnexpectedEof),
            #[allow(unused_variables)]
            Some(t @ Token::$variant $pattern) => {
                // t is the entire matched token, and
                // any fields/patterns in $pattern are bound for $return_expr
                Ok($return_expr)
            }
            Some(t) => Err(ParserError::UnexpectedToken {
                token: t.clone(),
                message: $message.to_string(),
            }),
        }
    }};
}

#[macro_export]
macro_rules! take_variant {
    (
        $stmt:expr,
        // The variant name within `Statement`
        $Variant:ident
    ) => {{
        match $stmt {
            Statement::$Variant(val) => Ok(val),
            stmt => Err(ParserError::UnexpectedStatement {
                stmt,
                message: format!("Expect {}.", stringify!($Variant)),
            }),
        }
    }};
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> &Token {
        self.iter.peek().unwrap_or(&&Token::Eof)
    }

    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            iter: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];
        while *self.peek() != Token::Eof {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    /// Discard all Tokens until a known good starting point is reached
    fn synchronize(&mut self) {
        while let Some(token) = self.iter.next() {
            if let Token::Semicolon(_) = token {
                return;
            }

            match self.peek() {
                Token::Class(_)
                | Token::Fun(_)
                | Token::Var(_)
                | Token::For(_)
                | Token::If(_)
                | Token::While(_)
                | Token::Print(_)
                | Token::Return(_) => return,
                Token::Eof => return,
                _ => (),
            }
        }
    }

    fn identifier_from_token(&mut self, error_message: &str) -> Result<Identifier, ParserError> {
        let name = consume!(self, Identifier { ident, pos }, (ident, pos), error_message)?;
        Ok(Identifier {
            name: name.0.clone(),
            pos: name.1.clone(),
        })
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
    //
    // statement      → exprStmt
    //                | forStmt
    //                | ifStmt
    //                | printStmt
    //                | returnStmt
    //                | whileStmt
    //                | block ;
    //
    // exprStmt       → expression ";" ;
    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                            expression? ";"
    //                            expression? ")" statement ;
    // ifStmt         → "if" "(" expression ")" statement
    //                  ( "else" statement )? ;
    // printStmt      → "print" expression ";" ;
    // returnStmt     → "return" expression? ";" ;
    // whileStmt      → "while" "(" expression ")" statement ;
    // block          → "{" declaration* "}" ;
    fn declaration(&mut self) -> Result<Statement, ParserError> {
        let decl = match self.peek() {
            Token::Var(_) => {
                self.iter.next();
                self.variable()
            }
            Token::Class(_) => {
                self.iter.next();
                self.class()
            }
            Token::Fun(_) => {
                self.iter.next();
                self.function("function")
            }
            _ => self.statement(),
        };

        if decl.is_err() {
            self.synchronize();
        }
        decl
    }

    fn class(&mut self) -> Result<Statement, ParserError> {
        let name = self.identifier_from_token("Expect class name.")?;

        // Optional super class
        let mut parent = None;
        if let Token::Less(_) = self.peek() {
            // consume <
            self.iter.next();

            parent = Some(self.identifier_from_token("Expect super class name.")?);
        }

        consume!(self, LeftBrace(_), (), "Expect '{{' before class body.")?;
        let mut methods: Vec<Function> = vec![];

        loop {
            match self.peek() {
                Token::RightBrace(_) => break,
                _ => methods.push(take_variant!(self.function("method")?, Function)?),
            }
        }

        consume!(self, RightBrace(_), (), "Expect '}}' after class body.")?;

        Ok(Statement::Class(Class {
            name,
            parent,
            methods,
        }))
    }

    fn function(&mut self, kind: &str) -> Result<Statement, ParserError> {
        let name = self.identifier_from_token(&format!("Expect {kind} name."))?;

        consume!(
            self,
            LeftParen(_),
            (),
            format!("Expect '(' after {kind} name.")
        )?;

        let mut params: Vec<Identifier> = vec![];
        macro_rules! push_param {
            () => {{
                let name_token = consume!(
                    self,
                    Identifier { ident, pos },
                    (ident.clone(), pos.clone()),
                    "Expect parameter name."
                )?;

                params.push(Identifier {
                    name: name_token.0,
                    pos: name_token.1,
                });
                Ok(())
            }};
        }

        if let Token::RightParen(_) = self.peek() {
            // no params
        } else {
            push_param!()?;

            while let Token::Comma(_) = self.peek() {
                // Consume ,
                self.iter.next();
                push_param!()?;
            }
        }

        consume!(
            self,
            RightParen(pos),
            pos.clone(),
            "Expect ')' after arguments."
        )?;

        let key_pos = consume!(
            self,
            LeftBrace(pos),
            pos.clone(),
            format!("Expect '{{' before {kind} body.")
        )?;

        let body = self.block_stmt(key_pos)?;

        Ok(Statement::Function(Function {
            name,
            parameters: Some(params),
            body: if let Statement::Block(block) = body {
                block
            } else {
                return Err(ParserError::UnexpectedStatement {
                    stmt: body,
                    message: format!("Expect '{{' before {kind} body."),
                });
            },
        }))
    }

    fn variable(&mut self) -> Result<Statement, ParserError> {
        let name = self.identifier_from_token("Expect variable name.")?;

        let mut initializer: Option<Expression> = None;

        if let Token::Equal(_) = self.peek() {
            self.iter.next();
            initializer = match self.expression()? {
                Statement::Expression(expr) => Some(expr),
                stmt => {
                    return Err(ParserError::UnexpectedStatement {
                        stmt,
                        message: "Expect expression as initializer.".to_string(),
                    })
                }
            };
        }

        consume!(
            self,
            Semicolon(pos),
            (),
            "Expect ';' after variable declaration."
        )?;

        Ok(Statement::Var(Var { name, initializer }))
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek() {
            Token::For(pos) => {
                let sp = pos.clone();
                self.iter.next();
                self.for_stmt(sp)
            }
            Token::If(_) => {
                self.iter.next();
                self.if_stmt()
            }
            Token::Print(_) => {
                self.iter.next();
                self.print_stmt()
            }
            Token::Return(_) => self.return_stmt(),
            Token::While(_) => {
                self.iter.next();
                self.while_stmt()
            }
            Token::LeftBrace(pos) => {
                let key = pos.clone();
                self.iter.next();
                self.block_stmt(key)
            }
            _ => self.expr_stmt(),
        }
    }

    fn expr_stmt(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        match self.iter.next().unwrap_or(&Token::Eof) {
            Token::Semicolon(_) => Ok(expr),
            t => {
                let token = if let Token::Eof = t {
                    self.tokens.last().expect("Already in expression!").clone()
                } else {
                    t.clone()
                };
                Err(ParserError::UnexpectedToken {
                    token,
                    message: "Expect ';' after expression.".to_owned(),
                })
            }
        }
    }

    fn for_stmt(&mut self, pos: SourcePos) -> Result<Statement, ParserError> {
        consume!(self, LeftParen(_pos), _pos, "Expect '(' after 'for'.")?;

        let mut for_body = vec![];

        let initializer = match self.peek() {
            Token::Semicolon(_) => None,
            Token::Var(_) => {
                self.iter.next();
                Some(Statement::Var(take_variant!(self.variable()?, Var)?))
            }
            _ => Some(Statement::Expression(take_variant!(
                self.expr_stmt()?,
                Expression
            )?)),
        };

        if let Some(init) = initializer {
            for_body.push(init);
        }

        let condition;
        if let Token::Semicolon(pos) = self.peek() {
            condition = Expression::Literal(Value::Boolean {
                value: true,
                pos: pos.clone(),
            });
        } else {
            condition = take_variant!(self.expression()?, Expression)?;
        }

        consume!(
            self,
            Semicolon(_pos),
            _pos,
            "Expect ';' after loop condition."
        )?;

        let mut while_body = vec![];
        let while_pos;

        if let Token::RightParen(ppos) = self.peek() {
            while_pos = ppos.clone();
            // no increment
        } else {
            let increment = take_variant!(self.expression()?, Expression)?;
            while_body.push(Statement::Expression(increment));
            while_pos = self.peek().get_pos();
        }
        consume!(self, RightParen(_pos), (), "Expect ')' after for clauses.")?;

        while_body.insert(0, self.statement()?);

        for_body.push(Statement::While {
            condition,
            body: Box::new(Statement::Block(Block {
                key: while_pos,
                body: while_body,
            })),
        });

        Ok(Statement::Block(Block {
            key: pos,
            body: for_body,
        }))
    }

    fn if_stmt(&mut self) -> Result<Statement, ParserError> {
        consume!(self, LeftParen(_pos), _pos, "Expect '(' after 'if'.")?;

        let condition = take_variant!(self.expression()?, Expression)?;

        consume!(self, RightParen(_pos), _pos, "Expect ')' after 'if'.")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;

        if let Token::Else(_) = self.peek() {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_stmt(&mut self) -> Result<Statement, ParserError> {
        let value = take_variant!(self.expression()?, Expression)?;

        consume!(self, Semicolon(_pos), _pos, "Expect ';' after value.")?;

        Ok(Statement::Print(value))
    }

    fn return_stmt(&mut self) -> Result<Statement, ParserError> {
        let keyword = self.iter.next().expect("Token peeked!").clone();

        let mut value: Option<Expression> = None;

        if let Token::Semicolon(_) = self.peek() {
            // no return value
        } else {
            value = Some(take_variant!(self.expression()?, Expression)?);
            consume!(
                self,
                Semicolon(_pos),
                _pos,
                "Expect ';' after return value."
            )?;
        }

        Ok(Statement::Return { keyword, value })
    }

    fn while_stmt(&mut self) -> Result<Statement, ParserError> {
        consume!(self, LeftParen(_), (), "Expect '(' after 'while'.")?;

        let condition = take_variant!(self.expression()?, Expression)?;

        consume!(self, RightParen(_), (), "Expect ')' after condition.")?;

        let body = Box::new(self.statement()?);

        Ok(Statement::While { condition, body })
    }

    fn block_stmt(&mut self, key: SourcePos) -> Result<Statement, ParserError> {
        let mut stmts = vec![];

        loop {
            match self.peek() {
                Token::Eof | Token::RightBrace(_) => break,
                _ => stmts.push(self.declaration()?),
            }
        }

        consume!(self, RightBrace(_pos), _pos, "Expect '}' after block.")?;

        Ok(Statement::Block(Block { body: stmts, key }))
    }

    fn box_expression(&self, stmt: Statement) -> Result<Box<Expression>, ParserError> {
        match stmt {
            Statement::Expression(e) => Ok(Box::new(e)),
            _ => Err(ParserError::Error),
        }
    }

    // expression     → assignment ;
    fn expression(&mut self) -> Result<Statement, ParserError> {
        self.assignment()
    }

    // assignment     → ( call "." )? IDENTIFIER "=" assignment
    //                | logic_or ;
    fn assignment(&mut self) -> Result<Statement, ParserError> {
        let var = self.logic_or()?;

        if let Token::Equal(_) = self.peek() {
            let equals = self.iter.next().expect("Token was peeked!");
            let value = self.assignment()?;

            match var {
                Statement::Expression(Expression::Variable { name: n, .. }) => {
                    return Ok(Statement::Expression(Expression::Assign {
                        name: n,
                        value: self.box_expression(value)?,
                        scope_depth: None,
                    }));
                }
                Statement::Expression(Expression::Get { name, object }) => {
                    return Ok(Statement::Expression(Expression::Set {
                        value: self.box_expression(value)?,
                        name,
                        object,
                    }));
                }
                _ => {
                    return Err(ParserError::Syntax {
                        token: equals.clone(),
                        message: "Invalid assignment target.".to_owned(),
                    })
                }
            }
        }

        Ok(var)
    }

    // logic_or       → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(self, logic_and, Token::Or(_))
    }

    // logic_and      → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(self, equality, Token::And(_))
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(self, comparison, Token::BangEqual(_) | Token::EqualEqual(_))
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(
            self,
            term,
            Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_)
        )
    }

    // term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(self, factor, Token::Minus(_) | Token::Plus(_))
    }

    // factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Statement, ParserError> {
        binary_expression!(self, unary, Token::Star(_) | Token::Slash(_))
    }

    // unary          → ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> Result<Statement, ParserError> {
        if let Token::Bang(_) | Token::Minus(_) = self.peek() {
            let op = self.iter.next().expect("Token peeked!");
            let rhs = self.unary()?;

            return Ok(Statement::Expression(Expression::Unary {
                operator: op.clone(),
                rhs: self.box_expression(rhs)?,
            }));
        }

        self.call()
    }

    // call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

    fn call(&mut self) -> Result<Statement, ParserError> {
        let mut expr = self.primary()?;

        loop {
            expr = match self.peek() {
                Token::LeftParen(_) => self.finish_call(expr)?,
                // chained calls/property access class.field.nested
                Token::Dot(_) => {
                    // Consume Dot
                    self.iter.next();
                    let token = self.iter.next().ok_or(ParserError::UnexpectedEof)?;

                    match token {
                        Token::Identifier { ident, pos } => {
                            Statement::Expression(Expression::Get {
                                name: Identifier {
                                    name: ident.to_string(),
                                    pos: pos.clone(),
                                },
                                object: self.box_expression(expr)?,
                            })
                        }
                        _ => {
                            return Err(ParserError::UnexpectedToken {
                                token: token.clone(),
                                message: "Expect property name after '.'.".to_owned(),
                            })
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Statement) -> Result<Statement, ParserError> {
        // Consume (
        self.iter.next();

        let mut args: Vec<Expression> = vec![];

        if let Token::RightParen(_) = self.peek() {
            // empty call ()
        } else {
            args.push(take_variant!(self.expression()?, Expression)?);
            while let Token::Comma(_) = self.peek() {
                // Consume ,
                self.iter.next();
                args.push(take_variant!(self.expression()?, Expression)?);
            }
        }
        let pos = consume!(self, RightParen(pos), pos, "Expect ')' after arguments.")?;

        Ok(Statement::Expression(Expression::Call {
            callee: self.box_expression(callee)?,
            arguments: Some(args),
            pos: pos.clone(),
        }))
    }

    // primary        → "true" | "false" | "nil" | "this"
    //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    //                | "super" "." IDENTIFIER ;
    fn primary(&mut self) -> Result<Statement, ParserError> {
        let token = self.iter.next().ok_or(ParserError::UnexpectedEof)?;

        let stmt = match token {
            Token::True(p) => Statement::Expression(Expression::Literal(Value::Boolean {
                value: true,
                pos: p.clone(),
            })),
            Token::False(p) => Statement::Expression(Expression::Literal(Value::Boolean {
                value: false,
                pos: p.clone(),
            })),
            Token::Nil(p) => Statement::Expression(Expression::Literal(Value::Nil(p.clone()))),
            Token::This(_) => Statement::Expression(Expression::This {
                keyword: token.clone(),
            }),
            Token::Number { value, pos } => {
                Statement::Expression(Expression::Literal(Value::Number {
                    value: *value,
                    pos: pos.clone(),
                }))
            }
            Token::String { string, pos } => {
                Statement::Expression(Expression::Literal(Value::String {
                    value: string.clone(),
                    pos: pos.clone(),
                }))
            }
            Token::Identifier { ident, pos } => Statement::Expression(Expression::Variable {
                name: Identifier {
                    name: ident.clone(),
                    pos: pos.clone(),
                },
                scope_depth: None,
            }),
            Token::Super(_) => {
                consume!(self, Dot(_pos), _pos, "Expect '.' after 'super'.")?;
                let method = consume!(
                    self,
                    Identifier { ident, pos },
                    (ident, pos),
                    "Expect superclass method name."
                )?;
                Statement::Expression(Expression::Super {
                    keyword: token.clone(),
                    method: Identifier {
                        name: method.0.to_owned(),
                        pos: method.1.clone(),
                    },
                })
            }
            Token::LeftParen(_) => {
                let expr = self.expression()?;
                consume!(self, RightParen(_pos), _pos, "Expect ')' after expression.")?;
                Statement::Expression(Expression::Grouping {
                    expr: self.box_expression(expr)?,
                })
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: token.clone(),
                    message: "Expect expression.".to_owned(),
                })
            }
        };
        Ok(stmt)
    }
}

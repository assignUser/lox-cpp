use crate::parser::{Expression, Function, Statement, Value};
use crate::scanner::{SourcePos, Token};
use std::fmt::Display;
use std::io::Read;

use crate::LoxError;

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnValue {
    Value(Value),
    Function(Function),
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => value.fmt(f),
            Self::Function(function) => function.fmt(f),
        }
    }
}

pub enum InterpreterError {
    Error,
}

pub trait Interpretable {
    fn eval(&self) -> Result<ReturnValue, InterpreterError>;
}

// Block,
// Expression(Expression),
// For,
// If,
// Print(Expression),
// Return,
// While,
pub fn interpret(stmt: Statement) -> Result<ReturnValue, InterpreterError> {
    match stmt {
        Statement::Print(expr) => {
            println!("{}", expr.eval()?);
            Ok(ReturnValue::Value(Value::Nil(SourcePos { row: 0, col: 0 })))
        }
        Statement::Expression(expr) => Ok(expr.eval()?),
        _ => todo!(),
    }
}

// Assign {
//     name: Identifier,
//     value: Box<Expression>,
// },
// Binary {
//     lhs: Box<Expression>,
//     operator: Token,
//     rhs: Box<Expression>,
// },
// Call {
//     callee: Identifier,
//     arguments: Option<Vec<Expression>>,
//     pos: SourcePos,
// },
// Get {
//     name: Identifier,
//     object: Box<Expression>,
// },

// Set {
//     value: Box<Expression>,
//     name: Identifier,
//     object: Box<Expression>,
// },
// Super {
//     keyword: Token,
//     method: Identifier,
// },
// This {
//     keyword: Token,
// },
// Variable {
//     name: Identifier,
// },
// Literal(Value),
impl Interpretable for Expression {
    fn eval(&self) -> Result<ReturnValue, InterpreterError> {
        match self {
            Expression::Literal(value) => Ok(ReturnValue::Value(value.clone())),
            Expression::Unary { operator, rhs } => eval_unary(operator, rhs),
            Expression::Grouping { expr } => expr.eval(),
            Expression::Binary { lhs, operator, rhs } => eval_binary(lhs, operator, rhs),
            _ => todo!(),
        }
    }
}

fn eval_unary(operator: &Token, rhs: &Expression) -> Result<ReturnValue, InterpreterError> {
    let value = rhs.eval()?;

    match operator {
        Token::Minus(_) => {
            if let ReturnValue::Value(Value::Number { value, pos }) = value {
                Ok(ReturnValue::Value(Value::Number {
                    value: -value,
                    pos: pos.clone(),
                }))
            } else {
                Err(InterpreterError::Error) //TODO error msg
            }
        }

        Token::Bang(pos) => Ok(ReturnValue::Value(Value::Boolean {
            value: !is_truthy(&value),
            pos: pos.clone(),
        })),
        _ => Err(InterpreterError::Error), //TODO error msg
    }
}

fn is_truthy(value: &ReturnValue) -> bool {
    match value {
        ReturnValue::Value(Value::Boolean { value, .. }) => *value,
        ReturnValue::Value(Value::Nil(_)) => false,
        _ => true,
    }
}

fn is_nil(value: &ReturnValue) -> bool {
    matches!(value, ReturnValue::Value(Value::Nil(_)))
}

fn are_equal(lhs: &ReturnValue, rhs: &ReturnValue) -> bool {
    if is_nil(lhs) && is_nil(rhs) {
        true
    } else if is_nil(lhs) {
        false
    } else {
        lhs == rhs
    }
}

fn eval_binary(
    lhs: &Expression,
    operator: &Token,
    rhs: &Expression,
) -> Result<ReturnValue, InterpreterError> {
    let lhs = lhs.eval()?;
    let rhs = rhs.eval()?;

    let get_float = |v: &ReturnValue| -> Result<f64, InterpreterError> {
        match v {
            ReturnValue::Value(Value::Number { value, pos }) => Ok(*value),
            _ => Err(InterpreterError::Error),
        }
    };

    match operator {
        Token::And(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: is_truthy(&lhs) && is_truthy(&rhs),
            pos: p.clone(),
        })),
        Token::Or(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: is_truthy(&lhs) || is_truthy(&rhs),
            pos: p.clone(),
        })),
        Token::EqualEqual(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: are_equal(&lhs, &rhs),
            pos: p.clone(),
        })),
        Token::BangEqual(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: !are_equal(&lhs, &rhs),
            pos: p.clone(),
        })),
        Token::Less(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: get_float(&lhs)? < get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::LessEqual(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: get_float(&lhs)? <= get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::Greater(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: get_float(&lhs)? > get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::GreaterEqual(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: get_float(&lhs)? >= get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::Minus(p) => Ok(ReturnValue::Value(Value::Number {
            value: get_float(&lhs)? - get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::Plus(p) => match lhs {
            ReturnValue::Value(Value::Number { value, ref pos }) => {
                if !matches!(&rhs, ReturnValue::Value(Value::Number { .. })) {
                    Err(InterpreterError::Error)
                } else {
                    Ok(ReturnValue::Value(Value::Number {
                        value: value + get_float(&rhs)?,
                        pos: pos.clone(),
                    }))
                }
            }
            ReturnValue::Value(Value::String { value, ref pos }) => {
                let rhs = if let ReturnValue::Value(Value::String { value, pos }) = rhs {
                    value.clone()
                } else {
                    return Err(InterpreterError::Error);
                };

                Ok(ReturnValue::Value(Value::String {
                    // is this clone needed?
                    value: value.clone() + &rhs,
                    pos: p.clone(),
                }))
            }
            _ => Err(InterpreterError::Error),
            

        },
        Token::Star(p) => Ok(ReturnValue::Value(Value::Number {
            value: get_float(&lhs)? * get_float(&rhs)?,
            pos: p.clone(),
        })),
        Token::Slash(p) => Ok(ReturnValue::Value(Value::Number {
            value: get_float(&lhs)? / get_float(&rhs)?,
            pos: p.clone(),
        })),
        _ => todo!(),
    }
}

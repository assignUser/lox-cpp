use crate::parser::{Class, Expression, Function, Statement, Value};
use crate::scanner::{SourcePos, SourcePosition, Token};
use std::collections::HashMap;
use std::fmt::Display;


#[derive(Debug, Clone)]
pub enum ReturnValue {
    Value(Value),
    Function(Function),
}

#[derive(Debug, Clone)]
pub enum Variable {
    Value(Value),
    Function(Function),
    Class(Class),
}

impl SourcePosition for ReturnValue {
    fn get_pos(&self) -> SourcePos {
        match self {
            Self::Value(v) => v.get_pos(),
            Self::Function(f) => f.get_pos(),
        }
    }
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => value.fmt(f),
            Self::Function(function) => function.fmt(f),
        }
    }
}

impl PartialEq for ReturnValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ReturnValue::Value(a), ReturnValue::Value(b)) => a == b,
            (ReturnValue::Function(a), ReturnValue::Function(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    Error,
    UnexpectedValue { msg: String, pos: SourcePos },
    InvalidOperand { msg: String, pos: SourcePos },
    InvalidOperator { msg: String, pos: SourcePos },
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "Error"),
            Self::UnexpectedValue { msg, pos } => write!(f, "{msg}\n[line {0}]", pos.row),
            Self::InvalidOperator { msg, pos } => write!(f, "{msg}\n[line {0}]", pos.row),
            Self::InvalidOperand { msg, pos } => write!(f, "{msg}\n[line {0}]", pos.row),
        }
    }
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
pub struct Interpreter {
    globals: HashMap<String, Variable>,
    scopes: HashMap<SourcePos, HashMap<String, Variable>>,
    stack: Vec<SourcePos>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            scopes: HashMap::new(),
            stack: vec![],
        }
    }

    pub fn interpret(&mut self, statements: &Vec<Statement>) -> Result<(), InterpreterError> {
        for stmt in statements {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<ReturnValue, InterpreterError> {
        match statement {
            Statement::Print(expr) => {
                println!("{}", expr.eval()?);
                Ok(ReturnValue::Value(Value::Nil(SourcePos { row: 0, col: 0 })))
            }
            Statement::Expression(expr) => Ok(expr.eval()?),
            _ => todo!(),
        }
    }
}

// Assign {
//     name: Identifier,
//     value: Box<Expression>,
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
        Token::Minus(p) => {
            if let ReturnValue::Value(Value::Number { value, pos }) = value {
                Ok(ReturnValue::Value(Value::Number {
                    value: -value,
                    pos: pos.clone(),
                }))
            } else {
                Err(InterpreterError::InvalidOperand {
                    msg: "Operand must be a number.".to_string(),
                    pos: p.clone(),
                })
            }
        }

        Token::Bang(pos) => Ok(ReturnValue::Value(Value::Boolean {
            value: !is_truthy(&value),
            pos: pos.clone(),
        })),
        _ => Err(InterpreterError::InvalidOperator {
            msg: format!("Invalid operator '{}' for unary expression.", operator),
            pos: operator.get_pos(),
        }),
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

fn eval_binary(
    lhs: &Expression,
    operator: &Token,
    rhs: &Expression,
) -> Result<ReturnValue, InterpreterError> {
    let lhs = lhs.eval()?;
    let rhs = rhs.eval()?;

    let get_float = |v: &ReturnValue| -> Result<f64, InterpreterError> {
        match v {
            ReturnValue::Value(Value::Number { value, pos: _ }) => Ok(*value),
            _ => Err(InterpreterError::UnexpectedValue {
                msg: format!("Expect 'Number', found {v}"),
                pos: v.get_pos(),
            }),
        }
    };

    let get_string = |v: &ReturnValue| -> Result<String, InterpreterError> {
        match v {
            ReturnValue::Value(Value::String { value, pos: _ }) => Ok(value.clone()),
            _ => Err(InterpreterError::UnexpectedValue {
                msg: format!("Expect 'String', found {v}"),
                pos: v.get_pos(),
            }),
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
            value: lhs == rhs,
            pos: p.clone(),
        })),
        Token::BangEqual(p) => Ok(ReturnValue::Value(Value::Boolean {
            value: lhs != rhs,
            pos: p.clone(),
        })),
        Token::Less(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Boolean {
                value: get_float(&lhs)? < get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::LessEqual(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Boolean {
                value: get_float(&lhs)? <= get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::Greater(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Boolean {
                value: get_float(&lhs)? > get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::GreaterEqual(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Boolean {
                value: get_float(&lhs)? >= get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::Minus(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Number {
                value: get_float(&lhs)? - get_float(&rhs)?,
                pos: p.clone(),
            }))
        }

        Token::Star(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Number {
                value: get_float(&lhs)? * get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::Slash(p)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Number {
                value: get_float(&lhs)? / get_float(&rhs)?,
                pos: p.clone(),
            }))
        }
        Token::Slash(_)
        | Token::Star(_)
        | Token::Greater(_)
        | Token::GreaterEqual(_)
        | Token::Less(_)
        | Token::LessEqual(_)
        | Token::Minus(_) => Err(InterpreterError::InvalidOperand {
            msg: "Operands must be numbers.".to_string(),
            pos: lhs.get_pos(),
        }),
        Token::Plus(_)
            if matches!(lhs, ReturnValue::Value(Value::Number { .. }))
                && matches!(rhs, ReturnValue::Value(Value::Number { .. })) =>
        {
            Ok(ReturnValue::Value(Value::Number {
                value: get_float(&lhs)? + get_float(&rhs)?,
                pos: lhs.get_pos(),
            }))
        }
        Token::Plus(_)
            if matches!(lhs, ReturnValue::Value(Value::String { .. }))
                && matches!(rhs, ReturnValue::Value(Value::String { .. })) =>
        {
            Ok(ReturnValue::Value(Value::String {
                value: get_string(&lhs)? + &get_string(&rhs)?,
                pos: lhs.get_pos(),
            }))
        }
        Token::Plus(_) => Err(InterpreterError::InvalidOperand {
            msg: "Operands must be two numbers or two strings.".to_string(),
            pos: lhs.get_pos(),
        }),
        _ => unreachable!("Binary expression only constructed with specific tokens."),
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn binary_test() {
//         // let expr = Expression::Binary {
//         //     lhs: Box::new(Expression::Literal(Value::Number {
//         //         value: 5.0,
//         //         pos: SourcePos { row: 1, col: 1 },
//         //     })),
//         //     operator: Token::Plus(SourcePos { row: 1, col: 3 }),
//         //     rhs: Box::new(Expression::Literal(Value::Number {
//         //         value: 7.0,
//         //         pos: SourcePos { row: 1, col: 5 },
//         //     })),
//         // };
//         // dbg!(expr.eval().unwrap());
//         let expr = Statement::Print(Expression::Literal(Value::String {
//             value: "Hello, world!".to_string(),
//             pos: SourcePos { row: 0, col: 0 },
//         }));
//         interpret(expr);
//         assert!(false);
//     }
// }

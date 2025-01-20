use crate::parser::Expression;
use crate::parser::Statement;
use crate::parser::Value;
use crate::LoxError;

pub trait Interpretable {
    fn eval(&self) -> Value;
}

pub fn interpret(stmt: Statement) -> Option<Value> {
    match stmt {
        Statement::Print(expr) => {
            println!("{}", expr.eval());
            None
        }
        Statement::Expression(expr) => Some(expr.eval()),
        _ => todo!(),
    }
}

impl Interpretable for Expression {
    fn eval(&self) -> Value {
        match self {
            Expression::Literal(value) => value.clone(),
            _ => todo!(),
        }
    }
}

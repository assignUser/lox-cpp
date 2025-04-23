use crate::parser::{Expression, Function, Identifier, Statement, Value};
use crate::scanner::{SourcePos, SourcePosition, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;
use std::vec;

trait Callable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<ReturnValue>,
        pos: &SourcePos,
    ) -> Result<ReturnValue, InterpreterError>;
    fn arity(&self) -> Result<usize, InterpreterError>;
}

#[derive(Debug, Clone, Eq)]
pub enum ReturnValue {
    Return(Box<ReturnValue>),
    Value(Value),
    Function(Function),
    // Class(Class),
}

impl Callable for ReturnValue {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<ReturnValue>,
        pos: &SourcePos,
    ) -> Result<ReturnValue, InterpreterError> {
        match self {
            Self::Function(function) => {
                // ExprPtr return_value = interpreter.executeBlock(decl.body, env);
                // if (not m_isInitializer) {
                //   return std::move(return_value);
                interpreter.add_scope();

                for (param, value) in function
                    .parameters
                    .iter()
                    .flatten()
                    .zip(arguments.into_iter())
                {
                    interpreter.assign_var(param, value);
                }

                let body = Statement::Block(function.body.clone());
                let value = interpreter.execute(&body)?;

                interpreter.pop_frame();

                if let Some(value) = value {
                    Ok(value)
                } else {
                    Ok(ReturnValue::Value(Value::Nil(pos.clone())))
                }
            }
            _ => {
                dbg!("not callable");
                Err(InterpreterError::Error)
            }
        }
    }

    fn arity(&self) -> Result<usize, InterpreterError> {
        match self {
            Self::Function(function) => {
                let arity = function.parameters.as_ref().map_or(3, |v| v.len());
                Ok(arity)
            }
            _ => {
                dbg!("not callable");
                Err(InterpreterError::Error)
            }
        }
    }
}

impl SourcePosition for ReturnValue {
    fn get_pos(&self) -> SourcePos {
        match self {
            Self::Value(v) => v.get_pos(),
            Self::Function(f) => f.get_pos(),
            Self::Return(r) => r.get_pos(),
        }
    }
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => value.fmt(f),
            Self::Function(function) => function.fmt(f),
            Self::Return(r) => r.fmt(f),
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

type Scope = HashMap<String, ReturnValue>;

pub struct Interpreter {
    globals: Scope,
    locals: Vec<Rc<RefCell<Scope>>>,
    stack: Vec<usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            locals: vec![],
            stack: vec![],
        }
    }

    fn add_scope(&mut self) {
        let new_scope = Rc::new(RefCell::new(Scope::new()));
        let i = self.locals.len();
        self.locals.push(new_scope);
        self.stack.push(i);
    }

    pub fn interpret(&mut self, statements: &Vec<Statement>) -> Result<(), InterpreterError> {
        for stmt in statements {
            self.execute(stmt)?;
        }

        Ok(())
    }

    // For,
    // Return,
    // execute a statement
    fn execute(&mut self, statement: &Statement) -> Result<Option<ReturnValue>, InterpreterError> {
        match statement {
            Statement::Block(block) => {
                self.add_scope();

                for stmt in block.body.iter() {
                    let result = self.execute(stmt)?;
                    if let Some(ReturnValue::Return(value)) = result {
                        self.pop_frame();
                        return Ok(Some(*value));
                    }
                }
                self.pop_frame();
            }
            Statement::Expression(expr) => {
                return Ok(Some(self.evaluate(expr)?));
            }
            Statement::Print(expr) => {
                println!("{}", self.evaluate(expr)?);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&self.evaluate(condition)?) {
                    let result = self.execute(then_branch)?;
                    if result.is_some() {
                        return Ok(result);
                    }
                } else if let Some(else_branch) = else_branch {
                    let result = self.execute(else_branch)?;
                    if result.is_some() {
                        return Ok(result);
                    }
                }
            }
            Statement::Return { keyword, value } => {
                if let Some(value) = value {
                    return Ok(Some(ReturnValue::Return(Box::new(self.evaluate(value)?))));
                }
            }
            Statement::Var(var) => {
                if let Some(init) = &var.initializer {
                    let init = self.evaluate(init)?;

                    self.assign_var(&var.name, init)?;
                } else {
                    self.assign_var(
                        &var.name,
                        ReturnValue::Value(Value::Nil(SourcePos { row: 0, col: 0 })),
                    )?;
                }
            }
            Statement::Function(function) => {
                self.assign_var(&function.name, ReturnValue::Function(function.clone()))?;
            }
            _ => {
                dbg!(&statement);
                todo!()
            }
        };

        Ok(None)
    }

    fn assign_var(
        &mut self,
        name: &Identifier,
        value: ReturnValue,
    ) -> Result<(), InterpreterError> {
        if self.stack.is_empty() {
            self.assign_global(name, value)?;
        } else {
            self.assign_local(0, name, value)?;
        }

        Ok(())
    }

    fn pop_frame(&mut self) {
        self.stack.pop();
        // Clean up local scopes once back to global
        if self.stack.is_empty() {
            self.locals.clear();
        }
    }

    // evaluate an expression
    pub fn evaluate(&mut self, expr: &Expression) -> Result<ReturnValue, InterpreterError> {
        match expr {
            Expression::Literal(value) => Ok(ReturnValue::Value(value.clone())),
            Expression::Unary { operator, rhs } => self.eval_unary(operator, rhs),
            Expression::Grouping { expr } => self.evaluate(expr),
            Expression::Binary { lhs, operator, rhs } => self.eval_binary(lhs, operator, rhs),
            Expression::Assign {
                name,
                value,
                scope_depth,
            } => self.eval_assign(name, value, scope_depth),
            Expression::Variable { name, scope_depth } => self.lookup_variable(name, scope_depth),
            Expression::Call {
                callee,
                arguments,
                pos,
            } => {
                let callee = self.evaluate(callee)?;
                let mut args = vec![];

                if let Some(arguments) = arguments {
                    for arg in arguments {
                        args.push(self.evaluate(arg)?);
                    }
                }

                callee.call(self, args, pos)
            }
            _ => todo!(),
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

    fn assign_local(
        &mut self,
        scope_depth: usize,
        name: &Identifier,
        value: ReturnValue,
    ) -> Result<(), InterpreterError> {
        if self.stack.len() < scope_depth + 1 {
            // assignment assumes more nested scopes than exist
            dbg!("depth > stack");
            return Err(InterpreterError::Error);
        }

        let local_scope = self
            .locals
            .get_mut(self.stack[self.stack.len() - (scope_depth + 1)]);

        if let Some(local_scope) = local_scope {
            local_scope.borrow_mut().insert(name.name.clone(), value);
        } else {
            // missing expected scope
            dbg!("missing expected scope");
            return Err(InterpreterError::Error);
        }

        Ok(())
    }

    fn assign_global(
        &mut self,
        name: &Identifier,
        value: ReturnValue,
    ) -> Result<(), InterpreterError> {
        self.globals.insert(name.name.clone(), value);

        Ok(())
    }

    fn lookup_variable(
        &self,
        name: &Identifier,
        scope_depth: &Option<usize>,
    ) -> Result<ReturnValue, InterpreterError> {
        if let Some(depth) = scope_depth {
            let maybe_value = self.lookup_local(&name.name, *depth)?;

            if let Some(value) = maybe_value {
                return Ok(value);
            }
            // not found, must be global
            self.lookup_global(&name.name)
        } else {
            self.lookup_global(&name.name)
        }
    }

    fn lookup_global(&self, name: &String) -> Result<ReturnValue, InterpreterError> {
        let value = self.globals.get(name);
        if let Some(value) = value {
            Ok(value.clone())
        } else {
            // add proper error
            dbg!(name);
            dbg!("undefined variable");
            Err(InterpreterError::Error)
        }
    }

    fn lookup_local(
        &self,
        name: &String,
        depth: usize,
    ) -> Result<Option<ReturnValue>, InterpreterError> {
        for (_, scope_pos) in self.stack.iter().enumerate().rev().skip(depth) {
            let value;

            if let Some(local_scope) = self.locals.get(*scope_pos) {
                value = local_scope.borrow_mut().get(name).cloned();
            } else {
                // missing expected scope
                dbg!("missing expected scope");
                return Err(InterpreterError::Error);
            }

            if value.is_some() {
                return Ok(value);
            }
        }
        Ok(None)
    }

    fn eval_assign(
        &mut self,
        name: &Identifier,
        value: &Expression,
        scope_depth: &Option<usize>,
    ) -> Result<ReturnValue, InterpreterError> {
        let value = self.evaluate(value)?;

        if let Some(scope_depth) = scope_depth {
            self.assign_local(*scope_depth, name, value.clone())?;
        } else {
            self.assign_global(name, value.clone())?;
        }

        Ok(value)
    }

    fn eval_unary(
        &mut self,
        operator: &Token,
        rhs: &Expression,
    ) -> Result<ReturnValue, InterpreterError> {
        let value = self.evaluate(rhs)?;

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

    fn eval_binary(
        &mut self,
        lhs: &Expression,
        operator: &Token,
        rhs: &Expression,
    ) -> Result<ReturnValue, InterpreterError> {
        let lhs = self.evaluate(lhs)?;
        let rhs = self.evaluate(rhs)?;

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

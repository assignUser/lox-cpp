use crate::parser::Block;
use crate::parser::Expression;
use crate::parser::Function;
use crate::parser::Identifier;
use crate::parser::Statement;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    Error,
}

pub trait Resolvable {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), ResolverError>;
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolvable for Statement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), ResolverError> {
        match self {
            Self::Block(block) => block.resolve(resolver)?,
            Self::Var(var) => {
                resolver.declare(&var.name);
                if let Some(init) = &mut var.initializer {
                    init.resolve(resolver)?;
                }
                resolver.define(&var.name);
            }
            Self::Function(Function {
                name,
                parameters,
                body,
            }) => {
                resolver.declare(name);
                // allow functions to recursively call themselves
                resolver.define(name);

                resolver.begin_scope();

                for param in parameters.iter().flatten() {
                    resolver.declare(param);
                    resolver.define(param);
                }

                body.resolve(resolver)?;

                resolver.end_scope();
            }
            Self::Expression(expr) => expr.resolve(resolver)?,
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.resolve(resolver)?;
                then_branch.resolve(resolver)?;
                if let Some(else_stmt) = else_branch {
                    else_stmt.resolve(resolver)?;
                }
            }
            Self::Print(expr) => expr.resolve(resolver)?,
            Self::Return { keyword: _, value } => {
                if let Some(expr) = value {
                    expr.resolve(resolver)?;
                }
            }
            Self::While { condition, body } => {
                condition.resolve(resolver)?;
                body.resolve(resolver)?;
            }
            Self::Class(_) => todo!(),
        };
        Ok(())
    }
}

impl Resolvable for Block {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), ResolverError> {
        resolver.begin_scope();
        for statement in self.body.iter_mut() {
            statement.resolve(resolver)?;
        }
        resolver.end_scope();
        Ok(())
    }
}

impl Resolvable for Expression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), ResolverError> {
        match self {
            Self::Variable { name, scope_depth } => {
                if !resolver.scopes.is_empty()
                    && resolver
                        .scopes
                        .last()
                        // We want to check if the variable that's being used is declared but not defined -> currently in
                        // initializer, so None = false, variable not defined -> see chapter 8; true
                        // -> ok, false -> declared but not defined -> error
                        .map_or(false, |scope| !scope.get(&name.name).unwrap_or(&true))
                {
                    // Lox.error(expr.name,
                    // "Can't read local variable in its own initializer.");
                    return Err(ResolverError::Error);
                } else {
                    *scope_depth = resolver.get_depth(&name.name);
                }
            }
            Self::Assign {
                name,
                value,
                scope_depth,
            } => {
                value.resolve(resolver)?;
                *scope_depth = resolver.get_depth(&name.name);
            }
            Self::Binary { lhs, operator: _, rhs } => {
                lhs.resolve(resolver)?;
                rhs.resolve(resolver)?;
            }
            Self::Grouping { expr } => expr.resolve(resolver)?,
            Self::Literal(_value) => {}
            Self::Unary { operator: _, rhs } => rhs.resolve(resolver)?,
            Self::Call {
                callee,
                arguments,
                pos:_,
            } => {
                callee.resolve(resolver)?;

                for arg in arguments.iter_mut().flatten() {
                    arg.resolve(resolver)?;
                }
            }
            _ => todo!(),
        };
        Ok(())
    }
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: vec![] }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: &Identifier) {
        self.set_scope(name.name.clone(), false);
    }

    pub fn define(&mut self, name: &Identifier) {
        self.set_scope(name.name.clone(), true);
    }

    fn set_scope(&mut self, name: String, value: bool) {
        self.scopes.last_mut().and_then(|s| s.insert(name, value));
    }

    pub fn get_depth(&self, name: &String) -> Option<usize> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.get(name).is_some() {
                return Some(self.scopes.len() - 1 - i);
            }
        }
        None
    }

    pub fn resolve(&mut self, statements: &mut Vec<Statement>) -> Result<(), ResolverError> {
        self.scopes.clear();

        for statement in statements {
            statement.resolve(self)?;
        }

        Ok(())
    }
}

use super::parser::{Statement, Expression, Program};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Object {
    Int(usize),
    Bool(bool),
    None
}

impl Object {
    fn inspect(&self) -> String {
        match &self {
            Object::Int(value) => format!("{value}"),
            Object::Bool(value) => format!("{value}"),
            Object::None => "NULL".into(),
        }
    }
}

pub trait AstNode {
    fn eval(&self) -> Object;
}

impl AstNode for Statement {
    fn eval(&self) -> Object {
        match &self {
            Statement::Let { name, value } => evaluate_let_statement(name, value),
            Statement::Return { value } => evaluate_return_statement(value),
            Statement::ExpressionStatement { value } => value.eval(),
        }
    }
}

fn evaluate_let_statement(name: &str, value: &Expression) -> Object {
    todo!();
}

fn evaluate_return_statement(value: &Expression) -> Object {
    todo!();
}

impl AstNode for Program {
    fn eval(&self) -> Object {
        if self.statements.is_empty() {
            return Object::None;
        }

        return self.statements
            .iter()
            .map(|statement| statement.eval())
            .last().unwrap();
    }
}

impl AstNode for Expression {
    fn eval(&self) -> Object {
        use Object::*;

        match &self{
            Expression::Ident(_) => todo!(),
            Expression::Int(value) => Int(*value),
            Expression::Bool(value) => Bool(*value),
            Expression::PrefixExpression { operator, right } => todo!(),
            Expression::InfixExpression { left, operator, right } => todo!(),
            Expression::If { condition, consequence, alternative } => todo!(),
            Expression::Function { parameters, body } => todo!(),
            Expression::CallExpression { function, arguments } => todo!(),
        }
    }
}

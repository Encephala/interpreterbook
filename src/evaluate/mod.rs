use super::parser::{Statement, Expression, Program, PrefixOperator};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Object {
    Int(isize),
    Bool(bool),
    Null
}

impl Object {
    fn inspect(&self) -> String {
        match &self {
            Object::Int(value) => format!("{value}"),
            Object::Bool(value) => format!("{value}"),
            Object::Null => "NULL".into(),
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
            return Object::Null;
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
            Expression::Int(value) => Int(*value as isize),
            Expression::Bool(value) => Bool(*value),
            Expression::PrefixExpression { operator, right } => evaluate_prefix_expression(operator, right.as_ref()),
            Expression::InfixExpression { left, operator, right } => todo!(),
            Expression::If { condition, consequence, alternative } => todo!(),
            Expression::Function { parameters, body } => todo!(),
            Expression::CallExpression { function, arguments } => todo!(),
        }
    }
}

fn evaluate_prefix_expression(operator: &PrefixOperator, right: &Expression) -> Object {
    let result = right.eval();

    return match operator {
        PrefixOperator::Minus => todo!(),
        PrefixOperator::Bang => evaluate_prefix_bang(result),
    };
}

fn evaluate_prefix_bang(right: Object) -> Object {
    use Object::*;

    match right {
        Bool(value) => Bool(!value),
        Int(value) => Bool(value == 0),
        Null => Null,
    }
}

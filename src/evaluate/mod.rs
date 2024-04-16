use crate::parser::BlockStatement;

use super::parser::{Statement, Expression, Program, PrefixOperator, InfixOperator};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(isize),
    Bool(bool),
    Null,
}

use Object::*;

impl Object {
    fn inspect(&self) -> String {
        match &self {
            Object::Int(value) => format!("{value}"),
            Object::Bool(value) => format!("{value}"),
            Object::Null => format!("NULL"),
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Int(value) => write!(f, "{}", value),
            Bool(value) => write!(f, "{}", value),
            Null => f.write_str("NULL"),
        }
    }
}

pub trait AstNode {
    fn eval(&self) -> Result<Object, String>;
}

impl AstNode for Statement {
    fn eval(&self) -> Result<Object, String> {
        match &self {
            Statement::Let { name, value } => evaluate_let_statement(name, value),
            Statement::Return { value } => evaluate_return_statement(value),
            Statement::ExpressionStatement { value } => value.eval(),
        }
    }
}

fn evaluate_let_statement(name: &str, value: &Expression) -> Result<Object, String> {
    todo!();
}

fn evaluate_return_statement(value: &Expression) -> Result<Object, String> {
    todo!();
}

impl AstNode for Program {
    fn eval(&self) -> Result<Object, String> {
        if self.statements.is_empty() {
            return Err("No statements to execute".into());
        }

        return self.statements
            .iter()
            .map(|statement| statement.eval())
            .last().unwrap();
    }
}

impl AstNode for Expression {
    fn eval(&self) -> Result<Object, String> {
        match &self{
            Expression::Ident(_) => todo!(),
            Expression::Int(value) => Ok(Int(*value as isize)),
            Expression::Bool(value) => Ok(Bool(*value)),
            Expression::PrefixExpression { operator, right } => evaluate_prefix_expression(operator, right.as_ref()),
            Expression::InfixExpression { left, operator, right } => evaluate_infix_expression(left.as_ref(), operator, right.as_ref()),
            Expression::If { condition, consequence, alternative } => evaluate_conditional_expression(condition, consequence, alternative),
            Expression::Function { parameters, body } => todo!(),
            Expression::CallExpression { function, arguments } => todo!(),
        }
    }
}

impl AstNode for BlockStatement {
    fn eval(&self) -> Result<Object, String> {
        self.statements.iter().for_each(|statement| { statement.eval(); } );

        todo!();
    }
}

fn evaluate_prefix_expression(operator: &PrefixOperator, right: &Expression) -> Result<Object, String> {
    let right = right.eval()?;

    return match operator {
        PrefixOperator::Minus => evaluate_prefix_minus(right),
        PrefixOperator::Bang => evaluate_prefix_bang(right),
    };
}

fn evaluate_prefix_bang(right: Object) -> Result<Object, String> {
    match right {
        Bool(value) => Ok(Bool(!value)),
        Int(value) => Ok(Bool(value == 0)),
        _ => Err(format!("{:?} is not booleanish", right)),
    }
}

fn evaluate_prefix_minus(right: Object) -> Result<Object, String> {
    match right {
        Int(value) => Ok(Int(-value)),
        _ => Err(format!("{:?} can't be negated", right)),
    }
}

fn evaluate_infix_expression(left: &Expression, operator: &InfixOperator, right: &Expression) -> Result<Object, String> {
    let left = &left.eval()?;
    let right = &right.eval()?;

    return evaluate_infix(left, operator, right);
}

fn evaluate_infix(left: &Object, operator: &InfixOperator, right: &Object) -> Result<Object, String> {
    if let (Int(left), Int(right)) = (left, right) {
        return evaluate_infix_integer(*left, operator, *right);
    }

    if let (Bool(left), Bool(right)) = (left, right) {
        return evaluate_infix_boolean(*left, operator, *right);
    }

    return Err(format!("Can't apply {:?} to incompatible types {:?} and {:?}", operator, left, right));
}

fn evaluate_infix_integer(left: isize, operator: &InfixOperator, right: isize) -> Result<Object, String> {
    match operator {
        InfixOperator::Plus => Ok(Int(left + right)),
        InfixOperator::Minus => Ok(Int(left - right)),
        InfixOperator::Multiply => Ok(Int(left * right)),
        InfixOperator::Divide => Ok(Int(left / right)),
        InfixOperator::GreaterThan => Ok(Bool(left > right)),
        InfixOperator::LessThan => Ok(Bool(left < right)),
        InfixOperator::Equals => Ok(Bool(left == right)),
        InfixOperator::NotEquals => Ok(Bool(left != right))
    }
}

fn evaluate_infix_boolean(left: bool, operator: &InfixOperator, right: bool) -> Result<Object, String> {
    match operator {
        InfixOperator::Plus => Ok(Bool(left || right)),
        InfixOperator::Minus => Ok(Bool(left && !right)),
        InfixOperator::Multiply => Ok(Bool(left && right)),
        InfixOperator::GreaterThan => Ok(Bool(left && !right)),
        InfixOperator::LessThan => Ok(Bool(!left && right)),
        InfixOperator::Equals => Ok(Bool(left == right)),
        InfixOperator::NotEquals => Ok(Bool(left != right)),
        InfixOperator::Divide => Err(format!("Can't apply boolean {:?} to {:?} and {:?}", operator, left, right))
    }
}

fn evaluate_conditional_expression(condition: &Expression, consequence: &BlockStatement, alternative: &Option<BlockStatement>) -> Result<Object, String> {
    if let Bool(value) = condition.eval()? {
        if value {
            return consequence.eval();
        }

        if let Some(alternative) = alternative {
            return alternative.eval();
        }

        return Err(format!("Condition {:?} evaluated to false but if statement had no alternative", condition));
    }

    return Err(format!("Condition {:?} didn't evaluate to a Boolean object", condition));
}

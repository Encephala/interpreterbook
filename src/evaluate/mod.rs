use super::parser::{Statement, Expression, Program, PrefixOperator, InfixOperator};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(isize),
    Bool(bool),
    Return(Box<Object>),
    None,
}

use Object::*;

impl Object {
    fn as_truthy(&self) -> Result<Object, String> {
        match &self {
            Int(value) => Ok(Bool(*value != 0)),
            Bool(value) => Ok(Bool(*value)),
            _ => Err(format!("{:?} is not booleanish", self))
        }
    }

    fn as_number(&self) -> Result<Object, String> {
        match &self {
            Int(value) => Ok(Int(*value)),
            _ => Err(format!("{:?} can't be cast to integer", self))
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Int(value) => write!(f, "{value}"),
            Bool(value) => write!(f, "{value}"),
            Return(value) => write!(f, "{value}"),
            None => f.write_str("None"),
        }
    }
}

pub trait AstNode {
    fn evaluate(&self) -> Result<Object, String>;
}

impl AstNode for Statement {
    fn evaluate(&self) -> Result<Object, String> {
        match &self {
            Statement::Let { name, value } => evaluate_let_statement(name, value),
            Statement::Return { value } => evaluate_return_statement(value),
            Statement::ExpressionStatement { value } => value.evaluate(),
        }
    }
}

fn evaluate_let_statement(name: &str, value: &Expression) -> Result<Object, String> {
    todo!();
}

fn evaluate_return_statement(value: &Expression) -> Result<Object, String> {
    return Ok(Object::Return(value.evaluate()?.into()));
}

impl AstNode for Program {
    fn evaluate(&self) -> Result<Object, String> {
        return evaluate_statements(&self.statements);
    }
}

fn evaluate_statements(statements: &[Statement]) -> Result<Object, String> {
    // If no statements, result is None
    let mut result = Object::None;

    for statement in statements.iter() {
        result = statement.evaluate()?;

        // Early return, destructuring the Return object
        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    return Ok(result);
}

impl AstNode for Expression {
    fn evaluate(&self) -> Result<Object, String> {
        match &self{
            Expression::Empty => Ok(Object::None),
            Expression::Ident(_) => todo!(),
            Expression::Int(value) => Ok(Int(*value as isize)),
            Expression::Bool(value) => Ok(Bool(*value)),
            Expression::Block(statements) => evaluate_statements(statements),
            Expression::PrefixExpression { operator, right } => evaluate_prefix_expression(operator, right.as_ref()),
            Expression::InfixExpression { left, operator, right } => evaluate_infix_expression(left.as_ref(), operator, right.as_ref()),
            Expression::If { condition, consequence, alternative } => evaluate_conditional_expression(condition, consequence, alternative),
            Expression::Function { parameters, body } => todo!(),
            Expression::CallExpression { function, arguments } => todo!(),
        }
    }
}

fn evaluate_prefix_expression(operator: &PrefixOperator, right: &Expression) -> Result<Object, String> {
    let right = right.evaluate()?;

    return match operator {
        PrefixOperator::Minus => evaluate_prefix_minus(right),
        PrefixOperator::Bang => evaluate_prefix_bang(right),
    };
}

fn evaluate_prefix_bang(right: Object) -> Result<Object, String> {
    if let Bool(value) = right.as_truthy()? {
        return Ok(Bool(!value));
    }

    // The above will always match, because as_truthy returns Object::Bool
    panic!("Can I stop being stupid");
}

fn evaluate_prefix_minus(right: Object) -> Result<Object, String> {
    if let Int(value) = right.as_number()? {
        return Ok(Int(-value));
    }

    // The above will always match, because as_integer returns Object::Int
    panic!("Can I stop being stupid");
}

fn evaluate_infix_expression(left: &Expression, operator: &InfixOperator, right: &Expression) -> Result<Object, String> {
    let left = &left.evaluate()?;
    let right = &right.evaluate()?;

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

fn evaluate_conditional_expression(condition: &Expression, consequence: &Expression, alternative: &Option<Box<Expression>>) -> Result<Object, String> {
    if let Bool(value) = condition.evaluate()?.as_truthy()? {
        if value {
            return consequence.evaluate();
        }

        if let Some(alternative) = alternative {
            return alternative.evaluate();
        }

        return Ok(Object::None);
    }

    return Err(format!("Condition {:?} didn't evaluate to a Boolean object", condition));
}

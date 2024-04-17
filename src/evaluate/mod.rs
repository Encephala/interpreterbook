use std::collections::HashMap;

use super::parser::{Statement, Expression, Program, PrefixOperator, InfixOperator};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(isize),
    Bool(bool),
    Return(Box<Object>),
    Function{ parameters: Vec<String>, body: Box<Expression> },
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
            Function{ parameters, body } => write!(
                f,
                "fn({parameters:?}) {{{body:?}}}"
            ),
            None => f.write_str("None"),
        }
    }
}

pub trait AstNode {
    fn evaluate(&self, environment: &mut ExecutionEnvironment) -> Result<Object, String>;
}

impl AstNode for Statement {
    fn evaluate(&self, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
        match &self {
            Statement::Let { name, value } => {
                evaluate_let_statement(name, value, environment)
            },
            Statement::Return { value } => {
                evaluate_return_statement(value, environment)
            },
            Statement::ExpressionStatement { value } => {
                value.evaluate(environment)
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExecutionEnvironment(
    HashMap<String, Object>
);

impl ExecutionEnvironment {
    pub fn new() -> Self {
        return Self(HashMap::new());
    }

    pub fn get(&mut self, key: &str) -> Result<Object, String> {
        let result = self.0.get(key);

        return result.map(Object::clone).ok_or(format!("Variable {key} doesn't exist"));
    }

    pub fn insert(&mut self, key: &str, value: Object) {
        self.0.insert(key.into(), value);
    }
}

fn evaluate_let_statement(name: &str, value: &Expression, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
    let value = value.evaluate(environment)?;

    environment.insert(name, value);

    return Ok(Object::None);
}

fn evaluate_return_statement(value: &Expression, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
    return Ok(Object::Return(value.evaluate(environment)?.into()));
}

impl AstNode for Program {
    fn evaluate(&self, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
        // If no statements, result is None
        let mut result = Object::None;

        for statement in self.statements.iter() {
            result = statement.evaluate(environment)?;

            // Early return, destructuring the Return object
            if let Object::Return(value) = result {
                return Ok(*value);
            }
        }

        return Ok(result);
    }
}

impl AstNode for Expression {
    fn evaluate(&self, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
        match &self{
            Expression::Empty => Ok(Object::None),
            Expression::Ident(name) => environment.get(name),
            Expression::Int(value) => Ok(Int(*value as isize)),
            Expression::Bool(value) => Ok(Bool(*value)),
            Expression::Block(statements) => evaluate_block_expression(statements, environment),
            Expression::PrefixExpression { operator, right } => {
                evaluate_prefix_expression(operator, right.as_ref(), environment)
            },
            Expression::InfixExpression { left, operator, right } => {
                evaluate_infix_expression(left.as_ref(), operator, right.as_ref(), environment)
            },
            Expression::If { condition, consequence, alternative } => {
                evaluate_conditional_expression(condition, consequence, alternative, environment)
            },
            Expression::Function {
                parameters,
                body
            } => Ok(Object::Function{
                // TODO: Can we avoid cloning?
                // Don't know yet, maybe if we just give Object a lifetime and work with refs?
                parameters: parameters.clone(),
                body: body.clone(),
            }),
            Expression::CallExpression {
                function, arguments
            } => evaluate_function_call(function, arguments, environment)
        }
    }
}

fn evaluate_block_expression(statements: &[Statement], environment: &mut ExecutionEnvironment) -> Result<Object, String> {
    // If no statements, result is None
    let mut result = Object::None;

    for statement in statements.iter() {
        result = statement.evaluate(environment)?;

        // Early return, without destructuring
        if let Object::Return(value) = result {
            return Ok(Object::Return(value));
        }
    }

    return Ok(result);
}

fn evaluate_prefix_expression(
    operator: &PrefixOperator,
    right: &Expression,
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    let right = right.evaluate(environment)?;

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

fn evaluate_infix_expression(
    left: &Expression,
    operator: &InfixOperator,
    right: &Expression,
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    let left = &left.evaluate(environment)?;
    let right = &right.evaluate(environment)?;

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

fn evaluate_conditional_expression(
    condition: &Expression,
    consequence: &Expression,
    alternative: &Option<Box<Expression>>,
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    if let Bool(value) = condition.evaluate(environment)?.as_truthy()? {
        if value {
            return consequence.evaluate(environment);
        }

        if let Some(alternative) = alternative {
            return alternative.evaluate(environment);
        }

        return Ok(Object::None);
    }

    return Err(format!("Condition {:?} didn't evaluate to a Boolean object", condition));
}

fn evaluate_function_call(function: &Expression,
    arguments: &Vec<Expression>,
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    let function = function.evaluate(environment)?;

    let arguments  = arguments.into_iter()
        .map(|argument| argument.evaluate(environment))
        .collect::<Result<Vec<Object>, String>>()?;

    if let Object::Function{ parameters, body } = function {
        if arguments.len() != parameters.len() {
            return Err(
                format!("Number of arguments {:?} does not equal number of parameters {:?}",
                arguments.len(),
                parameters.len())
            );
        }

        let mut environment = environment.clone();

        parameters.iter().zip(arguments).for_each(|(parameter, argument)| environment.insert(parameter, argument));

        return body.evaluate(&mut environment);
    } else {
        panic!("I did the stupid again");
    }
}

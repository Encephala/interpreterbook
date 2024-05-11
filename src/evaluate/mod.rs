mod builtins;

use std::collections::HashMap;

use super::parser::{Statement, Expression, Program, PrefixOperator, InfixOperator, Modifiable};
use builtins::BuiltinFunction;

#[cfg(test)]
mod tests;


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(isize),
    Bool(bool),
    Str(String),
    Return(Box<Object>),
    Function{ parameters: Vec<String>, body: Box<Expression>, environment: ExecutionEnvironment },
    Builtin(BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Quote(Expression),
    Macro { parameters: Vec<String>, body: Box<Expression>, environment: ExecutionEnvironment },
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
            Str(value) => f.write_str(value),
            Return(value) => write!(f, "{value}"),
            Function{ parameters, body, .. } => write!(
                f,
                "fn({parameters:?}) {{{body:?}}}"
            ),
            Builtin(builtin) => write!(
                f,
                "Builtin {builtin}"
            ),
            Array(values) => write!(
                f,
                "[{}]",
                values.iter().map(|object| format!("{object}")).collect::<Vec<_>>().join(", ")
            ),
            Hash(value) => write!(f, "HashMap {value:?}"),
            Quote(value) => write!(f, "{:?}", value),
            Macro { parameters, body, .. } => write!(
                f,
                "macro({parameters:?}) {{{body:?}}}"
            ),
            None => f.write_str("None"),
        }
    }
}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self {
            Object::Hash(_) => panic!("Can't be fucked to do this tbh"),
            _ => std::mem::discriminant(self).hash(state),
        }
    }
}

// For inside unquote calls, where we have to convert an object back into an expression
impl From<Object> for Expression {
    fn from(value: Object) -> Self {
        use Expression as E;

        match value {
            Int(value) => {
                let negative = value < 0;

                if negative {
                    E::PrefixExpression { operator: PrefixOperator::Negate, right: E::Int((-value) as usize).into() }
                } else {
                    E::Int(value as usize)
                }
            }
            Bool(value) => E::Bool(value),
            Str(value) => E::Str(value),
            Quote(value) => value,
            other => panic!("Tried to convert object {other:?} into an Expression, but that's not possible")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExecutionEnvironment {
    variables: HashMap<String, Object>,
    builtins: HashMap<String, Object>
}

impl std::hash::Hash for ExecutionEnvironment {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        panic!("Can't be bothered to do this");
    }
}


impl ExecutionEnvironment {
    pub fn new() -> Self {
        let mut result = Self { variables: HashMap::new(), builtins: HashMap::new() };

        result.register_builtins();

        return result;
    }

    pub fn insert(&mut self, key: String, value: Object) {
        self.variables.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        return self.variables.get(key).or(self.builtins.get(key));
    }


    // For macro environments
    fn add_macro(&mut self, statement: Statement) {
        let Statement::Let { name, value } = statement
        else {
            panic!("Tried adding a macro that isn't actually a Macro (not a Let)")
        };

        let Expression::Macro { parameters, body } = *value
        else {
            println!("blabla");
            panic!("Tried adding a macro that isn't actually a Macro (not a Macro)")
        };

        self.insert(name, Object::Macro { parameters, body, environment: self.clone() })
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


fn evaluate_let_statement(name: &str, value: &Expression, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
    let value = value.evaluate(environment)?;

    environment.insert(name.into(), value);

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
        }

        // Destructure the final result if it's a Return
        if let Object::Return(value) = result {
            return Ok(*value);
        }

        return Ok(result);
    }
}

impl AstNode for Expression {
    fn evaluate(&self, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
        match &self {
            Expression::Empty => Ok(Object::None),
            Expression::Ident(name) => {
                environment.get(name)
                    .map(Object::clone)
                    .ok_or(format!("Identifier '{name}' doesn't exist"))
            },
            Expression::Int(value) => Ok(Int(*value as isize)),
            Expression::Str(value) => Ok(Str(value.clone())),
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
                // Don't know yet
                parameters: parameters.clone(),
                body: body.clone(),
                environment: environment.clone()
            }),
            Expression::CallExpression {
                function, arguments
            } => evaluate_function_call(function, arguments, environment),
            Expression::Array(value) => Ok(Array(
                value.iter()
                    .map(|expression| expression.evaluate(environment))
                    .collect::<Result<Vec<_>, String>>()?
            )),
            Expression::Index { into, index } => evaluate_infix_index(
                into,
                index,
                environment
            ),
            Expression::HashLiteral(value) => evaluate_hash_literal(value, environment),
            Expression::Quote(value) => process_quote(value, environment),
            Expression::Unquote(_) => Err("Used `Unquote` function outside of `quote` environment".into()),
            Expression::Macro { .. } => panic!("Tried evaluating macro at runtime"),
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
        PrefixOperator::Negate => evaluate_prefix_negate(right),
        PrefixOperator::Not => evaluate_prefix_not(right),
    };
}

fn evaluate_prefix_not(right: Object) -> Result<Object, String> {
    if let Bool(value) = right.as_truthy()? {
        return Ok(Bool(!value));
    }

    // The above will always match, because as_truthy returns Object::Bool
    panic!("Can I stop being stupid");
}

fn evaluate_prefix_negate(right: Object) -> Result<Object, String> {
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

    if let (Str(left), Str(right)) = (left, right) {
        return evaluate_infix_string(left, operator, right);
    }

    return Err(format!("Can't apply {:?} to incompatible types {:?} and {:?}", operator, left, right));
}

fn evaluate_infix_integer(left: isize, operator: &InfixOperator, right: isize) -> Result<Object, String> {
    use InfixOperator::*;

    match operator {
        Add => Ok(Int(left + right)),
        Subtract => Ok(Int(left - right)),
        Multiply => Ok(Int(left * right)),
        Divide => Ok(Int(left / right)),
        GreaterThan => Ok(Bool(left > right)),
        LessThan => Ok(Bool(left < right)),
        Equals => Ok(Bool(left == right)),
        NotEquals => Ok(Bool(left != right)),
        _ => Err(format!("Invalid operation {left:?} {operator:?} {right:?}")),
    }
}

fn evaluate_infix_boolean(left: bool, operator: &InfixOperator, right: bool) -> Result<Object, String> {
    use InfixOperator::*;

    match operator {
        Add => Ok(Bool(left || right)),
        Subtract => Ok(Bool(left && !right)),
        Multiply => Ok(Bool(left && right)),
        GreaterThan => Ok(Bool(left && !right)),
        LessThan => Ok(Bool(!left && right)),
        Equals => Ok(Bool(left == right)),
        NotEquals => Ok(Bool(left != right)),
        _ => Err(format!("Invalid operation {left:?} {operator:?} {right:?}")),
    }
}

fn evaluate_infix_string(left: &String, operator: &InfixOperator, right: &String) -> Result<Object, String> {
    match operator {
        InfixOperator::Add => Ok(Str(left.to_owned() + right)),
        InfixOperator::Subtract => {
            if left.ends_with(right) {
                Ok(Str(left.chars().take(left.len() - right.len()).collect::<String>()))
            } else {
                Err(format!("Left '{left}' doesn't start with right '{right}'"))
            }
        },
        InfixOperator::Equals => Ok(Bool(left == right)),
        InfixOperator::NotEquals => Ok(Bool(left != right)),
        _ => Err(format!("Can't apply string {:?} to {left} and {right}", operator)),
    }
}

fn evaluate_infix_index(
    left: &Expression,
    right: &Expression,
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    match left.evaluate(environment)? {
        Object::Array(left) => {
            if let Object::Int(right) = right.evaluate(environment)? {
                return left.get(right as usize)
                    .map(|object| { object.to_owned()})
                    .ok_or(format!("Index {right} out of bounds"));
            } else {
                return Err(format!("{right:?} is an invalid index"));
            }
        },
        Object::Hash(left) => {
            let right = right.evaluate(environment)?;

            left.get(&right).cloned().ok_or(
                format!("{right:?} not found in map")
            )
        },
        _ => return Err(format!("Tried indexing into {left:?} which isn't supported")),
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

fn evaluate_function_call(
    function: &Expression,
    arguments: &[Expression],
    environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    let function = function.evaluate(environment)?;

    let arguments  = arguments.iter()
        .map(|argument| argument.evaluate(environment))
        .collect::<Result<Vec<Object>, String>>()?;

    if let Object::Function { parameters, body, mut environment} = function {
        if arguments.len() != parameters.len() {
            return Err(
                format!("Number of arguments {:?} does not equal number of parameters {:?}",
                arguments.len(),
                parameters.len())
            );
        }

        parameters.iter().zip(arguments).for_each(|(parameter, argument)| {
            environment.insert(parameter.into(), argument);
        });

        let result = body.evaluate(&mut environment)?;

        // Destructure value if it's a Return
        if let Object::Return(value) = result {
            return Ok(*value);
        }

        return Ok(result);
    } else if let Object::Builtin(builtin) = function {
        return builtin.call(arguments);
    } else {
        return Err(format!("{function:?} isn't a function"));
    }
}

fn evaluate_hash_literal(
    value: &HashMap<Expression,
    Expression>, environment: &mut ExecutionEnvironment
) -> Result<Object, String> {
    let mut result = HashMap::new();

    value.iter()
        .map(|(key, value)| -> Result<(), String> {
        let key = key.evaluate(environment)?;
        let value = value.evaluate(environment)?;

        result.insert(key, value);

        return Ok(());
    }).collect::<Result<Vec<_>, String>>()?;

    return Ok(Object::Hash(result));
}

fn process_quote(expression: &Expression, environment: &mut ExecutionEnvironment) -> Result<Object, String> {
    let mut map_unquote = |expression| {
        if let Expression::Unquote(expression) = expression {
            Ok(expression.evaluate(environment)?.into())
        } else {
            Ok(expression)
        }
    };

    let expression = (*expression).clone().modify(&mut map_unquote)?;

    return Ok(Object::Quote(expression));
}

impl Program {
    fn define_macros(mut self, macro_env: &mut ExecutionEnvironment) -> Self {
        // Hell yeah nesting
        // Add macros to macro_env, keep only remaining statements
        // (filter_map instead of filter because filter_map owns its parameters)
        let filtered_statements = self.statements.into_iter().filter_map(|statement| {
            let Statement::Let { value, ..} = statement.clone() else { return Some(statement); };
            if matches!(value.as_ref(), Expression::Macro { .. }) {
                macro_env.add_macro(statement);

                Option::None
            } else {
                Some(statement)
            }
        }).collect::<Vec<_>>();

        self.statements = filtered_statements;

        return self;
    }

    pub fn expand_macros(mut self, macro_env: &mut ExecutionEnvironment) -> Result<Program, String> {
        self = self.define_macros(macro_env);

        return self.modify(&mut |expression| {
            let Some(the_macro) = cast_to_macro(&expression, macro_env)
            else {
                return Ok(expression);
            };

            let Expression::CallExpression { arguments, .. } = expression
            else {
                panic!("This never happens, as cast_to_macro also does this destructuring")
            };

            let quoted_arguments = quote_arguments(arguments);

            let Object::Macro { parameters, body, environment } = the_macro
            else {
                panic!("This never happens");
            };

            let mut evaluation_env = extend_environment(parameters, quoted_arguments, environment);

            let result = body.evaluate(&mut evaluation_env)?;

            if !matches!(result, Object::Quote(_)) {
                return Err("Macros must return a Quote object".into());
            }

            return Ok(result.into());
        })
    }
}

fn cast_to_macro(expression: &Expression, macro_env: &mut ExecutionEnvironment) -> Option<Object> {
    if let Expression::CallExpression { function, .. } = expression {
        let Expression::Ident(name) = function.as_ref()
        else {
            return Option::None;
        };

        let Some(object) = macro_env.get(name).cloned()
        else {
            return Option::None;
        };

        if matches!(object, Object::Macro { .. }) {
            return Some(object);
        }
    }

    println!("Not a call expression");
    return Option::None;
}

fn quote_arguments(arguments: Vec<Expression>) -> Vec<Object> {
    return arguments.into_iter().map(Quote).collect();
}

fn extend_environment(parameters: Vec<String>, arguments: Vec<Object>, mut environment: ExecutionEnvironment) -> ExecutionEnvironment {
    parameters.clone().into_iter().zip(arguments).for_each(|(parameter, argument)| {
        environment.insert(parameter, argument);
    });

    return environment;
}

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use super::{ExecutionEnvironment, Object};


#[derive(Debug, PartialEq, Clone, EnumIter)]
pub enum BuiltinFunction {
    Len,
}

impl BuiltinFunction {
    pub fn call(&self, arguments: Vec<Object>) -> Result<Object, String> {
        use BuiltinFunction::*;

        match &self {
            Len => len(arguments),
        }
    }
}

impl std::fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BuiltinFunction::*;

        match &self {
            Len => f.write_str("len"),
        }
    }
}

impl ExecutionEnvironment {
    pub fn register_builtins(&mut self) {
        BuiltinFunction::iter().for_each(|builtin| {
            self.insert(
                builtin.to_string(),
                Object::Builtin(builtin)
            );
        });
    }
}

fn len(parameters: Vec<Object>) -> Result<Object, String> {
    if parameters.is_empty() {
        return Err("Called len with no parameters".into());
    }

    let result = parameters.iter().map(|parameter| {
        match parameter {
            Object::Str(value) => {
                Ok(Object::Int(value.len() as isize))
            }
            _ => Err(format!("Parameter {parameter:?} of call to len doesn't have a length"))
        }
    }).collect::<Result<Vec<Object>, String>>()?;

    match result.len() {
        1 => Ok(result.into_iter().next().unwrap()),
        _ => Ok(Object::Array(result)),
    }
}

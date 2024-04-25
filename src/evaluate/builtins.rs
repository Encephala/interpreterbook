use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use super::{ExecutionEnvironment, Object};


#[derive(Debug, PartialEq, Eq, Clone, EnumIter, Hash)]
pub enum BuiltinFunction {
    Len,
    First,
    Rest,
    Push,
    Puts,
}

impl BuiltinFunction {
    pub fn call(&self, arguments: Vec<Object>) -> Result<Object, String> {
        use BuiltinFunction::*;

        match &self {
            Len => len(arguments),
            First => first(arguments),
            Rest => rest(arguments),
            Push => push(arguments),
            Puts => puts(arguments),
        }
    }
}

impl std::fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BuiltinFunction::*;

        match &self {
            Len => f.write_str("len"),
            First => f.write_str("first"),
            Rest => f.write_str("rest"),
            Push => f.write_str("push"),
            Puts => f.write_str("puts"),
        }
    }
}

impl ExecutionEnvironment {
    pub fn register_builtins(&mut self) {
        BuiltinFunction::iter().for_each(|builtin| {
            self.builtins.insert(
                builtin.to_string(),
                Object::Builtin(builtin)
            );
        });
    }
}

fn len(arguments: Vec<Object>) -> Result<Object, String> {
    if arguments.len() != 1 {
        return Err(format!("Called len with invalid number of arguments {}", arguments.len()));
    }

    match arguments.first().unwrap() {
        Object::Str(value) => {
            Ok(Object::Int(value.len() as isize))
        }
        Object::Array(value) => {
            Ok(Object::Int(value.len() as isize))
        },
        other => Err(format!("Argument {other:?} is in valid in len"))
    }
}

fn first(arguments: Vec<Object>) -> Result<Object, String> {
    if arguments.len() != 1 {
        return Err(format!("Called first with invalid number of arguments {}", arguments.len()));
    }

    match arguments.first().unwrap() {
        Object::Array(value) => {
            if value.is_empty() {
                Err(format!("Argument {value:?} is empty in first"))
            } else {
                Ok(value.first().unwrap().clone())
            }
        },
        other => Err(format!("Argument {other:?} is invalid in first")),
    }
}

fn rest(arguments: Vec<Object>) -> Result<Object, String> {
    if arguments.len() != 1 {
        return Err(format!("Called rest with invalid number of arguments {}", arguments.len()));
    }

    match arguments.first().unwrap() {
        Object::Array(value) => {
            if value.is_empty() {
                Err(format!("Argument {value:?} is empty in rest"))
            } else {
                Ok(Object::Array(value.into_iter().skip(1).cloned().collect()))
            }
        },
        other => Err(format!("Argument {other:?} is invalid in first")),
    }
}

fn push(arguments: Vec<Object>) -> Result<Object, String> {
    if arguments.len() != 2 {
        return Err(format!("Called push with invalid number of arguments {}", arguments.len()));
    }

    let mut iter = arguments.iter().cloned();

    let array = iter.next().unwrap();
    let new = iter.next().unwrap();

    match array {
        Object::Array(mut array) => {
            array.push(new);
            Ok(Object::Array(array))
        },
        other => Err(format!("Argument {other:?} is invalid in push")),
    }
}

fn puts(arguments: Vec<Object>) -> Result<Object, String> {
    println!("{}", arguments.iter()
        .map(|argument| argument.to_string())
        .collect::<Vec<_>>()
        .join(" "));

    return Ok(Object::None);
}

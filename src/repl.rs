use std::io::{stdin, stdout, Write};

use super::parser::Parser;
use super::evaluate::{Object, AstNode, ExecutionEnvironment};

pub fn start() {
    let stdin = stdin();
    let mut stdout = stdout();

    let mut environment = ExecutionEnvironment::new();

    loop {
        print!(">> ");

        let _ = stdout.flush();

        let mut input = String::new();

        let _ = stdin.read_line(&mut input);

        // Exit on "exit" or CTRL+D
        if input == "exit\n" {
            break;
        } else if input.is_empty() {
            println!();
            break;
        }

        if input == "env\n" {
            println!("{environment:?}");
            continue;
        }

        let mut program = Parser::new(&input).parse_program();

        if !program.errors.is_empty() {
            println!("Error(s) while parsing: {:?}", program.errors);
            continue;
        }

        match program.expand_macros(&mut environment) {
            Ok(new_program) => program = new_program,
            Err(message) => { println!("Error while expanding macros: {message}"); continue },
        }

        let result = program.evaluate(&mut environment);

        match result {
            Ok(value) => {
                if value != Object::None {
                    println!("{value}")
                }
            },
            Err(message) => println!("Error while executing: {:?}", message),
        }
    }
}

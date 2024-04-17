use std::io::{stdin, stdout, Write};

use super::parser::Parser;
use super::evaluate::{AstNode, ExecutionEnvironment};

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

        let program = Parser::new(&input).parse_program();

        let mut environment = ExecutionEnvironment::new();

        if !program.errors.is_empty() {
            println!("Error(s) while parsing: {:?}", program.errors);
            continue;
        }

        let result = program.evaluate(&mut environment);

        if let Err(message) = result {
            println!("Error while executing: {:?}", message);
            continue;
        }

        println!("{}", result.unwrap());
    }
}

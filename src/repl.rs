use std::io::{stdin, stdout, Write};

use super::lexer::{Lexer, Token};
use super::parser::Parser;

impl<'a> Lexer<'a> {
    #[allow(dead_code)]
    pub fn collect_input_to_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        loop {
            let token = self.next_token();

            tokens.push(token);

            if tokens.last().unwrap() == &Token::Eof {
                break;
            }
        }

        return tokens
    }
}

pub fn start() {
    let stdin = stdin();
    let mut stdout = stdout();

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

        if !program.errors.is_empty() {
            println!("Error(s) while parsing: {:?}", program.errors);
        }

        println!("Parsed: {:#?}", program.statements);
    }
}

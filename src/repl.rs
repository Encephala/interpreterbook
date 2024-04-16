use std::io::{stdin, stdout, Write};

use super::lexer::{Lexer, Token};

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

        let mut input = String::new();

        let _ = stdout.flush();

        let _ = stdin.read_line(&mut input);

        // Exit on "stop", "exit" or CTRL+D
        if input == "exit\n" {
            break;
        } else if input.is_empty() {
            println!();
            break;
        }

        let mut lexer = Lexer::new(&input);

        println!("{:?}", lexer.collect_input_to_tokens());
    }
}

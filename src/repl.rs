use std::io::{stdin, stdout, Write};

use super::lexer::Lexer;

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

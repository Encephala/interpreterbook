use std::str;

#[derive(Debug, PartialEq)]
pub enum Token {
    IDENT(String),
    INT(usize),

    ILLEGAL,
    EOF,

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    EQUALS,
    NOTEQUALS,
    LESSTHAN,
    GREATERTHAN,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    FUNCTION,
    RETURN,
}

use Token::*;


#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    index: usize,
    next_index: usize,
    char: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Self {
            input: input.as_bytes(), // Assume input is always an ASCII string
            index: 0,
            next_index: 0,
            char: 0,
        };

        lexer.read_char();

        return lexer;
    }

    fn peek(&self) -> u8 {
        if self.index >= self.input.len() {
            return 0;
        }

        return self.input[self.next_index];
    }

    fn read_char(&mut self) {
        if self.next_index >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input[self.next_index];
        }

        self.index = self.next_index;
        self.next_index += 1;
    }

    fn read_identifier(&mut self) -> Token {

        let start_index = self.index;

        // identifier starts with a letter but may contain numbers after that first letter
        while self.char.is_ascii_alphanumeric() {
            self.read_char();
        }

        return match &self.input[start_index..self.index] {
            b"let" => LET,
            b"true" => TRUE,
            b"false" => FALSE,
            b"if" => IF,
            b"else" => ELSE,
            b"fn" => FUNCTION,
            b"return" => RETURN,
            _ => IDENT(str::from_utf8(&self.input[start_index..self.index]).unwrap().into())
        };
    }

    fn read_number(&mut self) -> Token {
        let start_index = self.index;

        while self.char.is_ascii_digit() {
            self.read_char()
        }

        return INT(str::from_utf8(&self.input[start_index..self.index]).unwrap().parse().unwrap());
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.char {
            c if c.is_ascii_alphabetic() => return self.read_identifier(),
            c if c.is_ascii_digit() => return self.read_number(),
            b'=' => {
                        if self.peek() == b'=' {
                            self.read_char();
                            EQUALS
                        } else {
                            ASSIGN
                        }
                    }
            b'+' => PLUS,
            b'-' => MINUS,
            b'!' => {
                        if self.peek() == b'=' {
                            self.read_char();
                            NOTEQUALS
                        } else {
                            BANG
                        }
                    }
            b'*' => ASTERISK,
            b'/' => SLASH,
            b'<' => LESSTHAN,
            b'>' => GREATERTHAN,
            b',' => COMMA,
            b';' => SEMICOLON,
            b'(' => LPAREN,
            b')' => RPAREN,
            b'{' => LBRACE,
            b'}' => RBRACE,
            0 => EOF,
            _ => ILLEGAL
        };

        self.read_char();

        return token;
    }

    pub fn collect_input_to_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        loop {
            let token = self.next_token();

            tokens.push(token);

            if tokens.last().unwrap() == &EOF {
                break;
            }
        }

        return tokens
    }
}

#[cfg(test)]
mod tests {
    use super::{Token::*, Lexer};

    #[test]
    fn next_token_basic_functionality() {

        let input = "=+(){},;";

        let mut lexer = Lexer::new(input);

        let expected_result = vec![
            ASSIGN,
            PLUS,
            LPAREN,
            RPAREN,
            LBRACE,
            RBRACE,
            COMMA,
            SEMICOLON,
            EOF,
        ];

        let true_result = lexer.collect_input_to_tokens();

        assert_eq!(expected_result, true_result);
    }

    #[test]
    fn next_token_real_world_input() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);";

        let mut lexer = Lexer::new(input);

        let true_result = lexer.collect_input_to_tokens();

        let expected_result = vec![
            LET,
            IDENT("five".into()),
            ASSIGN,
            INT(5),
            SEMICOLON,
            LET,
            IDENT("ten".into()),
            ASSIGN,
            INT(10),
            SEMICOLON,
            LET,
            IDENT("add".into()),
            ASSIGN,
            FUNCTION,
            LPAREN,
            IDENT("x".into()),
            COMMA,
            IDENT("y".into()),
            RPAREN,
            LBRACE,
            IDENT("x".into()),
            PLUS,
            IDENT("y".into()),
            SEMICOLON,
            RBRACE,
            SEMICOLON,
            LET,
            IDENT("result".into()),
            ASSIGN,
            IDENT("add".into()),
            LPAREN,
            IDENT("five".into()),
            COMMA,
            IDENT("ten".into()),
            RPAREN,
            SEMICOLON,
            EOF,
        ];

        assert_eq!(true_result, expected_result);
    }

    // Test some specific characters and keywords like `true` and `false`
    #[test]
    fn next_token_extended_token_set() {
        let input = "!-/*5;
        5 < 10 > 5;
        if (5 < 10) {
        return true;
        } else {
        return false;
        }
        10 == 10;
        10 != 9;";

        let mut lexer = Lexer::new(input);

        let true_result = lexer.collect_input_to_tokens();

        let expected_result = vec![
            BANG,
            MINUS,
            SLASH,
            ASTERISK,
            INT(5),
            SEMICOLON,
            INT(5),
            LESSTHAN,
            INT(10),
            GREATERTHAN,
            INT(5),
            SEMICOLON,
            IF,
            LPAREN,
            INT(5),
            LESSTHAN,
            INT(10),
            RPAREN,
            LBRACE,
            RETURN,
            TRUE,
            SEMICOLON,
            RBRACE,
            ELSE,
            LBRACE,
            RETURN,
            FALSE,
            SEMICOLON,
            RBRACE,
            INT(10),
            EQUALS,
            INT(10),
            SEMICOLON,
            INT(10),
            NOTEQUALS,
            INT(9),
            SEMICOLON,
            EOF
        ];

        assert_eq!(true_result, expected_result);
    }
}

use std::str;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Token {
    Ident(String),
    Int(String),
    Str(String),

    Illegal,
    Eof,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,

    Comma,
    Colon,
    Semicolon,
    Apostrophe,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    HashStart, // #
    HashEnd, // $

    Let,
    True,
    False,
    If,
    Else,
    Function,
    Return,
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
        if self.index + 1 >= self.input.len() {
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
        while self.char.is_ascii_alphanumeric() || self.char == b'_' {
            self.read_char();
        }

        return match &self.input[start_index..self.index] {
            b"let" => Let,
            b"true" => True,
            b"false" => False,
            b"if" => If,
            b"else" => Else,
            b"fn" => Function,
            b"return" => Return,
            _ => Ident(str::from_utf8(&self.input[start_index..self.index]).unwrap().into())
        };
    }

    fn read_number(&mut self) -> Token {
        let start_index = self.index;

        while self.char.is_ascii_digit() {
            self.read_char()
        }

        return Int(str::from_utf8(&self.input[start_index..self.index]).unwrap().into());
    }

    fn read_string(&mut self) -> Token {
        // Skip apostrophe
        self.read_char();

        let start_index = self.index;

        while self.char != b'"' && self.char != b'\'' && self.char != 0 {
            self.read_char()
        }

        let result = str::from_utf8(&self.input[start_index..self.index]).unwrap().into();

        // Skip apostrophe
        self.read_char();

        return Str(result);
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.char {
            c if c.is_ascii_alphabetic() => return self.read_identifier(),
            c if c.is_ascii_digit() => return self.read_number(),
            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Equals
                } else {
                    Assign
                }
                    }
            b'+' => Plus,
            b'-' => Minus,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    NotEquals
                } else {
                    Bang
                }
                    }
            b'*' => Asterisk,
            b'/' => Slash,
            b'<' => LessThan,
            b'>' => GreaterThan,
            b',' => Comma,
            b':' => Colon,
            b';' => Semicolon,
            b'"' | b'\'' => return self.read_string(),
            b'(' => LParen,
            b')' => RParen,
            b'{' => LBrace,
            b'}' => RBrace,
            b'#' => HashStart,
            b'$' => HashEnd,
            b'[' => LBracket,
            b']' => RBracket,
            0 => Eof,
            _ => Illegal
        };

        self.read_char();

        return token;
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, Token::*, Lexer};

    impl<'a> Lexer<'a> {
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

    #[test]
    fn next_token_basic_functionality() {

        let input = "=+(){},;";

        let mut lexer = Lexer::new(input);

        let expected_result = vec![
            Assign,
            Plus,
            LParen,
            RParen,
            LBrace,
            RBrace,
            Comma,
            Semicolon,
            Eof,
        ];

        let true_result = lexer.collect_input_to_tokens();

        assert_eq!(expected_result, true_result);
    }

    #[test]
    fn next_token_real_world_input() {
        let input = "let five_num = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);

        'epic_string';

        [1, 2];";

        let mut lexer = Lexer::new(input);

        let true_result = lexer.collect_input_to_tokens();

        let expected_result = vec![
            Let,
            Ident("five_num".into()),
            Assign,
            Int("5".into()),
            Semicolon,
            Let,
            Ident("ten".into()),
            Assign,
            Int("10".into()),
            Semicolon,
            Let,
            Ident("add".into()),
            Assign,
            Function,
            LParen,
            Ident("x".into()),
            Comma,
            Ident("y".into()),
            RParen,
            LBrace,
            Ident("x".into()),
            Plus,
            Ident("y".into()),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident("result".into()),
            Assign,
            Ident("add".into()),
            LParen,
            Ident("five".into()),
            Comma,
            Ident("ten".into()),
            RParen,
            Semicolon,
            Str("epic_string".into()),
            Semicolon,
            LBracket,
            Int("1".into()),
            Comma,
            Int("2".into()),
            RBracket,
            Semicolon,
            Eof,
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
        10 != 9;
        #1: 5$";

        let mut lexer = Lexer::new(input);

        let true_result = lexer.collect_input_to_tokens();

        let expected_result = vec![
            Bang,
            Minus,
            Slash,
            Asterisk,
            Int("5".into()),
            Semicolon,
            Int("5".into()),
            LessThan,
            Int("10".into()),
            GreaterThan,
            Int("5".into()),
            Semicolon,
            If,
            LParen,
            Int("5".into()),
            LessThan,
            Int("10".into()),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            False,
            Semicolon,
            RBrace,
            Int("10".into()),
            Equals,
            Int("10".into()),
            Semicolon,
            Int("10".into()),
            NotEquals,
            Int("9".into()),
            Semicolon,
            HashStart,
            Int("1".into()),
            Colon,
            Int("5".into()),
            HashEnd,
            Eof
        ];

        assert_eq!(true_result, expected_result);
    }
}

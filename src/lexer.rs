use std::str;

#[derive(Debug, PartialEq)]
enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(usize),

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    index: usize,
    next_index: usize,
    char: u8,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Self {
            input: input.as_bytes(), // Assume input is always an ASCII string
            index: 0,
            next_index: 0,
            char: 0,
        };

        lexer.read_char();

        return lexer;
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
            b"fn" => Token::FUNCTION,
            b"let" => Token::LET,
            _ => Token::IDENT(str::from_utf8(&self.input[start_index..self.index]).unwrap().into())
        };
    }

    fn read_number(&mut self) -> Token {
        let start_index = self.index;

        while self.char.is_ascii_digit() {
            self.read_char()
        }

        return Token::INT(str::from_utf8(&self.input[start_index..self.index]).unwrap().parse().unwrap());
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn next_token(&mut self) -> Token {
        use Token::*;

        self.skip_whitespace();

        let token = match self.char {
            c if c.is_ascii_alphabetic() => return self.read_identifier(),
            c if c.is_ascii_digit() => return self.read_number(),
            b'=' => ASSIGN,
            b';' => SEMICOLON,
            b'(' => LPAREN,
            b')' => RPAREN,
            b',' => COMMA,
            b'+' => PLUS,
            b'{' => LBRACE,
            b'}' => RBRACE,
            0 => EOF,
            _ => ILLEGAL
        };

        self.read_char();

        return token;
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, Lexer};

    #[test]
    fn next_token_basic_functionality() {
        use Token::*;

        let input = "=+(){},;";

        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), ASSIGN);
        assert_eq!(lexer.next_token(), PLUS);
        assert_eq!(lexer.next_token(), LPAREN);
        assert_eq!(lexer.next_token(), RPAREN);
        assert_eq!(lexer.next_token(), LBRACE);
        assert_eq!(lexer.next_token(), RBRACE);
        assert_eq!(lexer.next_token(), COMMA);
        assert_eq!(lexer.next_token(), SEMICOLON);
    }

    #[test]
    fn next_token_real_world_input() {
        use Token::*;

        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);";

        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), LET);
        assert_eq!(lexer.next_token(), IDENT("five".into()));
        assert_eq!(lexer.next_token(), ASSIGN);
        assert_eq!(lexer.next_token(), INT(5));
        assert_eq!(lexer.next_token(), SEMICOLON);
        assert_eq!(lexer.next_token(), LET);
        assert_eq!(lexer.next_token(), IDENT("ten".into()));
        assert_eq!(lexer.next_token(), ASSIGN);
        assert_eq!(lexer.next_token(), INT(10));
        assert_eq!(lexer.next_token(), SEMICOLON);
        assert_eq!(lexer.next_token(), LET);
        assert_eq!(lexer.next_token(), IDENT("add".into()));
        assert_eq!(lexer.next_token(), ASSIGN);
        assert_eq!(lexer.next_token(), FUNCTION);
        assert_eq!(lexer.next_token(), LPAREN);
        assert_eq!(lexer.next_token(), IDENT("x".into()));
        assert_eq!(lexer.next_token(), COMMA);
        assert_eq!(lexer.next_token(), IDENT("y".into()));
        assert_eq!(lexer.next_token(), RPAREN);
        assert_eq!(lexer.next_token(), LBRACE);
        assert_eq!(lexer.next_token(), IDENT("x".into()));
        assert_eq!(lexer.next_token(), PLUS);
        assert_eq!(lexer.next_token(), IDENT("y".into()));
        assert_eq!(lexer.next_token(), SEMICOLON);
        assert_eq!(lexer.next_token(), RBRACE);
        assert_eq!(lexer.next_token(), SEMICOLON);
        assert_eq!(lexer.next_token(), LET);
        assert_eq!(lexer.next_token(), IDENT("result".into()));
        assert_eq!(lexer.next_token(), ASSIGN);
        assert_eq!(lexer.next_token(), IDENT("add".into()));
        assert_eq!(lexer.next_token(), LPAREN);
        assert_eq!(lexer.next_token(), IDENT("five".into()));
        assert_eq!(lexer.next_token(), COMMA);
        assert_eq!(lexer.next_token(), IDENT("ten".into()));
        assert_eq!(lexer.next_token(), RPAREN);
        assert_eq!(lexer.next_token(), SEMICOLON);
        assert_eq!(lexer.next_token(), EOF);
    }
}

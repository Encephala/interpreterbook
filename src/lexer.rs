use std::str::{Bytes};

#[derive(Debug, PartialEq)]
enum TokenType {
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

struct Token {
    token_type: TokenType,
    literal: u8
}

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a [u8],
    index: usize,
    next_index: usize,
    char: u8
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Self {
            input: input.as_bytes(), // Assume input is always an ASCII string
            index: 0,
            next_index: 0,
            char: 0
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

    fn next_token(&mut self) -> Token {
        use TokenType::*;

        let token_type = match self.char {
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

        return Token { token_type, literal: self.char }
    }
}

#[cfg(test)]
mod tests {
    use super::{TokenType, Lexer};

    #[test]
    fn text_next_token() {
        let input = "=+(){},;";

        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::ASSIGN);
        assert_eq!(lexer.next_token().token_type, TokenType::PLUS);
        assert_eq!(lexer.next_token().token_type, TokenType::LPAREN);
        assert_eq!(lexer.next_token().token_type, TokenType::RPAREN);
        assert_eq!(lexer.next_token().token_type, TokenType::LBRACE);
        assert_eq!(lexer.next_token().token_type, TokenType::RBRACE);
        assert_eq!(lexer.next_token().token_type, TokenType::COMMA);
        assert_eq!(lexer.next_token().token_type, TokenType::SEMICOLON);
    }
}

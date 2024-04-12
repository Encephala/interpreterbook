use super::lexer::{Token, Lexer};


struct Program {
    statements: Vec<Statement>
}


#[derive(Debug)]
enum Statement {
    Let{ name: String, value: Expression },
}

#[derive(Debug)]
struct LetStatement {
    name: String,
    value: Expression
}


#[derive(Debug)]
enum Expression {
    Todo,
}


struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    next_token: Token,
    errors: Vec<String>
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        let mut lexer = Lexer::new(input);

        let first_token = lexer.next_token();
        let second_token = lexer.next_token();

        return Self { lexer, token: first_token, next_token: second_token, errors: vec![] };
    }

    fn next_token(&mut self) {
        // Move next token into current token
        std::mem::swap(&mut self.token, &mut self.next_token);

        // Overwrite next token (dropping old value of current token)
        self.next_token = self.lexer.next_token();
    }

    fn check_and_proceed(&mut self, token: Token) -> Result<&str, String> {
        if self.token != token {
            return Err(
                format!("Expected token {:?}, found {:?}", token, &self.token)
            );
        }

        self.next_token();

        // Dummy return
        return Ok("");
    }

    fn consume_until_semicolon(&mut self) {
        while self.token != Token::Semicolon {
            self.next_token();
        }

        // Skip semicolon itself as well
        self.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut result = Program { statements: vec![] };

        while self.token != Token::Eof {
            let statement = self.parse_statement();

            match statement {
                Ok(statement) => result.statements.push(statement),
                Err(message) => {
                    self.errors.push(format!("Failed to parse statement: {message}"));

                    // Ignore remainder of statement
                    // Idk if this is good behaviour; if we ignore a statement, future code probably doesn't parse
                    self.consume_until_semicolon();
                }
            }
        }

        if !self.errors.is_empty() {
            println!("")
        }

        return result;
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        use Token::*;

        return match self.token {
            Let => self.parse_let_statement(),
            _ => Err("Unknown token matched".into())
        };
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let name: String;

        if let Token::Ident(_name) = &self.token {
            name = _name.clone();
        } else {
            return Err(
                format!("Token in let {:?} not an identifier", &self.token)
            );
        }

        self.next_token();

        self.check_and_proceed(Token::Assign)?;

        // TODO make this the code when parse expression works
        // let value = self.parse_expression()?;

        let value = Expression::Todo;

        self.next_token();

        self.check_and_proceed(Token::Semicolon)?;

        return Ok(Statement::Let { name, value });
    }

    fn parse_expression(&mut self) -> Result<Statement, String> {
        todo!();
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_statements_literal_value() {
        let input = "let x = 5;
        let y = 10;
        let foobar = 838383;";

        let mut parser = Parser::new(input);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            panic!("Got {} parsing error(s):\n{:?}\nSucceeded in parsing: {:?}",
                parser.errors.len(),
                parser.errors,
                program.statements
            );
        }

        assert_eq!(program.statements.len(), 3);

        let expected_names = vec!["x", "y", "foobar"];

        program.statements.iter().zip(expected_names).for_each(|(statement, name)| {
            test_single_let_statement(statement, name)
        })
    }

    fn test_single_let_statement(statement: &Statement, expected_name: &str) {
        if let Statement::Let{ name, value } = statement {
            assert_eq!(name, expected_name);
        } else {
            panic!("Testing statement {:?} that isn't a Let statement", statement);
        }
    }

    #[test]
    fn parse_let_yields_correct_errors() {
        let input = "let x 5;
        let = 10;
        let 838383;";

        let mut parser = Parser::new(input);

        let program = parser.parse_program();

        dbg!(&program.statements);
        dbg!(&parser.errors);

        assert_eq!(parser.errors.len(), 3);

        assert_eq!(parser.errors, vec![
            "Failed to parse statement: Expected token Assign, found Int(5)",
            "Failed to parse statement: Token in let Assign not an identifier",
            "Failed to parse statement: Token in let Int(838383) not an identifier"
        ]);
    }
}

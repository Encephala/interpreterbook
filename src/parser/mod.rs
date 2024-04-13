use super::lexer::{Token, Lexer};


struct Program {
    statements: Vec<Statement>
}


#[derive(Debug)]
enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    ExpressionStatement { value: Expression },
}


#[derive(Debug, PartialEq)]
enum Expression {
    Ident(String),
    Todo,
}

// PartialOrd default implementation defines the order to be the order in which they were defined
#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call
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

    fn check_and_skip(&mut self, token: Token) -> Result<(), String> {
        if self.token != token {
            return Err(
                format!("Expected token {:?}, found {:?}", token, &self.token)
            );
        }

        self.next_token();

        return Ok(());
    }

    fn skip_if_token(&mut self, token: Token) {
        if self.token == token {
            self.next_token();
        }
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
                    // Idk if this is good behaviour
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
            Return => self.parse_return_statement(),
            _ => self.parse_expression_statement()
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

        self.check_and_skip(Token::Assign)?;

        let value = self.parse_expression(Precedence::Lowest)?;

        self.check_and_skip(Token::Semicolon)?;

        return Ok(Statement::Let { name, value });
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.check_and_skip(Token::Semicolon)?;

        return Ok(Statement::Return { value });
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let value = self.parse_expression(Precedence::Lowest)?;

        self.skip_if_token(Token::Semicolon);

        return Ok(Statement::ExpressionStatement { value });
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        if let Some(expression) = self.parse_prefix() {
            return expression;
        }

        // TODO: Handle what should happen if we don't have a matching prefix parser
        return Ok(Expression::Todo);
    }

    /// Return types:
    ///
    /// - `None` if prefix isn't defined for some token
    /// - `Some(Err(_))` if prefix is defined but couldn't parse correctly
    /// - `Some(Ok(_))` if all went well
    fn parse_prefix(&mut self) -> Option<Result<Expression, String>> {
        use Token::*;

        let result = match &self.token {
            Ident(name) => Some(Ok(Expression::Ident(name.clone()))),
            _ => None
        };

        self.next_token();

        return result;
    }

    /// Return types:
    ///
    /// - `None` if infix isn't defined for some token
    /// - `Some(Err(_))` if infix is defined but couldn't parse correctly
    /// - `Some(Ok(_))` if all went well
    fn parse_infix(&mut self) -> Option<Result<Expression, String>> {
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

        check_parser_errors(&parser, &program);

        assert_eq!(program.statements.len(), 3);

        let expected_names = vec!["x", "y", "foobar"];

        program.statements.iter().zip(expected_names).for_each(|(statement, name)| {
            validate_single_let_statement(statement, name)
        });
    }

    fn check_parser_errors(parser: &Parser, program: &Program) {
        if !parser.errors.is_empty() {
            panic!("Got {} parsing error(s):\n{:?}\nSucceeded in parsing: {:?}",
                parser.errors.len(),
                parser.errors,
                program.statements
            );
        }
    }

    fn validate_single_let_statement(statement: &Statement, expected_name: &str) {
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

    #[test]
    fn parse_return_literal_value() {
        let input = "return 5;
        return 10;
        return 993322;";

        let mut parser = Parser::new(input);

        let program = parser.parse_program();

        check_parser_errors(&parser, &program);

        assert_eq!(program.statements.len(), 3);

        program.statements.iter().for_each(|statement| validate_single_return_statement(statement));
    }

    fn validate_single_return_statement(statement: &Statement) {
        if let Statement::Return { .. } = statement {
        } else {
            panic!("Testing statement {:?} that isn't a Return statement", statement);
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let mut parser = Parser::new(input);

        let program = parser.parse_program();

        check_parser_errors(&parser, &program);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.first().unwrap();

        if let Statement::ExpressionStatement { value } = statement {
            assert_eq!(value, &Expression::Ident("foobar".into()));
        } else {
            panic!("Testing statement {:?} that isn't an Expression statement", statement);
        }
    }
}

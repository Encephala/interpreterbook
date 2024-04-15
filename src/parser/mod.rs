use std::collections::HashMap;

use super::lexer::{Token, Lexer};


struct Program {
    statements: Vec<Statement>
}


#[derive(Debug, PartialEq)]
enum Statement {
    Let { name: String, value: Box<Expression> },
    Return { value: Box<Expression> },
    ExpressionStatement { value: Box<Expression> },
}


#[derive(Debug, PartialEq)]
enum Expression {
    Ident(String),
    Int(usize),
    PrefixExpression { operator: PrefixOperator, right: Box<Expression> },
    InfixExpression { left: Box<Expression>, operator: InfixOperator, right: Box<Expression> },
}

#[derive(Debug, PartialEq)]
enum PrefixOperator {
    Minus,
    Bang,
}

impl From<&Token> for PrefixOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            _ => panic!("I did something stupid")
        }
    }
}

#[derive(Debug, PartialEq)]
enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals
}

impl From<&Token> for InfixOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::GreaterThan => InfixOperator::GreaterThan,
            Token::LessThan => InfixOperator::LessThan,
            Token::Equals => InfixOperator::Equals,
            Token::NotEquals => InfixOperator::NotEquals,
            _ => panic!("I did something stupid again")
        }
    }
}

// PartialOrd default implementation defines the order to be the order in which they were defined
#[derive(Debug, PartialEq, PartialOrd, Clone)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call
}


#[derive(Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    next_token: Token,
    errors: Vec<String>,
    precedence_map: HashMap<&'a Token, &'a Precedence>
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        let mut lexer = Lexer::new(input);

        let first_token = lexer.next_token();
        let second_token = lexer.next_token();

        let mut precedence_map = HashMap::new();

        [
            (Token::Equals, Precedence::Equals),
            (Token::NotEquals, Precedence::Equals),
            (Token::LessThan, Precedence::LessGreater),
            (Token::GreaterThan, Precedence::LessGreater),
            (Token::Plus, Precedence::Sum),
            (Token::Minus, Precedence::Sum),
            (Token::Slash, Precedence::Product),
            (Token::Asterisk, Precedence::Product),
        ].iter().for_each(|(token, precedence)| { precedence_map.insert(token, precedence); });

        return Self { lexer, token: first_token, next_token: second_token, errors: vec![], precedence_map };
    }

    fn next_token(&mut self) {
        // Move next token into current token
        std::mem::swap(&mut self.token, &mut self.next_token);

        // Overwrite next token (dropping old value of current token)
        self.next_token = self.lexer.next_token();
    }

    fn check_and_skip(&mut self, token: &Token) -> Result<(), String> {
        if &self.token != token {
            return Err(
                format!("Expected token {:?}, found {:?}", token, &self.token)
            );
        }

        self.next_token();

        return Ok(());
    }

    fn skip_if_token(&mut self, token: &Token) {
        if &self.token == token {
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

        self.check_and_skip(&Token::Assign)?;

        let value = self.parse_expression(&Precedence::Lowest)?;

        self.next_token();

        self.check_and_skip(&Token::Semicolon)?;

        return Ok(Statement::Let { name, value });
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        // Skip return keyword
        self.next_token();

        let value = self.parse_expression(&Precedence::Lowest)?;

        self.next_token();

        self.check_and_skip(&Token::Semicolon)?;

        return Ok(Statement::Return { value });
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let value = self.parse_expression(&Precedence::Lowest)?;

        self.next_token();

        // Expression statements have optional semicolon,
        // so that in REPL we don't have to type semicolon.
        self.skip_if_token(&Token::Semicolon);

        return Ok(Statement::ExpressionStatement { value });
    }

    fn parse_expression(&mut self, parent_precedence: &Precedence) -> Result<Box<Expression>, String> {
        let mut expression = self.parse_prefix()?;

        while self.next_token != Token::Semicolon && parent_precedence < self.next_precedence() {
            self.next_token();

            expression = self.parse_infix(expression)?;
        }

        return Ok(expression);
    }

    fn get_precedence(&self, token: &Token) -> &Precedence {
        return self.precedence_map.get(token).unwrap_or(&&Precedence::Lowest);
    }

    fn next_precedence(&self) -> &Precedence {
        return self.get_precedence(&self.next_token);
    }

    fn current_precedence(&self) -> &Precedence {
        return self.get_precedence(&self.token);
    }

    fn parse_prefix(&mut self) -> Result<Box<Expression>, String> {
        use Token::*;

        let result = match &self.token {
            Ident(name) => Ok(Box::new(Expression::Ident(name.clone()))),
            Int(value) => Ok(Box::new(Expression::Int(value.parse().unwrap()))),
            Bang => self.parse_prefix_expression(),
            Minus => self.parse_prefix_expression(),
            _ => Err(format!("No prefix parser found for {:?}", self.token))
        };

        return result;
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<Expression>, String> {
        let operator = (&self.token).into();

        // Skip past the operator itself
        self.next_token();

        return Ok(Box::new(Expression::PrefixExpression {
            operator,
            right: self.parse_expression(&Precedence::Prefix)?
        }));
    }

    fn parse_infix(&mut self, left: Box<Expression>) -> Result<Box<Expression>, String> {
        use Token::*;

        let result = match &self.token {
            Plus => self.parse_infix_expression(left),
            Minus => self.parse_infix_expression(left),
            Slash => self.parse_infix_expression(left),
            Asterisk => self.parse_infix_expression(left),
            Equals => self.parse_infix_expression(left),
            NotEquals => self.parse_infix_expression(left),
            LessThan => self.parse_infix_expression(left),
            GreaterThan => self.parse_infix_expression(left),
            _ => Err(format!("No infix parser found for {:?}", self.token))
        };

        return result;
    }

    fn can_parse_infix(&mut self) -> bool {
        use Token::*;

        return [
            Plus,
            Minus,
            Slash,
            Asterisk,
            Equals,
            NotEquals,
            LessThan,
            GreaterThan,
        ].contains(&self.token);
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Box<Expression>, String>  {
        let precedence = self.current_precedence().clone(); // Clone precedence to drop reference to self

        let operator = (&self.token).into();

        self.next_token();

        return Ok(Box::new(Expression::InfixExpression { left,
            operator,
            right: self.parse_expression(&precedence)?
        }));
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn parse_then_check_errors_and_length(input: &str, expected_length: usize) -> Program {
        let mut parser = Parser::new(input);

        let program = parser.parse_program();

        check_parser_errors(&parser, &program);

        assert_eq!(program.statements.len(), expected_length);

        return program;
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

    #[test]
    fn parse_let_statements_literal_value() {
        let input = "let x = 5;
        let y = 10;
        let foobar = 838383;";

        let program = parse_then_check_errors_and_length(input, 3);

        let expected_names = vec!["x", "y", "foobar"];

        program.statements.iter().zip(expected_names).for_each(|(statement, name)| {
            validate_single_let_statement(statement, name)
        });
    }

    fn validate_single_let_statement(statement: &Statement, expected_name: &str) {
        if let Statement::Let{ name, value: _value } = statement {
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

        let _ = parser.parse_program();

        assert_eq!(parser.errors.len(), 3);

        assert_eq!(parser.errors, vec![
            "Failed to parse statement: Expected token Assign, found Int(\"5\")",
            "Failed to parse statement: Token in let Assign not an identifier",
            "Failed to parse statement: Token in let Int(\"838383\") not an identifier"
        ]);
    }

    #[test]
    fn parse_return_literal_value() {
        let input = "return 5;
        return 10;
        return 993322;";

        let program = parse_then_check_errors_and_length(input, 3);

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

        let program = parse_then_check_errors_and_length(input, 1);

        let statement = program.statements.first().unwrap();

        if let Statement::ExpressionStatement { value } = statement {
            assert_eq!(**value, Expression::Ident("foobar".into()));
        } else {
            panic!("Testing statement {:?} that isn't an Expression statement", statement);
        }
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let program = parse_then_check_errors_and_length(input, 1);

        let statement = program.statements.first().unwrap();

        dbg!(&statement);

        if let Statement::ExpressionStatement { value } = statement {
            assert_eq!(**value, Expression::Int(5));
        } else {
            panic!("Testing statement {:?} that isn't an Expression statement", statement);
        }
    }

    #[test]
    fn prefix_operators_integer_literals() {
        struct TestCase<'a>(&'a str, PrefixOperator, Expression);

        let inputs = [
            TestCase("!5;", PrefixOperator::Bang, Expression::Int(5)),
            TestCase("-15;", PrefixOperator::Minus, Expression::Int(15)),
        ];

        inputs.iter().for_each(|test_case| {
            let program = parse_then_check_errors_and_length(test_case.0, 1);

            let statement = program.statements.first().unwrap();

            if let Statement::ExpressionStatement { value } = statement {
                if let Expression::PrefixExpression { operator, right } = value.as_ref() {
                    assert_eq!(operator, &test_case.1);
                    assert_eq!(**right, test_case.2);
                } else {
                    panic!("Testing expression {:?} that isn't a Prefix expression", value);
                }
            } else {
                panic!("Testing statement {:?} that isn't an Expression statement", statement);
            }
        })
    }

    #[test]
    fn infix_operators_integer_literals() {
        struct TestCase<'a>(&'a str, Box<Expression>, InfixOperator, Box<Expression>);

        let inputs = [
            TestCase("5 + 5;", Box::new(Expression::Int(5)), InfixOperator::Plus, Box::new(Expression::Int(5))),
            TestCase("5 - 5;", Box::new(Expression::Int(5)), InfixOperator::Minus, Box::new(Expression::Int(5))),
            TestCase("5 * 5;", Box::new(Expression::Int(5)), InfixOperator::Times, Box::new(Expression::Int(5))),
            TestCase("5 / 5;", Box::new(Expression::Int(5)), InfixOperator::Divide, Box::new(Expression::Int(5))),
            TestCase("5 > 5;", Box::new(Expression::Int(5)), InfixOperator::GreaterThan, Box::new(Expression::Int(5))),
            TestCase("5 < 5;", Box::new(Expression::Int(5)), InfixOperator::LessThan, Box::new(Expression::Int(5))),
            TestCase("5 == 5;", Box::new(Expression::Int(5)), InfixOperator::Equals, Box::new(Expression::Int(5))),
            TestCase("5 != 5;", Box::new(Expression::Int(5)), InfixOperator::NotEquals, Box::new(Expression::Int(5))),
        ];

        inputs.iter().for_each(|test_case| {
            let program = parse_then_check_errors_and_length(test_case.0, 1);

            let statement = program.statements.first().unwrap();

            if let Statement::ExpressionStatement { value } = statement {
                if let Expression::InfixExpression {
                    left,
                    operator,
                    right
                } = value.as_ref() {
                    assert_eq!(left, &test_case.1);
                    assert_eq!(operator, &test_case.2);
                    assert_eq!(right, &test_case.3);
                } else {
                    panic!("Testing expression {:?} that isn't an Infix expression", value);
                }
            } else {
                panic!("Testing statement {:?} that isn't an Expression statement", statement)
            }
        })
    }

    #[test]
    fn infix_operators_correct_precedence() {
        use Expression::*;

        struct TestCase<'a>(&'a str, Expression);

        [
            TestCase("-a * b", InfixExpression {
                left: Box::new(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    right: Box::new(Ident("a".into()))
                }),
                operator: InfixOperator::Times,
                right: Box::new(Ident("b".into()))
            }),

            TestCase("a * b + c", InfixExpression {
                left: Box::new(InfixExpression {
                    left: Box::new(Ident("a".into())),
                    operator: InfixOperator::Times,
                    right: Box::new(Ident("b".into()))
                }),
                operator: InfixOperator::Plus,
                right: Box::new(Ident("c".into()))
            }),

            TestCase("a + b * c", InfixExpression {
                left: Box::new(Ident("a".into())),
                operator: InfixOperator::Plus,
                right: Box::new(InfixExpression {
                    left: Box::new(Ident("b".into())),
                    operator: InfixOperator::Times,
                    right: Box::new(Ident("c".into()))
                })
            }),

            TestCase("5 < 4 != 3 > 4", InfixExpression {
                left: Box::new(InfixExpression {
                    left: Box::new(Int(5)),
                    operator: InfixOperator::LessThan,
                    right: Box::new(Int(4))
                }),
                operator: InfixOperator::NotEquals,
                right: Box::new(InfixExpression {
                    left: Box::new(Int(3)),
                    operator: InfixOperator::GreaterThan,
                    right: Box::new(Int(4))
                })
            })
        ].iter().for_each(|test_case| {
            let program = parse_then_check_errors_and_length(test_case.0, 1);

            let statement = program.statements.first().unwrap();

            if let Statement::ExpressionStatement { value } = statement {
                assert_eq!(**value, test_case.1);
            } else {
                panic!("Testing statement {:?} that isn't an Expression statement", statement);
            }
        });
    }
}

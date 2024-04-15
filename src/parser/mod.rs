#[cfg(test)]
mod tests;

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
    Bool(bool),
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

    fn consume_until_statement_end(&mut self) {
        while self.token != Token::Semicolon && self.token != Token::Eof {
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
                    self.consume_until_statement_end();
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
            True | False => Ok(Box::new(Expression::Bool(self.token == True))),
            Bang => self.parse_prefix_expression(),
            Minus => self.parse_prefix_expression(),
            LParen => self.parse_grouped_expression(),
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

    fn parse_grouped_expression(&mut self) -> Result<Box<Expression>, String> {
        self.next_token();

        let result = self.parse_expression(&Precedence::Lowest);

        self.next_token();

        if self.token != Token::RParen {
            return Err(format!("Expected token {:?}, found {:?}", Token::RParen, &self.token));
        }

        return result;
    }
}

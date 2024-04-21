#[cfg(test)]
mod tests;

use std::collections::HashMap;

use super::lexer::{Token, Lexer};


#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<String>
}


#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Statement {
    Let { name: String, value: Box<Expression> },
    Return { value: Box<Expression> },
    ExpressionStatement { value: Box<Expression> },
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Empty,
    Ident(String),
    Int(usize),
    Str(String),
    Bool(bool),
    Block(Vec<Statement>),
    PrefixExpression { operator: PrefixOperator, right: Box<Expression> },
    InfixExpression { left: Box<Expression>, operator: InfixOperator, right: Box<Expression> },
    If { condition: Box<Expression>, consequence: Box<Expression>, alternative: Option<Box<Expression>> },
    Function { parameters: Vec<String>, body: Box<Expression> },
    CallExpression {
        function: Box<Expression>, // Always either Ident or Function
        arguments: Vec<Expression>
    },
    Array(Vec<Expression>),
    Index { into: Box<Expression>, index: Box<Expression> },
    HashLiteral(HashMap<Expression, Expression>),
}

impl std::hash::Hash for Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self {
            Expression::HashLiteral(_) => panic!("Can't be fucked to do this tbh"),
            _ => std::mem::discriminant(self).hash(state),
        }
    }
}

impl From<&Token> for String {
    fn from(value: &Token) -> Self {
        match value {
            Token::Ident(name) => name.clone(),
            _ => panic!("I keep doing something stupid")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PrefixOperator {
    Negate,
    Not,
}

impl From<&Token> for PrefixOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Bang => PrefixOperator::Not,
            Token::Minus => PrefixOperator::Negate,
            _ => panic!("I did something stupid")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
    Index,
}

impl From<&Token> for InfixOperator {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => InfixOperator::Add,
            Token::Minus => InfixOperator::Subtract,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::GreaterThan => InfixOperator::GreaterThan,
            Token::LessThan => InfixOperator::LessThan,
            Token::Equals => InfixOperator::Equals,
            Token::NotEquals => InfixOperator::NotEquals,
            Token::LBracket => InfixOperator::Index,
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
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    next_token: Token,
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
            (Token::LParen, Precedence::Call),
            (Token::LBracket, Precedence::Call),
        ].iter().for_each(|(token, precedence)| { precedence_map.insert(token, precedence); });

        return Self { lexer, token: first_token, next_token: second_token, precedence_map };
    }

    fn next_token(&mut self) {
        // Move next token into current token
        std::mem::swap(&mut self.token, &mut self.next_token);

        // Overwrite next token (dropping old value of current token)
        self.next_token = self.lexer.next_token();
    }

    /// Asserts that current token matches and if so, skips it
    fn check_and_skip(&mut self, token: &Token) -> Result<(), String> {
        if &self.token != token {
            return Err(
                format!("Expected token {:?}, found {:?}", token, &self.token)
            );
        }

        self.next_token();

        return Ok(());
    }

    /// Asserts that next token matches and if so, skips to it
    fn check_next_and_skip_to(&mut self, token: &Token) -> Result<(), String> {
        if &self.next_token != token {
            return Err(format!(
                "Expected token {:?}, found {:?}", token, &self.next_token
            ))
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
        // TODO: This doesn't ignore semicolons in inner block statements
        // Should keep a running counter of how many blocks deep we are and only check Semicolon
        // if we're 0 blocks deep
        while self.token != Token::Semicolon && self.token != Token::Eof {
            self.next_token();
        }

        // Skip semicolon itself as well
        self.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut result = Program { statements: vec![], errors: vec![] };

        while self.token != Token::Eof {
            let statement = self.parse_statement();

            match statement {
                Ok(statement) => result.statements.push(statement),
                Err(message) => {
                    result.errors.push(format!("Failed to parse statement: {message}"));

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

        let result = match self.token {
            Let => self.parse_let_statement(),
            Return => self.parse_return_statement(),
            _ => self.parse_expression_statement()
        }?;

        self.next_token();

        // Expression statements have optional semicolon,
        // so that in REPL we don't have to type semicolon.
        self.skip_if_token(&Token::Semicolon);

        return Ok(result);
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

        return Ok(Statement::Let { name, value });
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        // Skip return keyword
        self.next_token();

        let value = self.parse_expression(&Precedence::Lowest)?;

        return Ok(Statement::Return { value });
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let value = self.parse_expression(&Precedence::Lowest)?;

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

    fn current_precedence(&self) -> Precedence {
        // Make owned value to drop reference to self on function end
        return self.get_precedence(&self.token).to_owned();
    }

    fn parse_prefix(&mut self) -> Result<Box<Expression>, String> {
        use Token::*;

        let result = match &self.token {
            Ident(name) => Ok(Expression::Ident(name.clone()).into()),
            Int(value) => Ok(Expression::Int(value.parse().unwrap()).into()),
            Str(value) => Ok(Expression::Str(value.clone()).into()),
            True | False => Ok(Expression::Bool(self.token == True).into()),
            Bang => self.parse_prefix_expression(),
            Minus => self.parse_prefix_expression(),
            LParen => self.parse_grouped_expression(),
            If => self.parse_if_expression(),
            Function => self.parse_function_literal(),
            Semicolon => Ok(Expression::Empty.into()), // When statement starts with semicolon, it is an empty statement
            LBrace => self.parse_block_expression(),
            LBracket => self.parse_expression_list(Token::RBracket).map(|expressions| {
                Expression::Array(expressions).into()
            }),
            HashStart => self.parse_hash_literal(),
            _ => Err(format!("No prefix parser found for {:?}", &self.token))
        };

        return result;
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<Expression>, String> {
        let operator = (&self.token).into();

        // Skip past the operator itself
        self.next_token();

        return Ok(Expression::PrefixExpression {
            operator,
            right: self.parse_expression(&Precedence::Prefix)?
        }.into());
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
            // Special treatment, because parameters to a call expression
            // are multiple right expressions rather than just one
            // Idea: could we avoid this special behaviour if in parse expression,
            // we check for a comment and if so parse an expression list?
            // I guess that would allow for Python's implicit tuples as well, kinda cool
            LParen => self.parse_call_expression(left),
            LBracket => self.parse_index_expression(left),
            _ => Err(format!("No infix parser found for {:?}", self.token))
        };

        return result;
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Box<Expression>, String>  {
        let precedence = self.current_precedence();

        let operator = (&self.token).into();

        self.next_token();

        return Ok(Expression::InfixExpression { left,
            operator,
            right: self.parse_expression(&precedence)?
        }.into());
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<Expression>, String> {
        self.next_token();

        let result = self.parse_expression(&Precedence::Lowest);

        self.check_next_and_skip_to(&Token::RParen)?;

        return result;
    }

    fn parse_if_expression(&mut self) -> Result<Box<Expression>, String> {
        self.next_token(); // If keyword

        self.check_and_skip(&Token::LParen)?;

        let condition = self.parse_expression(&Precedence::Lowest)?;

        self.next_token();

        self.check_and_skip(&Token::RParen)?;

        let consequence = self.parse_block_expression()?;

        let mut alternative = None;

        if self.next_token == Token::Else {
            self.next_token(); // Right brace
            self.next_token(); // Else keyword

            alternative = Some(self.parse_block_expression()?);
        }

        return Ok(Expression::If { condition, consequence, alternative }.into())
    }

    fn parse_block_expression(&mut self) -> Result<Box<Expression>, String> {
        self.check_and_skip(&Token::LBrace)?;

        let mut result = vec![];

        while self.token != Token::RBrace && self.token != Token::Eof {
            let statement = self.parse_statement()?;

            result.push(statement);
        }

        if self.token != Token::RBrace {
            return Err(
                format!("Expected token {:?}, found {:?}", Token::RBrace, &self.token)
            );
        }

        return Ok(Expression::Block(result).into());
    }

    fn parse_function_literal(&mut self) -> Result<Box<Expression>, String> {
        self.check_next_and_skip_to(&Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.next_token(); // Right parenthesis

        let body = self.parse_block_expression()?;

        return Ok(Expression::Function { parameters, body }.into());
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>, String> {
        let result = self.parse_expression_list(Token::RParen);

        return result.map(|expressions| {
            expressions.into_iter().map(|expression| {
                if let Expression::Ident(value) = expression {
                    Ok(value)
                } else {
                    Err(format!("Token {expression:?} isn't an identifier"))
                }
            }).collect::<Result<Vec<String>, String>>()
        })?;
    }

    fn parse_call_expression(&mut self, left: Box<Expression>) -> Result<Box<Expression>, String> {
        let arguments = self.parse_expression_list(Token::RParen)?;

        return Ok(Expression::CallExpression { function: left, arguments }.into())
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, String> {
        self.next_token();

        let mut result = vec![];

        if self.token == end {
            return Ok(result)
        }

        result.push(*self.parse_expression(&Precedence::Lowest)?);

        while self.next_token == Token::Comma {
            self.next_token();
            self.next_token();

            result.push(*self.parse_expression(&Precedence::Lowest)?);
        }

        self.check_next_and_skip_to(&end)?;

        return Ok(result);
    }

    fn parse_index_expression(&mut self, left: Box<Expression>) -> Result<Box<Expression>, String> {
        self.next_token();

        let result = Ok(Expression::Index {
            into: left,
            index: self.parse_expression(&Precedence::Lowest)?,
        }.into());

        self.check_next_and_skip_to(&Token::RBracket)?;

        return result;
    }

    fn parse_hash_literal(&mut self) -> Result<Box<Expression>, String> {
        let mut result = HashMap::new();

        println!("Before loop {:?}, {:?}", &self.token, &self.next_token);

        while self.next_token != Token::HashEnd {
            println!("Start loop {:?}, {:?}", &self.token, &self.next_token);
            self.next_token();

            let key = self.parse_expression(&Precedence::Lowest)?;

            self.check_next_and_skip_to(&Token::Colon)?;

            self.next_token();

            let value = self.parse_expression(&Precedence::Lowest)?;

            println!("End loop {:?}, {:?}", &self.token, &self.next_token);
            result.insert(*key, *value);

            if self.next_token == Token::Comma {
                self.next_token();
                continue;
            }
        }

        self.check_next_and_skip_to(&Token::HashEnd)?;

        return Ok(Expression::HashLiteral(result).into());
    }
}

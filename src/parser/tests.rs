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
fn prefix_operators() {
    struct TestCase<'a>(&'a str, PrefixOperator, Expression);

    let inputs = [
        TestCase("!5;", PrefixOperator::Bang, Expression::Int(5)),
        TestCase("-15;", PrefixOperator::Minus, Expression::Int(15)),
        TestCase("!!false;", PrefixOperator::Bang, Expression::PrefixExpression {
            operator: PrefixOperator::Bang,
            right: Box::new(Expression::Bool(false)),
        })
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
        TestCase("5 * 5;", Box::new(Expression::Int(5)), InfixOperator::Multiply, Box::new(Expression::Int(5))),
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
            operator: InfixOperator::Multiply,
            right: Box::new(Ident("b".into()))
        }),

        TestCase("a * b + c", InfixExpression {
            left: Box::new(InfixExpression {
                left: Box::new(Ident("a".into())),
                operator: InfixOperator::Multiply,
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
                operator: InfixOperator::Multiply,
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
        }),

        TestCase("3 < 5 == true", InfixExpression {
            left: Box::new(InfixExpression {
                left: Box::new(Int(3)),
                operator: InfixOperator::LessThan,
                right: Box::new(Int(5))
            }),
            operator: InfixOperator::Equals,
            right: Box::new(Expression::Bool(true))
        }),

        TestCase("(5 + 6) * 2", InfixExpression {
            left: Box::new(InfixExpression {
                left: Box::new(Int(5)),
                operator: InfixOperator::Plus,
                right: Box::new(Int(6))
            }),
            operator: InfixOperator::Multiply,
            right: Box::new(Int(2))
        }),
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

#[test]
fn boolean_expression() {
    let input = "true; false";

    let program = parse_then_check_errors_and_length(input, 2);

    dbg!(&program.statements);

    assert_eq!(program.statements, vec![
        Statement::ExpressionStatement { value: Box::new(Expression::Bool(true)) },
        Statement::ExpressionStatement { value: Box::new(Expression::Bool(false)) },
    ]);
}

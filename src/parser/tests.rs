use super::*;

impl Program {
    fn first_statement(&self) -> &Statement {
        return self.statements.first().unwrap();
    }
}

fn parse_then_check_errors_and_length(input: &str, expected_length: usize) -> Program {
    let mut parser = Parser::new(input);

    let program = parser.parse_program();

    check_parse_errors(&program);

    assert_eq!(program.statements.len(), expected_length);

    return program;
}

fn check_parse_errors(program: &Program) {
    if !program.errors.is_empty() {
        panic!("Got {} parsing error(s):\n{:?}\nSucceeded in parsing: {:?}",
            program.errors.len(),
            program.errors,
            program.statements
        );
    }
}

fn check_and_destruct_expression_statement(statement: &Statement) -> &Expression {
    if let Statement::ExpressionStatement { value } = statement {
        return value.as_ref();
    } else {
        panic!("Statement not an Expression statement {:?}", statement);
    }
}

#[test]
fn parse_let_statements_literal_value() {
    let input = "let x = 5;
    let y = 10;
    let foobar = 838383;";

    let program = parse_then_check_errors_and_length(input, 3);

    let expected_names = vec!["x", "y", "foobar"];

    let expected_values = vec![
        Expression::Int(5),
        Expression::Int(10),
        Expression::Int(838383),
    ];

    program.statements.iter()
        .zip(expected_names)
        .zip(expected_values)
        .for_each(|((statement, expected_name), expected_value)| {
            if let Statement::Let{ name, value } = statement {
                assert_eq!(name, expected_name);

                assert_eq!(**value, expected_value);
            } else {
                panic!("Testing statement {:?} that isn't a Let statement", statement);
            }
    });
}

#[test]
fn parse_let_yields_correct_errors() {
    let input = "let x 5;
    let = 10;
    let 838383;";

    let mut parser = Parser::new(input);

    let program = parser.parse_program();

    assert_eq!(program.errors.len(), 3);

    assert_eq!(program.errors, vec![
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

    let expected_values = vec![
        Expression::Int(5),
        Expression::Int(10),
        Expression::Int(993322),
    ];

    program.statements.iter()
        .zip(expected_values)
        .for_each(|(statement, expected_value)| {
            if let Statement::Return { value } = statement {
                assert_eq!(**value, expected_value);
            } else {
                panic!("Testing statement {:?} that isn't a Return statement", statement);
            }
        });
}

#[test]
fn identifier_expression() {
    let input = "foobar;";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    assert_eq!(*value, Expression::Ident("foobar".into()));
}

#[test]
fn integer_expression() {
    let input = "5;";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    assert_eq!(*value, Expression::Int(5));
}

#[test]
fn string_expression() {
    let input = "'hello world'";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    assert_eq!(*value, Expression::Str("hello world".into()));
}

#[test]
fn prefix_operators() {
    struct TestCase<'a>(&'a str, PrefixOperator, Expression);

    let inputs = [
        TestCase("!5;", PrefixOperator::Bang, Expression::Int(5)),
        TestCase("-15;", PrefixOperator::Minus, Expression::Int(15)),
        TestCase("!!false;", PrefixOperator::Bang, Expression::PrefixExpression {
            operator: PrefixOperator::Bang,
            right: Expression::Bool(false).into(),
        })
    ];

    inputs.iter().for_each(|test_case| {
        let program = parse_then_check_errors_and_length(test_case.0, 1);

        let statement = program.first_statement();

        let value = check_and_destruct_expression_statement(statement);

        if let Expression::PrefixExpression { operator, right } = value {
            assert_eq!(*operator, test_case.1);
            assert_eq!(**right, test_case.2);
        } else {
            panic!("Testing expression {:?} that isn't a Prefix expression", value);
        }
    })
}

#[test]
fn infix_operators_integer_literals() {
    struct TestCase<'a>(&'a str, Box<Expression>, InfixOperator, Box<Expression>);

    let inputs = [
        TestCase("5 + 5;", Expression::Int(5).into(), InfixOperator::Plus, Expression::Int(5).into()),
        TestCase("5 - 5;", Expression::Int(5).into(), InfixOperator::Minus, Expression::Int(5).into()),
        TestCase("5 * 5;", Expression::Int(5).into(), InfixOperator::Multiply, Expression::Int(5).into()),
        TestCase("5 / 5;", Expression::Int(5).into(), InfixOperator::Divide, Expression::Int(5).into()),
        TestCase("5 > 5;", Expression::Int(5).into(), InfixOperator::GreaterThan, Expression::Int(5).into()),
        TestCase("5 < 5;", Expression::Int(5).into(), InfixOperator::LessThan, Expression::Int(5).into()),
        TestCase("5 == 5;", Expression::Int(5).into(), InfixOperator::Equals, Expression::Int(5).into()),
        TestCase("5 != 5;", Expression::Int(5).into(), InfixOperator::NotEquals, Expression::Int(5).into()),
    ];

    inputs.iter().for_each(|test_case| {
        let program = parse_then_check_errors_and_length(test_case.0, 1);

        let statement = program.first_statement();

        let value = check_and_destruct_expression_statement(statement);

        if let Expression::InfixExpression { left, operator, right } = value {
            assert_eq!(left, &test_case.1);
            assert_eq!(operator, &test_case.2);
            assert_eq!(right, &test_case.3);
        } else {
            panic!("Testing expression {:?} that isn't an Infix expression", value);
        }
    })
}

#[test]
fn infix_operators_correct_precedence() {
    use Expression::*;

    struct TestCase<'a>(&'a str, Expression);

    [
        TestCase("-a * b", InfixExpression {
            left: PrefixExpression {
                operator: PrefixOperator::Minus,
                right: Ident("a".into()).into(),
            }.into(),
            operator: InfixOperator::Multiply,
            right: Ident("b".into()).into()
        }),

        TestCase("a * b + c", InfixExpression {
            left: InfixExpression {
                left: Ident("a".into()).into(),
                operator: InfixOperator::Multiply,
                right: Ident("b".into()).into()
            }.into(),
            operator: InfixOperator::Plus,
            right: Ident("c".into()).into()
        }),

        TestCase("a + b * c", InfixExpression {
            left: Ident("a".into()).into(),
            operator: InfixOperator::Plus,
            right: InfixExpression {
                left: Ident("b".into()).into(),
                operator: InfixOperator::Multiply,
                right: Ident("c".into()).into()
            }.into()
        }),

        TestCase("add(a + b * c) + d", InfixExpression {
            left: CallExpression {
                function: Expression::Ident("add".into()).into(),
                arguments: vec![InfixExpression {
                    left: Ident("a".into()).into(),
                    operator: InfixOperator::Plus,
                    right: InfixExpression {
                        left: Ident("b".into()).into(),
                        operator: InfixOperator::Multiply,
                        right: Ident("c".into()).into(),
                    }.into(),
                }]
            }.into(),
            operator: InfixOperator::Plus,
            right: Ident("d".into()).into()
        }),

        TestCase("5 < 4 != 3 > 4", InfixExpression {
            left: InfixExpression {
                left: Int(5).into(),
                operator: InfixOperator::LessThan,
                right: Int(4).into()
            }.into(),
            operator: InfixOperator::NotEquals,
            right: InfixExpression {
                left: Int(3).into(),
                operator: InfixOperator::GreaterThan,
                right: Int(4).into()
            }.into()
        }),

        TestCase("3 < 5 == true", InfixExpression {
            left: InfixExpression {
                left: Int(3).into(),
                operator: InfixOperator::LessThan,
                right: Int(5).into(),
            }.into(),
            operator: InfixOperator::Equals,
            right: Bool(true).into()
        }),

        TestCase("(5 + 6) * 2", InfixExpression {
            left: InfixExpression {
                left: Int(5).into(),
                operator: InfixOperator::Plus,
                right: Int(6).into(),
            }.into(),
            operator: InfixOperator::Multiply,
            right: Int(2).into(),
        }),
    ].iter().for_each(|test_case| {
        let program = parse_then_check_errors_and_length(test_case.0, 1);

        let statement = program.first_statement();

        let value = check_and_destruct_expression_statement(statement);

        assert_eq!(*value, test_case.1);
    });
}

#[test]
fn boolean_expression() {
    let input = "true; false";

    let program = parse_then_check_errors_and_length(input, 2);

    assert_eq!(program.statements, vec![
        Statement::ExpressionStatement { value: Expression::Bool(true).into() },
        Statement::ExpressionStatement { value: Expression::Bool(false).into() },
    ]);
}

#[test]
fn if_expression() {
    use Expression::*;

    let input = "if (x < y) { x }";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    if let If { condition, consequence, alternative } = value {
        assert_eq!(**condition, InfixExpression {
            left: Ident("x".into()).into(),
            operator: InfixOperator::LessThan,
            right: Ident("y".into()).into(),
        });

        if let Expression::Block(statements) = consequence.as_ref() {
            assert_eq!(statements.len(), 1);
            assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Ident("x".into()).into() });

            assert_eq!(*alternative, None);
        } else {
            panic!("Consequence not a block expression");
        }
    } else {
        panic!("Expression not an If expression");
    }
}

#[test]
fn nested_if_expression() {
    let input = "if (true) { if (false) { 1 }}";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    assert_eq!(
        *value,
        Expression::If {
            condition: Expression::Bool(true).into(),
            consequence: Expression::Block(vec![Statement::ExpressionStatement { value: Expression::If {
                condition: Expression::Bool(false).into(),
                consequence: Expression::Block(vec![
                        Statement::ExpressionStatement { value : Expression::Int(1).into() }
                    ]).into(),
                alternative: None
            }.into()}]).into(),
            alternative: None
        }
    );
}

#[test]
fn if_else_expression() {
    use Expression::*;

    let input = "if (x < y) { x } else { y }";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    if let If { condition, consequence, alternative } = value {
        assert_eq!(**condition, InfixExpression {
            left: Ident("x".into()).into(),
            operator: InfixOperator::LessThan,
            right: Ident("y".into()).into(),
        });

        if let Expression::Block(statements) = consequence.as_ref() {
            assert_eq!(statements.len(), 1);
            assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Ident("x".into()).into() });
        } else {
            panic!("Consequence not a block expression");
        }

        if let Some(alternative) = alternative {
            if let Expression::Block(statements) = alternative.as_ref() {
                assert_eq!(statements.len(), 1);
                assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Ident("y".into()).into() });
            } else {
                panic!("Alternative not a BlockStatement");
            }
        } else {
            panic!("Consequence not a block expression");
        }
    } else {
        panic!("Expression not an If expression");
    }
}

#[test]
fn function_parameters() {
    struct TestCase<'a>(&'a str, Vec<String>);

    let inputs = vec![
        TestCase("fn() {}", vec![]),
        TestCase("fn(x) {}", vec!["x".into()]),
        TestCase("fn ( x , y , z ) { } ;", vec!["x".into(), "y".into(), "z".into()])
    ];

    inputs.iter().for_each(|test_case| {
        let program = parse_then_check_errors_and_length(test_case.0, 1);

        let statement = program.first_statement();

        let value = check_and_destruct_expression_statement(statement);

        if let Expression::Function { parameters, body } = value {
            assert_eq!(*parameters, test_case.1);
            assert_eq!(*body, Expression::Block(vec![]).into());
        } else {
            panic!("Expression not a Function expression")
        }
    })
}

#[test]
fn function_literal() {
    use Expression::*;

    let input = "fn(x, y) { x + y; }";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    if let Function { parameters, body } = value {
        assert_eq!(*parameters, vec!["x".to_string(), "y".to_string()]);
        assert_eq!(*body, Expression::Block(vec![
                Statement::ExpressionStatement { value: Expression::InfixExpression {
                    left: Ident("x".into()).into(),
                    operator: InfixOperator::Plus,
                    right: Ident("y".into()).into(),
                }.into()}
            ]).into());
    } else {
        panic!("Expression not a Function expression");
    }
}

#[test]
fn call_expressions() {
    use Expression::*;

    let input = "add(1, 2 * 3, 4 + 5)";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    if let CallExpression { function, arguments } = value {
        if let Ident(name) = function.as_ref() {
            assert_eq!(*name, "add".to_string());
        } else {
            panic!("Function expression not an identifier");
        }

        assert_eq!(*arguments, vec![
            Int(1),
            InfixExpression { left: Int(2).into(), operator: InfixOperator::Multiply, right: Int(3).into() },
            InfixExpression { left: Int(4).into(), operator: InfixOperator::Plus, right: Int(5).into() }
        ])
    } else {
        panic!("Expression not a Call expression");
    }
}

#[test]
fn anonymous_call_expression() {
    use Expression::*;

    let input = "fn(x) { x; }(5)";

    let program = parse_then_check_errors_and_length(input, 1);

    let statement = program.first_statement();

    let value = check_and_destruct_expression_statement(statement);

    if let CallExpression { function, arguments } = value {
        if let Function { parameters, body } = function.as_ref() {
            assert_eq!(*parameters, vec!["x".to_string()]);

            assert_eq!(**body, Block(vec![
                Statement::ExpressionStatement { value: Ident("x".into()).into() }
            ]));
        } else {
            panic!("Expression wasn't a Function expression");
        }

        assert_eq!(*arguments, vec![
            Int(5)
        ]);
    } else {
        panic!("Expression wasn't a Call expression");
    }
}

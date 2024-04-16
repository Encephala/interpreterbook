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

        TestCase("add(a + b * c) + d", InfixExpression {
            left: Box::new(CallExpression {
                function: Box::new(Expression::Ident("add".into())),
                arguments: vec![InfixExpression {
                    left: Box::new(Ident("a".into())),
                    operator: InfixOperator::Plus,
                    right: Box::new(InfixExpression {
                        left: Box::new(Ident("b".into())),
                        operator: InfixOperator::Multiply,
                        right: Box::new(Ident("c".into()))
                    })
                }]
            }),
            operator: InfixOperator::Plus,
            right: Box::new(Ident("d".into()))
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
            right: Box::new(Bool(true))
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
        Statement::ExpressionStatement { value: Box::new(Expression::Bool(true)) },
        Statement::ExpressionStatement { value: Box::new(Expression::Bool(false)) },
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
            left: Box::new(Ident("x".into())),
            operator: InfixOperator::LessThan,
            right: Box::new(Ident("y".into()))
        });

        if let BlockStatement { statements } = consequence {
            assert_eq!(statements.len(), 1);
            assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Box::new(Ident("x".into())) });
        } else {
            panic!("Consequence not a BlockStatement");
        }

        assert_eq!(*alternative, None);
    } else {
        panic!("Expression not an If expression");
    }
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
            left: Box::new(Ident("x".into())),
            operator: InfixOperator::LessThan,
            right: Box::new(Ident("y".into()))
        });

        let BlockStatement { statements } = consequence;

        assert_eq!(statements.len(), 1);
        assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Box::new(Ident("x".into())) });

        if let Some(BlockStatement { statements }) = alternative {
            assert_eq!(statements.len(), 1);
            assert_eq!(*statements.first().unwrap(), Statement::ExpressionStatement { value: Box::new(Ident("y".into())) });
        } else {
            panic!("Alternative not a BlockStatement");
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
            assert_eq!(*body, BlockStatement { statements: vec![] });
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
        assert_eq!(*body, BlockStatement {
            statements: vec![
                Statement::ExpressionStatement { value: Box::new(Expression::InfixExpression {
                    left: Box::new(Ident("x".into())),
                    operator: InfixOperator::Plus,
                    right: Box::new(Ident("y".into()))
                })}
            ]
            });
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
            InfixExpression { left: Box::new(Int(2)), operator: InfixOperator::Multiply, right: Box::new(Int(3)) },
            InfixExpression { left: Box::new(Int(4)), operator: InfixOperator::Plus, right: Box::new(Int(5)) }
        ])
    } else {
        panic!("Expression not a Call expression");
    }
}

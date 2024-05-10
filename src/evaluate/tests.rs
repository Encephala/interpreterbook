use super::super::parser::Parser;
use super::{AstNode, Object, Statement, Expression, InfixOperator, ExecutionEnvironment};

macro_rules! test_case {
    ($type:ty) => {
        struct TestCase<'a>(&'a str, $type);
    };
}

fn evaluate(input: &str) -> Result<Object, String> {
    let program = Parser::new(input).parse_program();

    let mut environment = ExecutionEnvironment::new();

    if !program.errors.is_empty() {
        panic!("Got errors while executing program: {:?}", program.errors);
    }

    return program.evaluate(&mut environment);
}

#[test]
fn integer_expression() {
    test_case!(isize);

    let inputs = [
        TestCase("5", 5),
        TestCase("10", 10),
        TestCase("-5", -5),
        TestCase("--10", 10),
        TestCase("1 * 2 + 3 + 4", 9),
        TestCase("2 * (5 + 10)", 30),
    ];

    inputs.iter().for_each(|test_case| {

        let result = evaluate(test_case.0).unwrap();

        if let Object::Int(value) = result {
            assert_eq!(value, test_case.1);
        } else {
            panic!("Evaluation result wasn't an Integer object");
        }
    })
}

#[test]
fn string_expression() {
    let input = "'hello world';";

    let result = evaluate(input).unwrap();

    assert_eq!(result, Object::Str("hello world".into()));
}

#[test]
fn boolean_expressions() {
    test_case!(bool);

    let inputs = [
        TestCase("true", true),
        TestCase("false", false),
        TestCase("1 < 2", true),
        TestCase("2 != 2", false),
        TestCase("2 == 3 == false", true),
    ];

    inputs.iter().for_each(|test_case| {

        let result = evaluate(test_case.0).unwrap();

        if let Object::Bool(value) = result {
            assert_eq!(value, test_case.1);
        } else {
            panic!("Evaluation result wasn't an Bool object");
        }
    })
}

#[test]
fn bang_operator() {
    test_case!(bool);

    let inputs = [
        TestCase("!true", false),
        TestCase("!5", false),
        TestCase("!!true", true),
        TestCase("!!5", true)
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        if let Object::Bool(value) = result {
            assert_eq!(value, test_case.1);
        } else {
            panic!("Evaluation result wasn't a Bool object")
        }
    });
}

#[test]
fn prefix_operator_minus_error_for_incompatible_types() {
    [
        "-true",
        "-(5 > 3)"
    ].iter().for_each(|input| {
        let result = evaluate(input);

        if result.is_ok() {
            panic!("Didn't get error as expected, got {:?}", result);
        }
    });
}

#[test]
fn infix_operator_integer_error_for_incompatible_types() {
    [
        "5 + true",
        "true / false",
        "true < 10"
    ].iter().for_each(|input| {
        assert!(evaluate(input).is_err());
    });
}

#[test]
fn string_operators() {
    test_case!(Object);

    let inputs = [
        TestCase("'hello' + 'world'", Object::Str("helloworld".into())),
        TestCase("'hello world' - 'world'", Object::Str("hello ".into())),
        TestCase("'hello' != 'world'", Object::Bool(true)),
        TestCase("'hello' == 'hello'", Object::Bool(true)),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

#[test]
fn conditional_expression() {
    use Object::*;

    test_case!(Object);

    let inputs = [
        TestCase("if (true) { 10 }", Int(10)),
        TestCase("if (1) { 10 }", Int(10)),
        TestCase("if (1 > 2) { 10 } else { 20 }", Int(20)),
        TestCase("if (0) { 1 } else { 2 }", Int(2)),
        TestCase("if (0) { 10 }", None),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

#[test]
fn empty_program_returns_none() {
    let input = ";";

    let result = evaluate(input).unwrap();

    assert_eq!(result, Object::None);
}

#[test]
fn empty_block_statement_returns_none() {
    let input = "{};";

    let result = evaluate(input).unwrap();

    assert_eq!(result, Object::None)
}

#[test]
fn return_statement() {
    test_case!(isize);

    let inputs = [
        TestCase("{return 10}", 10),
        TestCase("{return 2 * 5; 9}", 10),
        TestCase("{1; return 2; 3}", 2),
        TestCase("1; return 2; 3", 3), // No early returns out of the whole program
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

#[test]
fn nested_returns() {
    let inputs = [
        "{ { return 10 } return 1 };",
        "{ { return 10 } return 1 }; 5;"
    ];

    let expected_results = [10, 5];

    inputs.iter().zip(expected_results).for_each(|(input, result)| {
        assert_eq!(evaluate(input).unwrap(), Object::Int(result));
    });
}

#[test]
fn let_statements() {
    test_case!(isize);

    let inputs = [
        TestCase("let a = 5; a", 5),
        TestCase("let a = 5; let b = a; a * b;", 25),
        TestCase("let a = 1; let b = 2; let c = a + b + 3; c", 6)
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

#[test]
fn unbound_variable_error() {
    let input = "foobar;";

    let result = evaluate(input);

    dbg!(&result);
    assert!(result.is_err());
}

#[test]
fn function_literal() {
    let input = "fn(x) { x + 2; }";

    let result = evaluate(input).unwrap();

    if let Object::Function { parameters, body , environment} = result {
        assert_eq!(parameters, vec!["x".to_string()]);

        assert_eq!(*body, Expression::Block(vec![
            Statement::ExpressionStatement { value: Expression::InfixExpression {
                left: Expression::Ident("x".into()).into(),
                operator: InfixOperator::Add,
                right: Expression::Int(2).into()
            }.into()}
        ]));

        assert_eq!(environment, ExecutionEnvironment::new());
    }
}

#[test]
fn function_call() {
    test_case!(isize);

    let inputs = [
        TestCase("let identity = fn(x) { x; }; identity(5);", 5),
        TestCase("let identity = fn(x) { return x; }; identity(5);", 5),
        TestCase("let double = fn(x) { x * 2; }; double(5);", 10),
        TestCase("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        TestCase("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        TestCase("fn(x) { x; }(5)", 5),
        TestCase("let newAdder = fn(x) { fn(y) { x + y } };
        let addTwo = newAdder(2);
        addTwo(3)", 5),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

#[test]
fn function_call_on_nonfunction_error() {
    let input = "1(2);";

    let result = evaluate(input);

    assert_eq!(result, Err("Int(1) isn't a function".into()));
}

// Return statements shouldn't bubble up out of function calls
#[test]
fn function_return_bubbling() {
    let input = "let double = fn(x) { return x * 2 }; { double(1) + 1 }";

    let result = evaluate(input).unwrap();

    assert_eq!(result, Object::Int(3));
}

// #[test]
// fn recursion() {
//     let input = "let factorial = fn(x) {
//         if (x < 3) { return x }
//         else { return x * factorial(x - 1) }
//     };

//     factorial(5)";

//     let result = evaluate(input).unwrap();

//     assert_eq!(result, Object::Int(120));
// }

#[test]
fn builtin_len() {
    test_case!(isize);

    let inputs = [
        TestCase("len('')", 0),
        TestCase("len('abc')", 3),
        TestCase("len('a b c')", 5),
        TestCase("len([1, 2])", 2),
        TestCase("len([])", 0),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

#[test]
fn array() {
    use Object::*;

    test_case!(Object);

    let inputs = [
        TestCase("[1, 2];", Array(vec![
            Int(1), Int(2)
        ])),
        TestCase("['hey', 2, fn(x) { return x }];", Array(vec![
            Str("hey".into()),
            Int(2),
            Function {
                parameters: vec!["x".into()],
                body: Expression::Block(vec![
                    Statement::Return{ value: Expression::Ident("x".into()).into() }
                ]).into(),
                environment: ExecutionEnvironment::new()
            }
        ])),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    });
}

#[test]
fn indexing_array() {
    test_case!(isize);

    let inputs = [
        TestCase("[1, 2, 3, 4][2];", 3),
        TestCase("fn(){ [1, 2] }()[1];", 2),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    });
}

#[test]
fn indexing_array_out_of_bounds_error() {
    let input = "[][0]";

    let result = evaluate(input);

    assert_eq!(result, Err("Index 0 out of bounds".into()));
}

#[test]
fn builtin_first() {
    test_case!(Object);

    let inputs = [
        TestCase("first([1, 2])", Object::Int(1)),
        TestCase("first(['hey', 'bye'])", Object::Str("hey".into())),
        TestCase("first([1, 2])", Object::Int(1)),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

#[test]
fn builtin_rest() {
    test_case!(Object);

    let inputs = [
        TestCase("rest([1, 2])", Object::Array(vec![Object::Int(2)])),
        TestCase("rest(['hey', 'bye'])", Object::Array(vec![Object::Str("bye".into())])),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

#[test]
fn builtin_push() {
    test_case!(Object);

    let inputs = [
        TestCase("push([1, 2], 3)", Object::Array(vec![
            Object::Int(1),
            Object::Int(2),
            Object::Int(3)]
        )),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

#[test]
fn hash_literal() {
    let input = "
    let two = 'two';
    #
        'one': 10 - 9,
        two: 1 + 1,
        'thr' + 'ee': 6 / 2,
        4: 4,
        true: 5,
        false: 6,
    $;";

    let result = evaluate(input).unwrap();

    struct TestCase(Object, isize);

    let tests = [
        TestCase(Object::Str("one".into()), 1),
        TestCase(Object::Str("two".into()), 2),
        TestCase(Object::Str("three".into()), 3),
        TestCase(Object::Int(4), 4),
        TestCase(Object::Bool(true), 5),
        TestCase(Object::Bool(false), 6),
    ];

    if let Object::Hash(value) = result {
        tests.iter().for_each(|test_case| {
            assert_eq!(value.get(&test_case.0).unwrap(), &Object::Int(test_case.1));
        })
    } else {
        panic!("Result wasn't a Hash object")
    }
}

#[test]
fn hash_literal_indexing() {
    test_case!(Object);

    let inputs = [
        TestCase("#'foo': 5$['foo']", Object::Int(5)),
        TestCase("#5: 5$[5]", Object::Int(5)),
        TestCase("#true: 5$[true]", Object::Int(5)),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    });
}

#[test]
fn hash_literal_errors() {
    test_case!(&'a str);

    let inputs = [
        TestCase("#'foo': 5$['bar']", "Str(\"bar\") not found in map"),
        TestCase("#'foo': 5$[true]", "Bool(true) not found in map"),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0);

        assert_eq!(result, Err(test_case.1.into()));
    });
}

#[test]
fn quote_basic_functionality() {
    test_case!(Expression);

    let inputs = [
        TestCase("quote(5)", Expression::Int(5)),
        TestCase("quote('5')", Expression::Str("5".into())),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        if let Object::Quote(value) = result {
            assert_eq!(value, test_case.1);
        } else {
            panic!("Resulting object wasn't a Quote object");
        }
    });
}

#[test]
fn unquote_basic_functionality() {
    test_case!(Object);

    let inputs = [
        TestCase("quote(unquote(4))", Object::Quote(Expression::Int(4))),
        TestCase("quote(unquote(4 + 4))", Object::Quote(Expression::Int(8))),
        TestCase("quote(8 + unquote(4 + 4))", Object::Quote(Expression::InfixExpression {
            left: Expression::Int(8).into(),
            operator: InfixOperator::Add,
            right: Expression::Int(8).into()
        })),
        TestCase("quote(unquote(4 + 4) + 8)", Object::Quote(Expression::InfixExpression {
            left: Expression::Int(8).into(),
            operator: InfixOperator::Add,
            right: Expression::Int(8).into()
        })),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    });
}

#[test]
fn quote_inside_unquote() {
    use Expression::*;

    test_case!(Object);

    let inputs = [
        TestCase("quote(unquote(quote(4 + 5)) + 6)", Object::Quote(
            InfixExpression {
                left: InfixExpression {
                    left: Int(4).into(),
                    operator: InfixOperator::Add,
                    right: Int(5).into(),
                }.into(),
                operator: InfixOperator::Add,
                right: Int(6).into(),
            }
        )),
        TestCase("
        let quoted_infix = quote(4 + 5);
        quote(unquote(6 + 7) + unquote(quoted_infix));
        ",
        Object::Quote(
            InfixExpression {
                left: Int(13).into(),
                operator: InfixOperator::Add,
                right: InfixExpression {
                    left: Int(4).into(),
                    operator: InfixOperator::Add,
                    right: Int(5).into(),
                }.into()
            }
        )),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, test_case.1);
    })
}

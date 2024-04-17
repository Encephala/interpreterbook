use super::super::parser::Parser;
use super::{AstNode, Object, Statement, Expression, InfixOperator, ExecutionEnvironment};

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
    struct TestCase<'a>(&'a str, isize);

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
fn boolean_expressions() {
    struct TestCase<'a>(&'a str, bool);

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
    struct TestCase<'a>(&'a str, bool);

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

        if !result.is_err() {
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
fn conditional_expression() {
    use Object::*;

    struct TestCase<'a>(&'a str, Object);

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
    struct TestCase<'a>(&'a str, isize);

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
    struct TestCase<'a>(&'a str, isize);

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
                operator: InfixOperator::Plus,
                right: Expression::Int(2).into()
            }.into()}
        ]));

        assert_eq!(environment, ExecutionEnvironment::new());
    }
}

#[test]
fn function_call() {
    struct TestCase<'a>(&'a str, isize);

    let inputs = [
        TestCase("let identity = fn(x) { x; }; identity(5);", 5),
        TestCase("let identity = fn(x) { return x; }; identity(5);", 5),
        TestCase("let double = fn(x) { x * 2; }; double(5);", 10),
        TestCase("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        TestCase("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        TestCase("fn(x) { x; }(5)", 5),
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

// Return statements shouldn't bubble up out of function calls
#[test]
fn function_return_bubbling() {
    let input = "let double = fn(x) { return x * 2 }; { double(1) + 1 }";

    let result = evaluate(input).unwrap();

    assert_eq!(result, Object::Int(3));
}

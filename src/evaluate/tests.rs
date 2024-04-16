use super::super::parser::{Parser, Program};

use super::{AstNode, Object, Expression, Statement};

fn evaluate(input: &str) -> Object {
    let program = Parser::new(input).parse_program();

    assert!(program.errors.is_empty());

    let result = program.eval();

    if result.is_err() {
        panic!("Error(s) while evaluating: {:?}", result);
    }

    return result.unwrap();
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

        let result = evaluate(test_case.0);

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

        let result = evaluate(test_case.0);

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
        let result = evaluate(test_case.0);

        if let Object::Bool(value) = result {
            assert_eq!(value, test_case.1);
        } else {
            panic!("Evaluation result wasn't a Bool object")
        }
    })
}

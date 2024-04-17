use super::super::parser::Parser;

use super::{AstNode, Object};

fn evaluate(input: &str) -> Result<Object, String> {
    let program = Parser::new(input).parse_program();

    dbg!(&program.errors);

    assert!(program.errors.is_empty());

    return program.evaluate();
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
        assert!(evaluate(input).is_err());
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
        TestCase("return 10", 10),
        TestCase("return 2 * 5; 9", 10),
        TestCase("1; return 2; 3", 2)
    ];

    inputs.iter().for_each(|test_case| {
        let result = evaluate(test_case.0).unwrap();

        assert_eq!(result, Object::Int(test_case.1));
    })
}

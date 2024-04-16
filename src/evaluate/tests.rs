use super::super::parser::{Parser, Program};

use super::{AstNode, Object, Expression, Statement};

fn evaluate(input: &str) -> Object {
    let program = Parser::new(input).parse_program();

    assert!(program.errors.is_empty());

    return program.eval();
}

#[test]
fn evaluate_integer_expression() {
    struct TestCase<'a>(&'a str, usize);

    let inputs = [
        TestCase("5", 5),
        TestCase("10", 10),
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

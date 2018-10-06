use ast::*;
use rules::program;

#[test]
fn test() {
    let input = r"package kek;";
    let expect = Program {
        package: Identifier("kek".to_string()),
    };
    let res = program(input);
    assert_eq!(Ok(("", expect)), res);
}
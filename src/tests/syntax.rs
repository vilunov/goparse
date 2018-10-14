use lexer::*;
use syntax::*;
use types::*;

fn lexical(inp: &str) -> (Vec<Token>, IdentifierStorage, StringLiteralsStorage) {
    Lexer::new().tokenize(inp).unwrap().collect()
}

#[test]
fn test_expr() {
    let (tokens, _, _) = lexical("int(1)");
    let (rem, expr) =  expression(&tokens[..]).unwrap();
    assert_eq!(rem, &[]);
    println!("{:?}", expr);
}
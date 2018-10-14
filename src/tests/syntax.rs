use lexer::*;
use syntax::*;
use types::*;

fn lexical(inp: &str) -> (Vec<Token>, IdentifierStorage, StringLiteralsStorage) {
    Lexer::new().tokenize(inp).unwrap().collect()
}

#[test]
fn test_expr() {
    let (tokens, _, _) = lexical("1(1);");
    let (rem, expr) =  expression(&tokens[..]).unwrap();
    assert_eq!(rem, &[Token::Punc(Punctuation::Semicolon)]);
    println!("{:?}", expr);
}

fn test_signature() {
    let (tokens, _, _) = lexical("(a int, b ...int, c, d chan<- *[][1]int, int.int) bool;");
    println!("==================\n{:?}", tokens);
    let (rem, expr) =  signature(&tokens[..]).unwrap();
    assert_eq!(rem, &[Token::Punc(Punctuation::Semicolon)]);
    println!("{:?}", expr);
}

#[test]
fn test_parameters_spec() {
    let (tokens, _, _) = lexical("int.int;");
    println!("==================\n{:?}", tokens);
    let (rem, expr) =  parameters_spec(&tokens[..]).unwrap();
    assert_eq!(rem, &[Token::Punc(Punctuation::Semicolon)]);
    println!("{:?}", expr);
}

#[test]
fn test_parameters_ty() {
    let (tokens, _, _) = lexical("int.int;");
    println!("==================\n{:?}", tokens);
    let (rem, expr) =  ty(&tokens[..]).unwrap();
    assert_eq!(rem, &[Token::Punc(Punctuation::Semicolon)]);
    println!("Ty: {:?}", expr);
}
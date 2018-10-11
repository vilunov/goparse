pub mod ops;
pub mod literals;
pub mod idents;
pub mod integration;

#[test]
fn package_clause() {
    use ast::Identifier;
    use rules::package_clause;
    let out = package_clause("package a;");
    assert_eq!(Ok((";", Identifier("a".to_string()))), out);
}

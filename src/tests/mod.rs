pub mod idents;
pub mod integration;
pub mod literals;
pub mod ops;

#[test]
fn package_clause() {
    use ast::Identifier;
    use rules::package_clause;
    let out = package_clause("package a;");
    assert_eq!(Ok((";", Identifier("a".to_string()))), out);
}

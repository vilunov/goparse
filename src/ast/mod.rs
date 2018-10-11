pub use types::Keyword;
pub mod expr;
pub mod literals;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Program {
    pub package: Identifier,
}

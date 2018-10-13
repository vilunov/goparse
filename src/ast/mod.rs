pub use types::Keyword;
pub mod expr;
pub mod literals;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Program {
    pub package: usize,
    pub imports: Vec<ImportSpec>,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ImportSpecPackage {
    Dot,
    Package(usize),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct ImportSpec {
    pub package: Option<ImportSpecPackage>,
    pub path: String,
}

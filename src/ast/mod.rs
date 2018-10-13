pub use types::Keyword;
pub mod expr;
pub mod literals;

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Program {
    pub package: usize,
    pub imports: Vec<ImportSpec>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum ImportSpecPackage {
    Dot,
    Package(usize),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ImportSpec {
    pub package: Option<ImportSpecPackage>,
    pub path: usize,
}

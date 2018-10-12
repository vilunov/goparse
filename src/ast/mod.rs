pub use types::Keyword;
pub mod expr;
pub mod literals;


#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Program {
    pub package: usize,
    //pub imports: Vec<ImportSpec>,
}

pub enum ImportSpecPackage {
    None,
    Dot,
    Package(usize),
}

pub struct ImportSpec {
    package: ImportSpecPackage,
    path: String,
}

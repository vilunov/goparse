pub use types::Keyword;

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Program {
    pub package: usize,
    pub imports: Vec<ImportSpec>,
    pub decls: Vec<TopLevelDecl>,
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

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Type;

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Expression;

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ConstSpec {
    pub identifiers: Vec<usize>,
    pub right_side: Option<(Type, Vec<Expression>)>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum TopLevelDecl {
    Consts(Vec<ConstSpec>),
}

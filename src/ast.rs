pub use types::{Literal, Keyword};

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
pub enum Expression {
    Primary(PrimaryExpr),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ConstSpecRightSide {
    pub ty: Option<Ty>,
    pub expressions: Vec<Expression>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ConstSpec {
    pub identifiers: Vec<usize>,
    pub right_side: Option<ConstSpecRightSide>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum TopLevelDecl {
    Consts(Vec<ConstSpec>),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum FullIdentifier {
    Qualified {
        package: usize,
        identifier: usize,
    },
    Unqualified(usize),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum Ty {
    TypeName(FullIdentifier),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum PrimaryExpr {
    Literal(Literal),
    Identifier(FullIdentifier),
    Parenthesis(Box<Expression>),
}

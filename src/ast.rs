pub use types::{self, Literal, Keyword};

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
pub struct UnaryExpression {
    pub ops: Vec<UnaryOp>,
    pub primary: PrimaryExpr,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum BinaryOp {
    BinOp(types::BinaryOp),
    DoubleAnd, DoubleOr,
    Equals, Le, Lt, Ge, Gt, Ne,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct OpExpression {
    pub op: BinaryOp,
    pub expression: UnaryExpression,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Expression {
    pub head: UnaryExpression,
    pub tail: Vec<OpExpression>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ConstSpecRightSide {
    pub ty: Option<Ty>,
    pub expressions: Vec<Expression>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ConstSpec {
    pub identifiers: Vec<usize>,
    #[serde(flatten)]
    pub right_side: Option<ConstSpecRightSide>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct TypeSpec {
    pub identifier: usize,
    pub ty: Ty
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct VarSpec {
    pub identifiers: Vec<usize>,
    pub right_side: Option<VarRightSide>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum VarRightSide {
    WithType {
        ty: Ty,
        expression: Option<Vec<Expression>>
    },
    WithoutType(Vec<Expression>)
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum TopLevelDecl {
    Consts(Vec<ConstSpec>),
    Types(Vec<TypeSpec>),
    Vars(Vec<VarSpec>),
    Function(FuncDecl),
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
pub enum MethodSpec {
    Interface(FullIdentifier),
    Method {
        name: usize,
        signature: Signature,
    },
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum Ty {
    TypeName(FullIdentifier),
    Array {
        length: Option<Box<Expression>>,
        elements: Box<Ty>,
    },
    Map {
        keys: Box<Ty>,
        elements: Box<Ty>
    },
    ChanRx(Box<Ty>),
    ChanTx(Box<Ty>),
    ChanBi(Box<Ty>),
    Pointer(Box<Ty>),
    Function(Signature),
    Interface(Vec<MethodSpec>),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum PrimaryExprInner {
    Literal(Literal),
    Identifier(FullIdentifier),
    Parenthesis(Box<Expression>),
    Conversion {
        to: Ty,
        expression: Box<Expression>,
    },
    MethodExpr {
        receiver: Ty,
        method_identifier: usize,
    },
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum PrimaryExprMod {
    Selector(usize),
    Index(Box<Expression>),
    Slice2 {
        left: Option<Box<Expression>>,
        right: Option<Box<Expression>>,
    },
    Slice3 {
        left: Option<Box<Expression>>,
        center: Box<Expression>,
        right: Box<Expression>,
    },
    TypeAssertion(Ty),
    EmptyCall,
    TypeCall {
        ty: Ty,
        expressions: Vec<Expression>,
        dotdotdot: bool,
    },
    Call {
        expressions: Vec<Expression>,
        dotdotdot: bool,
    },
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct PrimaryExpr {
    pub inner: PrimaryExprInner,
    pub mods: Vec<PrimaryExprMod>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum UnaryOp {
    Plus, Minus, Bang, Hat, Multiply, And, LeftArrow
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Signature {
    pub params: Vec<ParametersDecl>,
    pub result: Option<Box<SignatureResult>>,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub enum SignatureResult {
    Params(Vec<ParametersDecl>),
    TypeResult(Ty)
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct ParametersDecl {
    pub idents: Vec<usize>,
    pub dotdotdot: bool,
    pub ty: Ty,
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct FuncDecl {
    pub name: usize,
    pub receiver: Option<Vec<ParametersDecl>>,
    #[serde(flatten)]
    pub signature: Signature
}



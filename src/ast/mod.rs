pub use types::Keyword;
pub mod expr;
pub mod literals;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    And,
    Or,
    Hat,
    LeftShift,
    RightShift,
    AndHat,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum PairedToken {
    /// `(` or `)`
    Parenthesis,
    /// `[` or `]`
    Bracket,
    /// `{` or `}`
    Brace,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Operator {
    BinOp(BinaryOp),
    BinOpAssign(BinaryOp),
    And,
    Or,
    LeftArrow,
    Increment,
    Decrement,
    Equals,
    Le,
    Lt,
    Ge,
    Gt,
    Ne,
    Bang,
    Assign,
    ColonAssign,
    DotDotDot,
    Left(PairedToken),
    Right(PairedToken),
    Comma,
    Dot,
    Colon,
    Semicolon,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Program {
    pub package: Identifier,
}

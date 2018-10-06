pub mod literals;

#[derive(Eq, PartialEq, Debug)]
pub enum Keyword {
    Break, Case, Chan, Const, Continue, Default,
    Defer, Else, Fallthrough, For, Func, Go, Goto,
    If, Import, Interface, Map, Package, Range, Return,
    Select, Struct, Switch, Type, Var,
}

#[derive(Eq, PartialEq, Debug)]
pub enum BinaryOp {
    Plus, Minus, Multiply, Divide, Modulus, And, Or, Hat,
    LeftShift, RightShift, AndHat,
}

#[derive(Eq, PartialEq, Debug)]
pub enum PairedToken {
    /// `(` or `)`
    Parenthesis,
    /// `[` or `]`
    Bracket,
    /// `{` or `}`
    Brace,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Operator {
    BinOp(BinaryOp), BinOpAssign(BinaryOp),
    And, Or, LeftArrow, Increment, Decrement,
    Equals, Le, Lt, Ge, Gt, Ne,
    Bang, Assign, ColonAssign, DotDotDot,
    Left(PairedToken), Right(PairedToken),
    Comma, Dot, Colon, Semicolon,
}

use ast::literals::Literal;
use types::BinaryOp;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Operation {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

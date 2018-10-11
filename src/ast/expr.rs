use ast::literals::Literal;
use ast::BinaryOp;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Operation {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

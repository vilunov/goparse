use ast::BinaryOp;
use ast::literals::Literal;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Operation {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

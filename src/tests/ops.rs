use ast::expr::Expression;
use ast::literals::Literal;
use types::BinaryOp;
use rules::expr::*;

const EMPTY: &'static str = "";
const ONE: &'static str = "1";

macro_rules! test {
    ($name: ident, $input: expr, $expect: expr) => {
        #[test]
        fn $name() {
            let input = format!("1{}1", $input);
            let expect = Expression::Operation {
                left: Box::new(Expression::Literal(Literal::Decimal(ONE.to_string()))),
                op: $expect,
                right: Box::new(Expression::Literal(Literal::Decimal(ONE.to_string()))),
            };
            let result = expr(&input);
            assert_eq!(Ok((EMPTY, expect)), result);
        }
    };
}

test!(plus, "+", BinaryOp::Plus);
test!(minus, "-", BinaryOp::Minus);
test!(mult, "*", BinaryOp::Multiply);
test!(div, "/", BinaryOp::Divide);
test!(modulus, "%", BinaryOp::Modulus);
test!(and, "&", BinaryOp::And);
test!(or, "|", BinaryOp::Or);
test!(shl, "<<", BinaryOp::LeftShift);
test!(shr, ">>", BinaryOp::RightShift);
test!(andhat, "&^", BinaryOp::AndHat);

use ast::BinaryOp;
use rules::expr::*;

const EMPTY: &[u8] = &[];

macro_rules! test {
    ($name: ident, $input: expr, $expect: expr) => {
        #[test]
        fn $name() {
            let result = bin_op($input);
            assert_eq!(Ok((EMPTY, $expect)), result);
        }
    }
}

test!(plus,    b"+",  BinaryOp::Plus);
test!(minus,   b"-",  BinaryOp::Minus);
test!(mult,    b"*",  BinaryOp::Multiply);
test!(div,     b"/",  BinaryOp::Divide);
test!(modulus, b"%",  BinaryOp::Modulus);
test!(and,     b"&",  BinaryOp::And);
test!(or,      b"|",  BinaryOp::Or);
test!(shl,     b"<<", BinaryOp::LeftShift);
test!(shr,     b">>", BinaryOp::RightShift);
test!(andhat,  b"&^", BinaryOp::AndHat);

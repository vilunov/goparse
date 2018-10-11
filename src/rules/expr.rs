use ast::expr::Expression;
use ast::literals::Literal;
use ast::BinaryOp;

named!(pub bin_op<&str, BinaryOp>, alt!(
    tag!("+") =>  { |_| BinaryOp::Plus } |
    tag!("-") =>  { |_| BinaryOp::Minus } |
    tag!("*") =>  { |_| BinaryOp::Multiply } |
    tag!("/") =>  { |_| BinaryOp::Divide } |
    tag!("%") =>  { |_| BinaryOp::Modulus } |
    tag!("&^") => { |_| BinaryOp::AndHat } |
    tag!("&") =>  { |_| BinaryOp::And } |
    tag!("|") =>  { |_| BinaryOp::Or } |
    tag!("<<") => { |_| BinaryOp::LeftShift } |
    tag!(">>") => { |_| BinaryOp::RightShift }
));

named!(pub expr<&str, Expression>, do_parse!(
    // left:  literal_decimal >>
    tag!("1") >>
    op:    bin_op          >>
    tag!("1") >>
    // right: literal_decimal >>
    (Expression::Operation {
        // left,
        left: Box::new(Expression::Literal(Literal::Decimal("1".to_string()))),
        op,
        right: Box::new(Expression::Literal(Literal::Decimal("1".to_string()))),
    })
));

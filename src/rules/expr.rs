use ast::BinaryOp;

named!(pub bin_op<&[u8], BinaryOp>, alt_complete!(
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

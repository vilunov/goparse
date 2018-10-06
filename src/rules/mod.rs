pub mod literals;
pub mod expr;

use ast::Keyword;

named!(pub keyword<&[u8], Keyword>, alt!(
    tag!("break") => { |_| Keyword::Break }
));

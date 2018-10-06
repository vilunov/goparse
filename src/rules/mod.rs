use std::str::from_utf8;

pub mod literals;
pub mod expr;

use ast::Keyword;
use ast::Identifier;

named!(pub keyword<&str, Keyword>, alt!(
    tag!("break") => { |_| Keyword::Break }
));

fn create_identifier(i: &str) -> Result<Identifier, ()> {
    Ok(Identifier(i.to_string()))
}

named!(pub ident<&str, Identifier>, map_res!(
    take_while!(char::is_alphanumeric),
    create_identifier
));

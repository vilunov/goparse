use std::str::from_utf8;

pub mod literals;
pub mod expr;

use ast::*;

named!(pub line_sep<&str, &str>, tag!(";"));

named!(pub keyword<&str, Keyword>, alt!(
    tag!("break") => { |_| Keyword::Break }
));

fn create_identifier(i: &str) -> Result<Identifier, ()> {
    Ok(Identifier(i.to_string()))
}

named!(pub ident<&str, Identifier>, map_res!(
    re_find!(r"^[\w&&[^0-9]]\w*"),
    create_identifier
));

named!(pub package_clause<&str, Identifier>, ws!(do_parse!(
    tag!("package") >> package_name: ident >> line_sep >>
    (package_name)
)));

named!(pub program<&str, Program>, ws!(do_parse!(
    package: package_clause >>
    (Program {
        package
    })
)));

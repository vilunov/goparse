use nom::IResult;
use nom::{need_more, Context, Err as ParseError, ErrorKind, Needed};

use ast::*;
use types::BinaryOp::*;
use types::Keyword::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;
use types::*;

fn token(token: Token, i: &[Token]) -> IResult<&[Token], ()> {
    if i.len() < 1 {
        need_more(i, Needed::Size(1))
    } else if i[0] != token {
        Err(ParseError::Error(Context::Code(i, ErrorKind::Tag)))
    } else {
        Ok((&i[1..], ()))
    }
}

macro_rules! token {
    ($name: ident, $val: expr) => {
        fn $name(i: &[Token]) -> IResult<&[Token], ()> {
            token($val, i)
        }
    };
}

token!(keyword_package, Kw(Package));
token!(keyword_import, Kw(Import));
token!(semicolon, Punc(Semicolon));
token!(dot, Punc(Dot));
token!(open_paren, Punc(Left(Parenthesis)));
token!(close_paren, Punc(Right(Parenthesis)));

fn identifier(tokens: &[Token]) -> IResult<&[Token], usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Ident(id) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}

fn string_literal(tokens: &[Token]) -> IResult<&[Token], usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let InterpretedString(id) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}

named!(import_spec(&[Token]) -> ImportSpec, do_parse!(
       package: opt!(alt!(value!(ImportSpecPackage::Dot, dot)
                        | map!(identifier, |i| ImportSpecPackage::Package(i))))
    >> path: string_literal

    >> (ImportSpec { package, path })
));

named!(import_spec_multi(&[Token]) -> Vec<ImportSpec>, do_parse!(
       open_paren
    >> specs: many0!(terminated!(import_spec, semicolon))
    >> close_paren

    >> (specs)
));

named!(import_decl(&[Token]) -> Vec<ImportSpec>, do_parse!(
       keyword_import
    >> specs: alt!(map!(import_spec, |i| vec![i])
                 | import_spec_multi)
    >> semicolon

    >> (specs)
));

named!(pub program(&[Token]) -> Program, do_parse!(
       keyword_package
    >> package: identifier
    >> semicolon
    >> imports: map!(many0!(complete!(import_decl)),
                     |i: Vec<Vec<ImportSpec>>| i.into_iter()
                                                .flatten()
                                                .collect::<Vec<ImportSpec>>())

    >> (Program {
        package, imports,
    })
));

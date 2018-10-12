use nom::IResult;
use nom::{need_more, Needed, Err as ParseError, Context, ErrorKind};

use ast::*;
use types::Keyword::*;
use types::BinaryOp::*;
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

fn keyword_package(i: &[Token]) -> IResult<&[Token], ()> {
    token(Kw(Package), i)
}

fn semicolon(i: &[Token]) -> IResult<&[Token], ()> {
    token(Punc(Semicolon), i)
}

fn identifier(tokens: &[Token]) -> IResult<&[Token], usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Ident(id) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}


named!(pub program<&[Token], Program>, do_parse!(
       keyword_package
    >> package: identifier
    >> semicolon

    >> (Program {
        package,
    })
));

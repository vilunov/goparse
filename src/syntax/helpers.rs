use nom::{self, need_more, Context, Err as ParseError, ErrorKind, Needed};

use ast::{UnaryOp, BinaryOp as AstBinaryOp};
use types::BinaryOp::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;
use types::*;

pub type IResult<'a, T> = nom::IResult<&'a [Token], T>;

pub fn token(i: &[Token], token: Token) -> IResult<()> {
    if i.len() < 1 {
        need_more(i, Needed::Size(1))
    } else if i[0] != token {
        Err(ParseError::Error(Context::Code(i, ErrorKind::Tag)))
    } else {
        Ok((&i[1..], ()))
    }
}

pub fn one_or_many<'a, T, F>(i: &'a [Token], parser: &F) -> IResult<'a, Vec<T>>
where
    F: Fn(&[Token]) -> IResult<T>,
{
    alt!(
        i,
        map!(parser, |i| vec![i]) | do_parse!(
            open_paren >> m: many0!(terminated!(call!(parser), semicolon)) >> close_paren >> (m)
        )
    )
}

pub fn flat_vec<T>(v: Vec<Vec<T>>) -> Vec<T> {
    v.into_iter().flatten().collect()
}

pub fn unary_op(i: &[Token]) -> IResult<UnaryOp> {
    if i.len() < 1 {
        need_more(i, Needed::Size(1))
    } else {
        match i[0] {
            BinOp(Plus) =>       Ok((&i[1..], UnaryOp::Plus)),
            BinOp(Minus) =>      Ok((&i[1..], UnaryOp::Minus)),
            Punc(Bang) =>        Ok((&i[1..], UnaryOp::Bang)),
            BinOp(Hat) =>        Ok((&i[1..], UnaryOp::Hat)),
            BinOp(Multiply) =>   Ok((&i[1..], UnaryOp::Multiply)),
            BinOp(And) =>        Ok((&i[1..], UnaryOp::And)),
            Punc(LeftArrow) =>   Ok((&i[1..], UnaryOp::LeftArrow)),
            _ => Err(ParseError::Error(Context::Code(i, ErrorKind::Tag))),
        }
    }
}

pub fn binary_op(i: &[Token]) -> IResult<AstBinaryOp> {
    if i.len() < 1 {
        need_more(i, Needed::Size(1))
    } else {
        match i[0] {
            BinOp(val) =>      Ok((&i[1..], AstBinaryOp::BinOp(val))),
            Punc(DoubleAnd) => Ok((&i[1..], AstBinaryOp::DoubleAnd)),
            Punc(DoubleOr) =>  Ok((&i[1..], AstBinaryOp::DoubleOr)),
            Punc(Equals) =>    Ok((&i[1..], AstBinaryOp::Equals)),
            Punc(Le) =>        Ok((&i[1..], AstBinaryOp::Le)),
            Punc(Lt) =>        Ok((&i[1..], AstBinaryOp::Lt)),
            Punc(Ge) =>        Ok((&i[1..], AstBinaryOp::Ge)),
            Punc(Gt) =>        Ok((&i[1..], AstBinaryOp::Gt)),
            Punc(Ne) =>        Ok((&i[1..], AstBinaryOp::Ne)),
            _ => Err(ParseError::Error(Context::Code(i, ErrorKind::Tag))),
        }
    }
}

macro_rules! token {
    ($name: ident, $val: expr) => {
        pub fn $name(i: &[Token]) -> IResult<()> {
            token(i, $val)
        }
    };
}

token!(semicolon, Punc(Semicolon));
token!(dot, Punc(Dot));
token!(comma, Punc(Comma));
token!(open_paren, Punc(Left(Parenthesis)));
token!(close_paren, Punc(Right(Parenthesis)));
token!(open_bracket, Punc(Left(Bracket)));
token!(close_bracket, Punc(Right(Bracket)));
token!(open_brace, Punc(Left(Brace)));
token!(close_brace, Punc(Right(Brace)));
token!(colon, Punc(Colon));
token!(dot_dot_dot, Punc(DotDotDot));
token!(star, BinOp(Multiply));
token!(left_arrow, Punc(LeftArrow));
token!(assign, Punc(Assign));

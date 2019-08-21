use nom::{self, Err, Needed};
use nom::branch::alt;
use nom::combinator::map;
use nom::error::{ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::terminated;

use crate::ast::{BinaryOp as AstBinaryOp, UnaryOp};
use crate::types::*;
use crate::types::BinaryOp::*;
use crate::types::Keyword::*;
use crate::types::PairedToken::*;
use crate::types::Punctuation::*;
use crate::types::Token::*;

pub type IResult<'a, T> = nom::IResult<&'a [Token], T>;

pub fn token(i: &[Token], token: Token) -> IResult<()> {
    if i.len() < 1 {
        Err(Err::Incomplete(Needed::Size(1)))
    } else if i[0] != token {
        Err(Err::Error(<_>::from_error_kind(i, ErrorKind::Tag)))
    } else {
        Ok((&i[1..], ()))
    }
}

pub fn one_or_many<'a, T>(
    input: &'a [Token],
    parser: &impl Fn(&[Token]) -> IResult<T>,
) -> IResult<'a, Vec<T>> {
    let one = map(parser, |i| vec![i]);
    let many = |input| {
        let (input, _) = open_paren(input)?;
        let (input, m) = many0(terminated(parser, semicolon))(input)?;
        let (input, _) = close_paren(input)?;
        Ok((input, m))
    };
    alt((one, many))(input)
}

pub fn flat_vec<T>(v: Vec<Vec<T>>) -> Vec<T> {
    v.into_iter().flatten().collect()
}

pub fn unary_op(i: &[Token]) -> IResult<UnaryOp> {
    if i.len() < 1 {
        Err(Err::Incomplete(Needed::Size(1)))
    } else {
        match i[0] {
            BinOp(Plus) => Ok((&i[1..], UnaryOp::Plus)),
            BinOp(Minus) => Ok((&i[1..], UnaryOp::Minus)),
            Punc(Bang) => Ok((&i[1..], UnaryOp::Bang)),
            BinOp(Hat) => Ok((&i[1..], UnaryOp::Hat)),
            BinOp(Multiply) => Ok((&i[1..], UnaryOp::Multiply)),
            BinOp(And) => Ok((&i[1..], UnaryOp::And)),
            Punc(LeftArrow) => Ok((&i[1..], UnaryOp::LeftArrow)),
            _ => Err(Err::Error(<_>::from_error_kind(i, ErrorKind::Tag))),
        }
    }
}

pub fn assign_op(i: &[Token]) -> IResult<Option<BinaryOp>> {
    if i.len() < 1 {
        Err(Err::Incomplete(Needed::Size(1)))
    } else {
        match i[0] {
            BinOpAssign(x) => Ok((&i[1..], Some(x))),
            Punc(Assign) => Ok((&i[1..], None)),
            _ => Err(Err::Error(<_>::from_error_kind(i, ErrorKind::Tag))),
        }
    }
}

pub fn binary_op(i: &[Token]) -> IResult<AstBinaryOp> {
    if i.len() < 1 {
        Err(Err::Incomplete(Needed::Size(1)))
    } else {
        match i[0] {
            BinOp(val) => Ok((&i[1..], AstBinaryOp::BinOp(val))),
            Punc(DoubleAnd) => Ok((&i[1..], AstBinaryOp::DoubleAnd)),
            Punc(DoubleOr) => Ok((&i[1..], AstBinaryOp::DoubleOr)),
            Punc(Equals) => Ok((&i[1..], AstBinaryOp::Equals)),
            Punc(Le) => Ok((&i[1..], AstBinaryOp::Le)),
            Punc(Lt) => Ok((&i[1..], AstBinaryOp::Lt)),
            Punc(Ge) => Ok((&i[1..], AstBinaryOp::Ge)),
            Punc(Gt) => Ok((&i[1..], AstBinaryOp::Gt)),
            Punc(Ne) => Ok((&i[1..], AstBinaryOp::Ne)),
            _ => Err(Err::Error(<_>::from_error_kind(i, ErrorKind::Tag))),
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
token!(double_and, Punc(DoubleAnd));
token!(increment, Punc(Increment));
token!(decrement, Punc(Decrement));
token!(equals, Punc(Equals));
token!(le, Punc(Le));
token!(lt, Punc(Lt));
token!(ge, Punc(Ge));
token!(gt, Punc(Gt));
token!(ne, Punc(Ne));
token!(bang, Punc(Bang));
token!(colon_assign, Punc(ColonAssign));
token!(assign, Punc(Assign));

token!(kw_break, Kw(Break));
token!(kw_case, Kw(Case));
token!(kw_const, Kw(Const));
token!(kw_cont, Kw(Continue));
token!(kw_default, Kw(Default));
token!(kw_defer, Kw(Defer));
token!(kw_fallthrough, Kw(Fallthrough));
token!(kw_for, Kw(For));
token!(kw_func, Kw(Func));
token!(kw_go, Kw(Go));
token!(kw_goto, Kw(Goto));
token!(kw_if, Kw(If));
token!(kw_import, Kw(Import));
token!(kw_interface, Kw(Interface));
token!(kw_map, Kw(Map));
token!(kw_package, Kw(Package));
token!(kw_range, Kw(Range));
token!(kw_return, Kw(Return));
token!(kw_select, Kw(Select));
token!(kw_struct, Kw(Struct));
token!(kw_switch, Kw(Switch));
token!(kw_type, Kw(Type));
token!(kw_var, Kw(Var));

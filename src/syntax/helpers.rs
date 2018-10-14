use nom::{self, need_more, Context, Err as ParseError, ErrorKind, Needed};

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
token!(dot_dot_dot, Punc(DotDotDot));

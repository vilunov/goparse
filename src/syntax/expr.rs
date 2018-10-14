use ast::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;
use types::*;

use syntax::helpers::*;
use syntax::{identifier, full_identifier, literal, ty};

named!(primary_expression_inner(&[Token]) -> PrimaryExprInner, alt!(
    map!(tuple!(open_paren, expression, close_paren),
         |(_, i, _)| PrimaryExprInner::Parenthesis(Box::new(i))) |
    do_parse!(
        to: ty >> open_paren >> expr: expression >> opt!(comma) >> close_paren
        >> (PrimaryExprInner::Conversion { to, expression: Box::new(expr) })
    ) |
    do_parse!(
        receiver: ty >> method_identifier: identifier
        >> (PrimaryExprInner::MethodExpr { receiver, method_identifier})
    ) |
    map!(full_identifier, PrimaryExprInner::Identifier) |
    map!(literal, PrimaryExprInner::Literal)
));

named!(primary_expression_modifier(&[Token]) -> PrimaryExprMod, alt!(
    do_parse!(
        dot >> identifier: identifier
        >> (PrimaryExprMod::Selector(identifier))
    ) |
    do_parse!(
        apply!(token, Punc(Left(Bracket))) >> expr: expression >> apply!(token, Punc(Right(Bracket)))
        >> (PrimaryExprMod::Index(Box::new(expr)))
    )
));

named!(primary_expression(&[Token]) -> PrimaryExpr, do_parse!(
    inner: primary_expression_inner >> mods: many0!(primary_expression_modifier)
    >> (PrimaryExpr { inner, mods })
));

named!(pub expression(&[Token]) -> Expression, alt!(
    map!(primary_expression, Expression::Primary)
));

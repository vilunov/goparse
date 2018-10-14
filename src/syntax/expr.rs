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

named!(primary_expression(&[Token]) -> PrimaryExpr, do_parse!(
       inner: primary_expression_inner >> mods: many0!(primary_expression_modifier)
    >> (PrimaryExpr { inner, mods })
));

named!(primary_expression_modifier(&[Token]) -> PrimaryExprMod, alt!(
    do_parse!(
           dot >> identifier: identifier
        >> (PrimaryExprMod::Selector(identifier))
    ) |
    do_parse!(
           open_bracket
        >> left: opt!(expression) >> colon >> center: expression >> colon >> right: expression
        >> close_bracket

        >> (PrimaryExprMod::Slice3 {
            left: left.map(Box::new),
            center: Box::new(center),
            right: Box::new(right),
        })
    ) |
    do_parse!(
           open_bracket
        >> left: opt!(expression) >> colon >> right: opt!(expression)
        >> close_bracket

        >> (PrimaryExprMod::Slice2 { left: left.map(Box::new), right: right.map(Box::new) })
    ) |
    do_parse!(
           open_bracket >> expr: expression >> close_bracket
        >> (PrimaryExprMod::Index(Box::new(expr)))
    ) |
    do_parse!(
           dot >> open_paren >> ty: ty >> close_paren
        >> (PrimaryExprMod::TypeAssertion(ty))
    ) |
    do_parse!(
           open_paren >> close_paren
        >> (PrimaryExprMod::EmptyCall)
    ) |
    do_parse!(
           open_paren
        >> ty: ty
        >> expr: tuple!(comma, separated_list!(comma, expression))
        >> dots: opt!(dot_dot_dot)
        >> close_paren

        >> (PrimaryExprMod::TypeCall { ty, expressions: expr.1, dotdotdot: dots.is_some() })
    ) |
    do_parse!(
           open_paren
        >> expressions: separated_nonempty_list!(comma, expression)
        >> dots: opt!(dot_dot_dot)
        >> close_paren

        >> (PrimaryExprMod::Call { expressions, dotdotdot: dots.is_some() })
    )
));

named!(unary_expression(&[Token]) -> UnaryExpression, do_parse!(
       ops: many0!(unary_op)
    >> primary: primary_expression

    >> (UnaryExpression { ops, primary })
));

named!(pub expression(&[Token]) -> Expression, do_parse!(
       head: unary_expression
    >> tail: many0!(tuple!(binary_op, unary_expression))

    >> (Expression { head, tail })
));

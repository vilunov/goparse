use ast::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;
use types::*;

use syntax::helpers::*;
use syntax::{identifier, full_identifier, literal, ty};

named!(primary_expression_inner(&[Token]) -> PrimaryExprInner, alt!(
    map!(composite_literal, PrimaryExprInner::CompositeLiteral) |
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

named!(pub primary_expression(&[Token]) -> PrimaryExpr, do_parse!(
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
        >> expr: opt!(map!(tuple!(comma, expression_list), |(_, i)| i))
        >> dots: opt!(dot_dot_dot)
        >> opt!(comma)
        >> close_paren

        >> (PrimaryExprMod::TypeCall {
            ty,
            expressions: expr.unwrap_or(vec![]),
            dotdotdot: dots.is_some()
        })
    ) |
    do_parse!(
           open_paren
        >> expressions: expression_list
        >> dots: opt!(dot_dot_dot)
        >> opt!(comma)
        >> close_paren

        >> (PrimaryExprMod::Call { expressions, dotdotdot: dots.is_some() })
    )
));

named!(expr_or_literal_value(&[Token]) -> ExprOrLiteralValue, alt!(
    map!(expression, ExprOrLiteralValue::Expression) |
    map!(literal_value, ExprOrLiteralValue::LiteralValue)
));

named!(literal_value(&[Token]) -> Vec<LiteralElement>, do_parse!(
       open_brace
    >> values: map!(opt!(tuple!(separated_nonempty_list!(comma, literal_element), opt!(comma))),
                    |i| i.map(|(j, _)| j).unwrap_or(vec![]))
    >> close_brace

    >> (values)
));

named!(literal_element(&[Token]) -> LiteralElement, do_parse!(
       key: opt!(map!(tuple!(expr_or_literal_value, colon), |(i, _)| i))
    >> element: expr_or_literal_value

    >> (LiteralElement { key, element })
));

named!(composite_literal(&[Token]) -> CompositeLiteral, do_parse!(
       start: opt!(tuple!(open_bracket, dot_dot_dot, close_bracket))
    >> ty: ty
    >> value: literal_value

    >> (CompositeLiteral {
        strange_things: start.is_some(),
        ty,
        value,
    })
));

named!(unary_expression(&[Token]) -> UnaryExpression, do_parse!(
       ops: many0!(unary_op)
    >> primary: primary_expression

    >> (UnaryExpression { ops, primary })
));

named!(pub expression(&[Token]) -> Expression, do_parse!(
       head: unary_expression
    >> tail: many0!(map!(tuple!(binary_op, unary_expression),
                         |(op, expression)| OpExpression { op, expression } ))

    >> (Expression { head, tail })
));

named!(pub expression_list(&[Token]) -> Vec<Expression>,
    separated_nonempty_list!(comma, expression)
);

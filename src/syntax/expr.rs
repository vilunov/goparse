use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::tuple;

use crate::ast::*;
use crate::syntax::{full_identifier, identifier, literal, ty};
use crate::syntax::helpers::*;
use crate::types::*;

use super::helpers::IResult;

fn primary_expression_inner(input: &[Token]) -> IResult<PrimaryExprInner> {
    fn conversion(input: &[Token]) -> IResult<PrimaryExprInner> {
        let (input, to) = ty(input)?;
        let (input, _) = open_paren(input)?;
        let (input, expr) = expression(input)?;
        let (input, _) = opt(comma)(input)?;
        let (input, _) = close_paren(input)?;
        Ok((
            input,
            PrimaryExprInner::Conversion {
                to,
                expression: Box::new(expr),
            },
        ))
    }

    fn method_expr(input: &[Token]) -> IResult<PrimaryExprInner> {
        let (input, receiver) = ty(input)?;
        let (input, method_identifier) = identifier(input)?;
        Ok((
            input,
            PrimaryExprInner::MethodExpr {
                receiver,
                method_identifier,
            },
        ))
    }

    alt((
        map(composite_literal, PrimaryExprInner::CompositeLiteral),
        map(tuple((open_paren, expression, close_paren)), |(_, i, _)| {
            PrimaryExprInner::Parenthesis(Box::new(i))
        }),
        conversion,
        method_expr,
        map(full_identifier, PrimaryExprInner::Identifier),
        map(literal, PrimaryExprInner::Literal),
    ))(input)
}

pub fn primary_expression(input: &[Token]) -> IResult<PrimaryExpr> {
    let (input, inner) = primary_expression_inner(input)?;
    let (input, mods) = many0(primary_expression_modifier)(input)?;
    Ok((input, PrimaryExpr { inner, mods }))
}

pub fn primary_expression_modifier(input: &[Token]) -> IResult<PrimaryExprMod> {
    fn selector(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = dot(input)?;
        let (input, identifier) = identifier(input)?;
        Ok((input, PrimaryExprMod::Selector(identifier)))
    }

    fn slice3(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_bracket(input)?;
        let (input, left) = opt(expression)(input)?;
        let (input, _) = colon(input)?;
        let (input, center) = expression(input)?;
        let (input, _) = colon(input)?;
        let (input, right) = expression(input)?;
        let (input, _) = close_bracket(input)?;
        Ok((
            input,
            PrimaryExprMod::Slice3 {
                left: left.map(Box::new),
                center: Box::new(center),
                right: Box::new(right),
            },
        ))
    }

    fn slice2(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_bracket(input)?;
        let (input, left) = opt(expression)(input)?;
        let (input, _) = colon(input)?;
        let (input, right) = opt(expression)(input)?;
        let (input, _) = close_bracket(input)?;
        Ok((
            input,
            PrimaryExprMod::Slice2 {
                left: left.map(Box::new),
                right: right.map(Box::new),
            },
        ))
    }

    fn index(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_bracket(input)?;
        let (input, expr) = expression(input)?;
        let (input, _) = close_bracket(input)?;
        Ok((input, PrimaryExprMod::Index(Box::new(expr))))
    }

    fn type_assertion(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_paren(input)?;
        let (input, t) = ty(input)?;
        let (input, _) = close_paren(input)?;
        Ok((input, PrimaryExprMod::TypeAssertion(t)))
    }

    fn empty_call(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_paren(input)?;
        let (input, _) = close_paren(input)?;
        Ok((input, PrimaryExprMod::EmptyCall))
    }

    fn type_call(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_paren(input)?;
        let (input, t) = ty(input)?;
        let (input, expr) = opt(map(tuple((comma, expression_list)), |(_, i)| i))(input)?;
        let (input, dots) = opt(dot_dot_dot)(input)?;
        let (input, _) = opt(comma)(input)?;
        let (input, _) = close_paren(input)?;
        Ok((
            input,
            PrimaryExprMod::TypeCall {
                ty: t,
                expressions: expr.unwrap_or(vec![]),
                dotdotdot: dots.is_some(),
            },
        ))
    }

    fn call(input: &[Token]) -> IResult<PrimaryExprMod> {
        let (input, _) = open_paren(input)?;
        let (input, expressions) = expression_list(input)?;
        let (input, dots) = opt(dot_dot_dot)(input)?;
        let (input, _) = opt(comma)(input)?;
        let (input, _) = close_paren(input)?;
        Ok((
            input,
            PrimaryExprMod::Call {
                expressions,
                dotdotdot: dots.is_some(),
            },
        ))
    }

    alt((
        selector,
        slice3,
        slice2,
        index,
        type_assertion,
        empty_call,
        type_call,
        call,
    ))(input)
}

pub fn expr_or_literal_value(input: &[Token]) -> IResult<ExprOrLiteralValue> {
    alt((
        map(expression, ExprOrLiteralValue::Expression),
        map(literal_value, ExprOrLiteralValue::LiteralValue),
    ))(input)
}

pub fn literal_value(input: &[Token]) -> IResult<Vec<LiteralElement>> {
    let (input, _) = open_brace(input)?;
    let (input, values) = map(
        opt(tuple((
            separated_nonempty_list(comma, literal_element),
            opt(comma),
        ))),
        |i| i.map(|(j, _)| j).unwrap_or(vec![]),
    )(input)?;
    let (input, _) = close_brace(input)?;
    Ok((input, values))
}

pub fn literal_element(input: &[Token]) -> IResult<LiteralElement> {
    let (input, key) = opt(map(tuple((expr_or_literal_value, colon)), |(i, _)| i))(input)?;
    let (input, element) = expr_or_literal_value(input)?;

    Ok((input, LiteralElement { key, element }))
}

pub fn composite_literal(input: &[Token]) -> IResult<CompositeLiteral> {
    let (input, start) = opt(tuple((open_bracket, dot_dot_dot, close_bracket)))(input)?;
    let (input, t) = ty(input)?;
    let (input, value) = literal_value(input)?;

    Ok((
        input,
        CompositeLiteral {
            strange_things: start.is_some(),
            ty: t,
            value,
        },
    ))
}

pub fn unary_expression(input: &[Token]) -> IResult<UnaryExpression> {
    let (input, ops) = many0(unary_op)(input)?;
    let (input, primary) = primary_expression(input)?;

    Ok((input, UnaryExpression { ops, primary }))
}

pub fn expression(input: &[Token]) -> IResult<Expression> {
    let (input, head) = unary_expression(input)?;
    let (input, tail) = many0(map(
        tuple((binary_op, unary_expression)),
        |(op, expression)| OpExpression { op, expression },
    ))(input)?;

    Ok((input, Expression { head, tail }))
}

pub fn expression_list(input: &[Token]) -> IResult<Vec<Expression>> {
    separated_list(comma, expression)(input)
}

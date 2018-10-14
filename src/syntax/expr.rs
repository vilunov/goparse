use ast::*;
use types::*;

use syntax::helpers::*;
use syntax::{full_identifier, literal};

named!(primary_expression(&[Token]) -> PrimaryExpr, alt!(
    map!(literal, PrimaryExpr::Literal) |
    map!(full_identifier, PrimaryExpr::Identifier) |
    map!(tuple!(open_paren, expression, close_paren),
         |(_, i, _)| PrimaryExpr::Parenthesis(Box::new(i)))
));

named!(pub expression(&[Token]) -> Expression, alt!(
    map!(primary_expression, Expression::Primary)
));

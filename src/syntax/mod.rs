use nom::{need_more, Context, Err as ParseError, ErrorKind, Needed};

use ast::*;
use types::BinaryOp::*;
use types::Keyword::*;
use types::Literal::*;
use types::Token::*;
use types::*;

mod helpers;
mod expr;

use self::helpers::*;
pub use self::expr::*;

fn identifier(tokens: &[Token]) -> IResult<usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Ident(id) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}

fn string_literal(tokens: &[Token]) -> IResult<usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Lit(InterpretedString(id)) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}

fn literal(tokens: &[Token]) -> IResult<Literal> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Lit(ref lit) = tokens[0] {
        Ok((&tokens[1..], lit.clone()))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }
}

named!(qualified_identifier(&[Token]) -> FullIdentifier, do_parse!(
       package: identifier
    >> dot
    >> identifier: identifier

    >> (FullIdentifier::Qualified {package, identifier})
));

named!(pub full_identifier(&[Token]) -> FullIdentifier, alt!(
    qualified_identifier |
    map!(identifier, FullIdentifier::Unqualified)
));

named!(ty(&[Token]) -> Ty, alt!(
    map!(full_identifier, Ty::TypeName) |
    do_parse!(
           open_bracket
        >> length: expression
        >> close_bracket
        >> elems: ty

        >> (Ty::Array { length: Box::new(length), elements: Box::new(elems) })
    )
));

named!(import_spec(&[Token]) -> ImportSpec, do_parse!(
       package: opt!(alt!(value!(ImportSpecPackage::Dot, dot)
                        | map!(identifier, ImportSpecPackage::Package)))
    >> path: string_literal

    >> (ImportSpec { package, path })
));

named!(import_decl(&[Token]) -> Vec<ImportSpec>, do_parse!(
       apply!(token, Kw(Import))
    >> specs: call!(one_or_many, &import_spec)
    >> semicolon

    >> (specs)
));

named!(const_spec(&[Token]) -> ConstSpec, do_parse!(
       identifiers: separated_nonempty_list!(comma, identifier)
    >> right_side: opt!(map!(tuple!(opt!(ty),
                                    apply!(token, Punc(Punctuation::Assign)),
                                    separated_nonempty_list!(comma, expression)),
                             |(i, _, j)| ConstSpecRightSide { ty: i, expressions: j }))

    >> (ConstSpec { identifiers, right_side })
));

named!(const_decl(&[Token]) -> Vec<ConstSpec>, do_parse!(
       apply!(token, Kw(Const))
    >> specs: call!(one_or_many, &const_spec)
    >> semicolon

    >> (specs)
));

named!(type_spec(&[Token]) -> TypeSpec, do_parse!(
       identifier: identifier
    >> opt!(apply!(token, Punc(Punctuation::Assign)))
    >> ty: ty

    >> (TypeSpec { identifier, ty } )
));

named!(type_decl(&[Token]) -> Vec<TypeSpec>, do_parse!(
       apply!(token, Kw(Type))
    >> specs: call!(one_or_many, &type_spec)
    >> semicolon

    >> (specs)
));

named!(var_spec(&[Token]) -> VarSpec, do_parse!(
       identifiers: separated_nonempty_list!(comma, identifier)
    >> right_side: opt!(alt!(
            do_parse!(
                    ty: ty
                 >> expression: opt!(map!(tuple!(
                                        apply!(token, Punc(Punctuation::Assign)),
                                        separated_nonempty_list!(comma, expression)),
                                        |(_, v)| v))
                 >> (VarRightSide::WithType { ty, expression} )
            ) |
            map!(tuple!(
                apply!(token, Punc(Punctuation::Assign)),
                separated_nonempty_list!(comma, expression)),
                |(_, v)| VarRightSide::WithoutType(v))))
    >> (VarSpec { identifiers, right_side })
));

named!(var_decl(&[Token]) -> Vec<VarSpec>, do_parse!(
       apply!(token, Kw(Var))
    >> specs: call!(one_or_many, &var_spec)
    >> semicolon

    >> (specs)
));

named!(parameters_spec(&[Token]) -> ParametersDecl, do_parse!(
        idents: separated_nonempty_list!(comma, identifier)
    >>  ddd: opt!(dot_dot_dot)
    >>  i: ty

    >> (ParametersDecl { idents, dotdotdot: ddd.is_some(), ty: i })
));

named!(parameters_decl(&[Token]) -> Vec<ParametersDecl>, do_parse!(
        open_paren
    >>  parameters: separated_list!(comma, &parameters_spec)
    >>  opt!(comma)
    >>  close_paren

    >> (parameters)
));

named!(signature(&[Token]) -> Signature, do_parse!(
        params: parameters_decl
    >>  result: opt!(alt!(map!(parameters_decl, SignatureResult::Params)
                        | map!(ty, SignatureResult::TypeResult)))

    >> (Signature { params, result })
));

named!(func_decl(&[Token]) -> FuncDecl, do_parse!(
       apply!(token, Kw(Func))
    >> name: identifier
    >> signature: signature
    >> semicolon

    >> (FuncDecl { name, signature })
));

named!(top_level_decl(&[Token]) -> TopLevelDecl,
    alt!(map!(const_decl, TopLevelDecl::Consts)
        |map!(var_decl, TopLevelDecl::Vars)
        |map!(func_decl, TopLevelDecl::Function)
        |map!(type_decl, TopLevelDecl::Types))
);

named!(pub program(&[Token]) -> Program, do_parse!(
       apply!(token, Kw(Package))
    >> package: identifier
    >> semicolon
    >> imports: map!(many0!(complete!(import_decl)), flat_vec)
    >> decls: many0!(complete!(top_level_decl))

    >> (Program {
        package, imports, decls,
    })
));

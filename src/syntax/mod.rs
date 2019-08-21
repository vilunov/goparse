use nom::{need_more, Context, Err as ParseError, ErrorKind, Needed};
use nom::multi::{separated_nonempty_list, many0, separated_list};
use nom::combinator::{map, opt, value};
use nom::branch::alt;
use nom::sequence::tuple;

use crate::ast::*;
use crate::types::BinaryOp::*;
use crate::types::Keyword::*;
use crate::types::Literal::*;
use crate::types::Token::*;
use crate::types::Punctuation::*;
use crate::types::*;

mod helpers;
mod expr;

use helpers::*;
pub use expr::*;

fn identifier(tokens: &[Token]) -> IResult<usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Ident(id) = tokens[0] {
        Ok((&tokens[1..], id))
    } else {
        Err(ParseError::Error(Context::Code(tokens, ErrorKind::Tag)))
    }

}

fn identifier_list(input: &[Token]) -> IResult<Vec<usize>> {
    separated_nonempty_list(comma, identifier)(input)
}

fn string_literal(tokens: &[Token]) -> IResult<usize> {
    if tokens.len() < 1 {
        need_more(tokens, Needed::Size(1))
    } else if let Lit(InterpretedString(id)) = tokens[0] {
        Ok((&tokens[1..], id))
    } else if let Lit(RawString(id)) = tokens[0] {
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

fn qualified_identifier(input: &[Token]) -> IResult<FullIdentifier> {
    let (input, package) = identifier(input)?;
    let (input, _) = dot(input)?;
    let (input, identifier) = identifier(input)?;
    Ok((input, FullIdentifier::Qualified {package, identifier }))
}

pub fn full_identifier(input: &[Token]) -> IResult<FullIdentifier> {
    alt((
        qualified_identifier,
        map(identifier, FullIdentifier::Unqualified)
    ))(input)
}

pub fn ty(input: &[Token]) -> IResult<Ty> {
    fn array(input: &[Token]) -> IResult<Ty> {
        let (input, _) = open_bracket(input)?;
        let (input, length) = opt(expression)(input)?;
        let (input, _) = close_bracket(input)?;
        let (input, elems) = ty(input)?;
        Ok((input, Ty::Array { length: length.map(Box::new), elements: Box::new(elems) }))
    }

    fn boxx(input: &[Token]) -> IResult<Ty> {
        let (input, _) = kw_map(input)?;
        let (input, _) = open_bracket(input)?;
        let (input, keys) = ty(input)?;
        let (input, _) = close_bracket(input)?;
        let (input, elems) = ty(input)?;
        Ok((input, Ty::Map { keys: Box::new(keys), elements: Box::new(elems) }))
    }

    fn pointer(input: &[Token]) -> IResult<Ty> {
        let (input, _) = star(input)?;
        let (input, t) = ty(input)?;
        Ok((input, Ty::Pointer(Box::new(t))))
    }

    fn chan_bi(input: &[Token]) -> IResult<Ty> {
        let (input, _) = token(input, Kw(Chan))?;
        let (input, t) = ty(input)?;
        Ok((input, Ty::ChanBi(Box::new(t))))
    }

    fn chan_tx(input: &[Token]) -> IResult<Ty> {
        let (input, _) = token(input, Kw(Chan))?;
        let (input, _) = left_arrow(input)?;
        let (input, t) = ty(input)?;
        Ok((input, Ty::ChanTx(Box::new(t))))
    }

    fn chan_rx(input: &[Token]) -> IResult<Ty> {
        let (input, _) = left_arrow(input)?;
        let (input, _) = token(input, Kw(Chan))?;
        let (input, t) = ty(input)?;
        Ok((input, Ty::ChanRx(Box::new(t))))
    }

    fn function(input: &[Token]) -> IResult<Ty> {
        let (input, _) = token(input, Kw(Func))?;
        let (input, s) = signature(input)?;
        Ok((input, Ty::Function(s)))
    }

    fn interface(input: &[Token]) -> IResult<Ty> {
        let (input, _) = token(input, Kw(Interface))?;
        let (input, _) = open_brace(input)?;
        let (input, specs) = many0(tuple((method_spec, semicolon)))(input)?;
        let (input, _) = close_brace(input)?;
        let specs = specs.into_iter().map(|(i, _)| i).collect();
        Ok((input, Ty::Interface(specs)))
    }

    fn strukt(input: &[Token]) -> IResult<Ty> {
        let (input, _) = token(input, Kw(Struct))?;
        let (input, _) = open_brace(input)?;
        let (input, specs) = many0(tuple((field_decl, semicolon)))(input)?;
        let (input, _) = close_brace(input)?;
        let specs = specs.into_iter().map(|(i, _)| i).collect();
        Ok((input, Ty::Struct(specs)))
    }

    fn nested(input: &[Token]) -> IResult<Ty> {
        let (input, _) = open_paren(input)?;
        let (input, i) = ty(input)?;
        let (input, _) = close_paren(input)?;
        Ok((input, i))
    }

    alt((
        map(full_identifier, Ty::TypeName),
        array,
        boxx,
        pointer,
        chan_bi,
        chan_tx,
        chan_rx,
        function,
        interface,
        strukt,
        nested,
    ))(input)
}


fn method_spec(input: &[Token]) -> IResult<MethodSpec> {
    fn method(input: &[Token]) -> IResult<MethodSpec> {
        let (input, name) = identifier(input)?;
        let (input, signature) = signature(input)?;
        Ok((input, MethodSpec::Method { name, signature }))
    }

    fn interface(input: &[Token]) -> IResult<MethodSpec> {
        let (input, identifier) = full_identifier(input)?;
        Ok((input, MethodSpec::Interface(identifier)))
    }

    alt((
        method,
        interface,
    ))(input)
}

fn field_decl(input: &[Token]) -> IResult<FieldDecl> {
    fn explicit(input: &[Token]) -> IResult<FieldDeclInner> {
        let (input, identifiers) = identifier_list(input)?;
        let (input, ty) = ty(input)?;
        Ok((input, FieldDeclInner::Explicit { identifiers, ty }))
    }
    fn embedded(input: &[Token]) -> IResult<FieldDeclInner> {
        let (input, i) = opt(star)(input)?;
        let (input, type_name) = full_identifier(input)?;
        let star = i.is_some();
        Ok((input, FieldDeclInner::Embedded { star, type_name }))
    }

    let (input, inner) = alt((explicit, embedded))(input)?;
    let (input, tag) = opt(string_literal)(input)?;
    Ok((input, FieldDecl { inner, tag }))
}

fn import_spec(input: &[Token]) -> IResult<ImportSpec> {
    let (input, package) = opt(alt((
        value(ImportSpecPackage::Dot, dot),
        map(identifier, ImportSpecPackage::Package),
    )))(input)?;
    let (input, path) = string_literal(input)?;
    Ok((input, ImportSpec { package, path }))
}

fn import_decl(input: &[Token]) -> IResult<Vec<ImportSpec>> {
    let (input, _) = token(input, Kw(Import))?;
    let (input, specs) = one_or_many(input, &import_spec)?;
    let (input, _) = semicolon(input)?;
    Ok((input, specs))
}

fn const_spec(input: &[Token]) -> IResult<ConstSpec> {
    fn right_side(input: &[Token]) -> IResult<ConstSpecRightSide> {
        let (input, ty) = opt(ty)(input)?;
        let (input, _) = assign(input)?;
        let (input, expressions) = expression_list(input)?;
        Ok((input, ConstSpecRightSide { ty, expressions }))
    }

    let (input, identifiers) = identifier_list(input)?;
    let (input, right_side) = opt(right_side)(input)?;
    Ok((input, ConstSpec { identifiers, right_side }))
}

fn const_decl(input: &[Token]) -> IResult<Vec<ConstSpec>> {
    let (input, _) = token(input, Kw(Const))?;
    let (input, specs) = one_or_many(input, &const_spec)?;
    Ok((input, specs))
}

fn type_spec(input: &[Token]) -> IResult<TypeSpec> {
    let (input, identifier) = identifier(input)?;
    let (input, _) = opt(assign)(input)?;
    let (input, ty) = ty(input)?;
    Ok((input, TypeSpec { identifier, ty }))
}

fn type_decl(input: &[Token]) -> IResult<Vec<TypeSpec>> {
    let (input, _) = token(input, Kw(Type))?;
    let (input, specs) = one_or_many(input, &type_spec)?;
    Ok((input, specs))
}

fn var_spec(input: &[Token]) -> IResult<VarSpec> {
    fn right_side_with_type(input: &[Token]) -> IResult<VarRightSide> {
        let (input, ty) = ty(input)?;
        let (input, k) = opt(tuple((assign, expression_list)))(input)?;
        let expression = k.map(|(_, i)| i);
        Ok((input, VarRightSide::WithType { ty, expression }))
    }

    fn right_side_without_type(input: &[Token]) -> IResult<VarRightSide> {
        let (input, _) = assign(input)?;
        let (input, expressions) = expression_list(input)?;
        Ok((input, VarRightSide::WithoutType(expressions)))
    }

    let (input, identifiers) = identifier_list(input)?;
    let (input, right_side) = opt(alt((right_side_with_type, right_side_without_type)))(input)?;
    Ok((input, VarSpec { identifiers, right_side }))
}

fn var_decl(input: &[Token]) -> IResult<Vec<VarSpec>> {
    let (input, _) = token(input, Kw(Var))?;
    let (input, specs) = one_or_many(input, &var_spec)?;
    Ok((input, specs))
}

pub fn parameters_spec(input: &[Token]) -> IResult<ParametersDecl> {
    let (input, idents) = separated_list(comma, identifier)(input)?;
    let (input, ddd) = opt(dot_dot_dot)(input)?;
    let (input, ty) = ty(input)?;
    let dotdotdot = ddd.is_some();
    Ok((input, ParametersDecl { idents, dotdotdot, ty }))
}

fn parameters_decl(input: &[Token]) -> IResult<Vec<ParametersDecl>> {
    let (input, _) = open_paren(input)?;
    let (input, parameters) = separated_list(comma, parameters_spec)(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, parameters))
}

pub fn signature(input: &[Token]) -> IResult<Signature> {
    let (input, params) = parameters_decl(input)?;
    let (input, result) = opt(alt((
        map(ty, SignatureResult::TypeResult),
        map(parameters_decl, SignatureResult::Params),
    )))(input)?;
    Ok((input, Signature { params, result: result.map(Box::new) }))
}

fn func_decl(input: &[Token]) -> IResult<FuncDecl> {
    let (input, _) = token(input, Kw(Func))?;
    let (input, receiver) = opt(parameters_decl)(input)?;
    let (input, name) = identifier(input)?;
    let (input, signature) = signature(input)?;
    let (input, body) = opt(block)(input)?;
    Ok((input, FuncDecl { name, receiver, signature, body }))
}

fn decl(input: &[Token]) -> IResult<Declaration> {
    alt((
        map(const_decl, Declaration::ConstDecl),
        map(var_decl, Declaration::VarDecl),
        map(type_decl, Declaration::TypeDecl),
    ))(input)
}

fn top_level_decl(input: &[Token]) -> IResult<TopLevelDecl> {
    let (input, stuff) = alt((
        map(decl, TopLevelDecl::Decl),
        map(func_decl, TopLevelDecl::Function),
    ))(input)?;
    let (input, _) = semicolon(input)?;
    Ok((input, stuff))
}

fn simple_stmt(input: &[Token]) -> IResult<SimpleStatement> {
    fn assign_stmt(input: &[Token]) -> IResult<SimpleStatement> {
        let (input, left) = expression_list(input)?;
        let (input, op) = assign_op(input)?;
        let (input, right) = expression_list(input)?;
        Ok((input, SimpleStatement::AssignStmt {left, op, right}))
    }

    fn short_var_stmt(input: &[Token]) -> IResult<SimpleStatement> {
        let (input, identifiers) = identifier_list(input)?;
        let (input, _) = colon_assign(input)?;
        let (input, expressions) = expression_list(input)?;
        Ok((input, SimpleStatement::ShortVarStmt { identifiers, expressions}))
    }

    fn send_stmt(input: &[Token]) -> IResult<SimpleStatement> {
        let (input, left) = expression(input)?;
        let (input, _) = token(input, Punc(LeftArrow))?;
        let (input, right) = expression(input)?;
        Ok((input, SimpleStatement::SendStmt { left, right }))
    }

    fn inc_stmt(input: &[Token]) -> IResult<SimpleStatement> {
        let (input, left) = expression(input)?;
        let (input, _) = token(input, Punc(Increment))?;
        Ok((input, SimpleStatement::IncStmt(left)))
    }

    fn dec_stmt(input: &[Token]) -> IResult<SimpleStatement> {
        let (input, left) = expression(input)?;
        let (input, _) = token(input, Punc(Decrement))?;
        Ok((input, SimpleStatement::DecStmt(left)))
    }

    alt((
        assign_stmt,
        short_var_stmt,
        send_stmt,
        inc_stmt,
        dec_stmt,
        map(expression, SimpleStatement::Expr)
    ))(input)
}

named!(stmt(&[Token]) -> Statement, alt!(
    map!(decl, Statement::Decl) |
    map!(simple_stmt, Statement::Simple) |
    do_parse!(
           label: identifier
        >> colon
        >> stmt: stmt
        >> (Statement::Labeled { label, statement: Box::new(stmt) })
    ) |
    map!(tuple!(apply!(token, Kw(Go)), expression), |(_, i)| Statement::Go(i)) |
    map!(tuple!(apply!(token, Kw(Defer)), expression), |(_, i)| Statement::Defer(i)) |
    map!(tuple!(apply!(token, Kw(Return)), separated_list!(comma, expression)),
         |(_, i)| Statement::Return(i)) |
    map!(tuple!(apply!(token, Kw(Break)), opt!(identifier)), |(_, i)| Statement::Break(i)) |
    map!(tuple!(apply!(token, Kw(Continue)), opt!(identifier)), |(_, i)| Statement::Continue(i)) |
    map!(tuple!(apply!(token, Kw(Goto)), identifier), |(_, i)| Statement::Goto(i)) |
    map!(apply!(token, Kw(Fallthrough)), |_| Statement::Fallthrough) |
    map!(block, Statement::Block) |
    map!(if_statement, Statement::If) |
    expr_switch |
    type_switch |
    map!(tuple!(kw_for, for_clause, block), |(_, clause, body)| Statement::For { clause, body })
));

named!(for_clause(&[Token]) -> ForClause, alt!(
    do_parse!(
           init: opt!(simple_stmt) >> semicolon
        >> condition: opt!(expression) >> semicolon
        >> post: opt!(simple_stmt)
        >> (ForClause::Clause {
            init: init.map(Box::new),
            condition: condition.map(Box::new),
            post: post.map(Box::new)
        })
    ) |
    do_parse!(
           expr: expression_list >> assign
        >> kw_range
        >> range: expression
        >> (ForClause::RangeExpr { expr, range: Box::new(range) })
    ) |
    do_parse!(
           identifiers: identifier_list >> colon_assign
        >> kw_range
        >> range: expression
        >> (ForClause::RangeIdents { identifiers, range: Box::new(range) })
    ) |
    map!(tuple!(kw_range, expression), |(_, i)| ForClause::Range(Box::new(i))) |
    map!(expression, |i| ForClause::Condition(Box::new(i)))
));

named!(if_statement(&[Token]) -> IfStmt, do_parse!(
       apply!(token, Kw(If))
    >> simple: opt!(map!(tuple!(simple_stmt, semicolon), |(i, _)| i))
    >> expression: expression
    >> block: block
    >> e: opt!(if_inner)

    >> (IfStmt {simple, expression, block, else_branch: e.map(Box::new) })
));

named!(if_inner(&[Token]) -> IfInner, do_parse!(
       apply!(token, Kw(Else))
    >> i: alt!(map!(if_statement, IfInner::IfStmt) | map!(block, IfInner::Block))

    >> (i)
));

named!(block(&[Token]) -> Block, do_parse!(
       open_brace
    >> statements: many0!(map!(tuple!(stmt, semicolon), |(i, _)| i))
    >> close_brace

    >> (Block { statements })
));

named!(expr_switch(&[Token]) -> Statement, do_parse!(
       apply!(token, Kw(Switch))
    >> simple: opt!(map!(tuple!(simple_stmt, semicolon), |(i, _)| i))
    >> expression: opt!(expression)
    >> open_brace
    >> clauses: many0!(do_parse!(
           expressions: alt!(
               map!(apply!(token, Kw(Default)), |_| None) |
               map!(tuple!(apply!(token, Kw(Case)), expression_list), |(_, i)| Some(i))
           )
        >> colon
        >> statements: separated_list!(semicolon, stmt)
        >> semicolon
        >> (ExprSwitchCase { expressions, statements })
    ))
    >> close_brace

    >> (Statement::ExprSwitchStmt { simple, expression, clauses })
));

named!(type_switch_guard(&[Token]) -> TypeSwitchGuard, do_parse!(
       identifier: opt!(map!(tuple!(identifier, colon_assign), |(i, _)| i))
    >> primary: primary_expression

    >> (TypeSwitchGuard { identifier, primary })
));

named!(type_switch(&[Token]) -> Statement, do_parse!(
       apply!(token, Kw(Switch))
    >> simple: opt!(map!(tuple!(simple_stmt, semicolon), |(i, _)| i))
    >> guard: type_switch_guard
    >> dot >> open_paren >> apply!(token, Kw(Type)) >> close_paren >> open_brace


    >> clauses: many0!(do_parse!(
           type_list: alt!(
               map!(apply!(token, Kw(Default)), |_| None) |
               map!(tuple!(apply!(token, Kw(Case)), separated_nonempty_list!(comma, ty)), |(_, i)| Some(i))
           )
        >> colon
        >> statements: separated_list!(semicolon, stmt)
        >> semicolon
        >> (TypeSwitchCase { type_list, statements })
    ))
    >> close_brace

    >> (Statement::TypeSwitchStmt { simple, guard, clauses })
));

named!(select_stmt(&[Token]) -> Statement, do_parse!(
       kw_select
    >> open_brace
    >> clauses: many0!(do_parse!(
            comm_case: alt!(
                 map!(tuple!(
                        kw_case,
                        expression,
                        apply!(token, Punc(LeftArrow)),
                        expression),
                 |(_, left, _, right)| Some(CommCase::SendStmt { left, right })) |
                 map!(tuple!(
                        kw_case,
                        expression_list,
                        assign,
                        expression),
                 |(_, list, _, expr)| Some(CommCase::RecvExprStmt { list, expr })) |
                 map!(tuple!(
                        kw_case,
                        identifier_list,
                        assign,
                        expression),
                 |(_, list, _, expr)| Some(CommCase::RecvIdentsStmt { list, expr})) |
                 map!(kw_default, |_| None)
            )
        >> statements: separated_list!(comma, stmt)

        >> (SelectClause { comm_case, statements })
    ))
    >> close_brace

    >> (Statement::SelectStmt(clauses) )
));

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

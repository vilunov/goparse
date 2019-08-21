use nom::{need_more, Context, Err as ParseError, ErrorKind, Needed};
use nom::apply;

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

named!(identifier_list(&[Token]) -> Vec<usize>,
    separated_nonempty_list!(comma, identifier)
);

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

named!(pub ty(&[Token]) -> Ty, alt!(
    map!(full_identifier, Ty::TypeName) |
    do_parse!(
           open_bracket
        >> length: opt!(expression)
        >> close_bracket
        >> elems: ty

        >> (Ty::Array { length: length.map(Box::new), elements: Box::new(elems) })
    ) |
    do_parse!(
           kw_map
        >> open_bracket
        >> keys: ty
        >> close_bracket
        >> elems: ty

        >> (Ty::Map { keys: Box::new(keys), elements: Box::new(elems) })
    ) |
    do_parse!(
           star
        >> t: ty
        >> (Ty::Pointer(Box::new(t)))
    ) |
    do_parse!(
           apply!(token, Kw(Chan))
        >> t: ty
        >> (Ty::ChanBi(Box::new(t)))
    ) |
    do_parse!(
           apply!(token, Kw(Chan)) >> left_arrow
        >> t: ty
        >> (Ty::ChanTx(Box::new(t)))
    ) |
    do_parse!(
           left_arrow >> apply!(token, Kw(Chan))
        >> t: ty
        >> (Ty::ChanRx(Box::new(t)))
    ) |
    do_parse!(
           apply!(token, Kw(Func))
        >> s: signature
        >> (Ty::Function(s))
    ) |
    do_parse!(
           apply!(token, Kw(Interface))
        >> open_brace
        >> specs: many0!(map!(tuple!(method_spec, semicolon), |(i, _)| i))
        >> close_brace

        >> (Ty::Interface(specs))
    ) |
    do_parse!(
           apply!(token, Kw(Struct))
        >> open_brace
        >> specs: many0!(map!(tuple!(field_decl, semicolon), |(i, _)| i))
        >> close_brace

        >> (Ty::Struct(specs))
    ) |
    map!(tuple!(open_paren, ty, close_paren), |(_, i, _)| i)
));

named!(method_spec(&[Token]) -> MethodSpec, alt!(
    map!(tuple!(identifier, signature), |(i, j)| MethodSpec::Method { name: i, signature: j }) |
    map!(full_identifier, MethodSpec::Interface)

));

named!(field_decl(&[Token]) -> FieldDecl, do_parse!(
       inner: alt!(do_parse!(identifiers: identifier_list >>
                             ty: ty >>
                             (FieldDeclInner::Explicit { identifiers, ty })) |
                   map!(tuple!(opt!(star), full_identifier),
                        |(i, j)| FieldDeclInner::Embedded { star: i.is_some(), type_name: j })

       )
    >> tag: opt!(string_literal)

    >> (FieldDecl { inner, tag })
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
       identifiers: identifier_list
    >> right_side: opt!(map!(tuple!(opt!(ty), assign, expression_list),
                             |(i, _, j)| ConstSpecRightSide { ty: i, expressions: j }))

    >> (ConstSpec { identifiers, right_side })
));

named!(const_decl(&[Token]) -> Vec<ConstSpec>, do_parse!(
       apply!(token, Kw(Const))
    >> specs: call!(one_or_many, &const_spec)

    >> (specs)
));

named!(type_spec(&[Token]) -> TypeSpec, do_parse!(
       identifier: identifier
    >> opt!(assign)
    >> ty: ty

    >> (TypeSpec { identifier, ty } )
));

named!(type_decl(&[Token]) -> Vec<TypeSpec>, do_parse!(
       apply!(token, Kw(Type))
    >> specs: apply!(one_or_many, &type_spec)

    >> (specs)
));

named!(var_spec(&[Token]) -> VarSpec, do_parse!(
       identifiers: identifier_list
    >> right_side: opt!(alt!(
            do_parse!(
                    ty: ty
                 >> expression: opt!(map!(tuple!(assign, expression_list),
                                          |(_, v)| v))
                 >> (VarRightSide::WithType { ty, expression} )
            ) |
            map!(
                tuple!(assign, expression_list),
                |(_, v)| VarRightSide::WithoutType(v)
            )
    ))
    >> (VarSpec { identifiers, right_side })
));

named!(var_decl(&[Token]) -> Vec<VarSpec>, do_parse!(
       apply!(token, Kw(Var))
    >> specs: apply!(one_or_many, &var_spec)

    >> (specs)
));

named!(pub parameters_spec(&[Token]) -> ParametersDecl, do_parse!(
       idents: separated_list!(comma, identifier)
    >> ddd: opt!(dot_dot_dot)
    >> ty: ty

    >> (ParametersDecl {
        idents,
        dotdotdot: ddd.is_some(),
        ty
    })
));

named!(parameters_decl(&[Token]) -> Vec<ParametersDecl>, do_parse!(
        open_paren
    >>  parameters: separated_list!(comma, &parameters_spec)
    >>  close_paren

    >> (parameters)
));

named!(pub signature(&[Token]) -> Signature, do_parse!(
        params: parameters_decl
    >>  result: opt!(alt!(map!(ty, SignatureResult::TypeResult)
                        | map!(parameters_decl, SignatureResult::Params)))

    >> (Signature { params, result: result.map(Box::new) })
));

named!(func_decl(&[Token]) -> FuncDecl, do_parse!(
       apply!(token, Kw(Func))
    >> receiver: opt!(parameters_decl)
    >> name: identifier
    >> signature: signature
    >> body: opt!(block)

    >> (FuncDecl { name, receiver, signature, body })
));

named!(decl(&[Token]) -> Declaration,
    alt!(map!(const_decl, Declaration::ConstDecl)
        |map!(var_decl, Declaration::VarDecl)
        |map!(type_decl, Declaration::TypeDecl)
));

named!(top_level_decl(&[Token]) -> TopLevelDecl, do_parse!(
       stuff: alt!(map!(decl, TopLevelDecl::Decl)
                 | map!(func_decl, TopLevelDecl::Function))
    >> semicolon

    >> (stuff)
));

named!(simple_stmt(&[Token]) -> SimpleStatement, alt!(
       do_parse!(
                left: expression_list
             >> op: assign_op
             >> right: expression_list

             >> (SimpleStatement::AssignStmt { left, op, right })
       ) |
       do_parse!(
                identifiers: identifier_list
             >> colon_assign
             >> expressions: expression_list

             >> (SimpleStatement::ShortVarStmt { identifiers, expressions })
       ) |
       do_parse!(
              left: expression
           >> apply!(token, Punc(LeftArrow))
           >> right: expression

           >> (SimpleStatement::SendStmt { left, right } )
       ) |
       do_parse!(
              left: expression >> apply!(token, Punc(Increment))
           >> (SimpleStatement::IncStmt(left))
       ) |
       do_parse!(
              left: expression >> apply!(token, Punc(Decrement))
           >> (SimpleStatement::DecStmt(left))
       ) |
       map!(expression, SimpleStatement::Expr)
));

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

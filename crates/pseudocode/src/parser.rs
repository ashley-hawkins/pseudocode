use chumsky::{
    input::ValueInput,
    pratt::{infix, left, postfix, prefix},
    prelude::*,
};

use crate::{
    expr::{ArrayIndex, BinaryOperator, Expr, UnaryOperator},
    token::{self, Token},
    util::{SourceSpan, Spanned},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct GotoStatement {
    pub line_number: Spanned<usize>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ReturnStatement<'a> {
    pub expr: Option<Spanned<Expr<'a>>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct SwapStatement<'a> {
    pub lhs: AssignmentLhs<'a>,
    pub rhs: AssignmentLhs<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct Block<'a>(pub Vec<Spanned<Statement<'a>>>);

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct IfStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
    pub then_branch: Block<'a>,
    pub else_branch: Option<Block<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ForStatement<'a> {
    pub loop_variable: &'a str,
    pub start_expr: Spanned<Expr<'a>>,
    pub end_expr: Spanned<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct WhileStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AssignmentStatement<'a> {
    pub lhs: AssignmentLhs<'a>,
    pub expression: Spanned<Expr<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct DebugStatement<'a> {
    pub with_newline: bool,
    pub args: Vec<DebugArgument<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum DebugArgument<'a> {
    String(&'a str),
    Expr(Spanned<Expr<'a>>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum AssignmentLhs<'a> {
    Variable(Spanned<&'a str>),
    ArrayAccess {
        left: Box<Spanned<Expr<'a>>>,
        right: Box<Spanned<Expr<'a>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AssertStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, derive_more::From)]
pub enum Statement<'a> {
    Assert(AssertStatement<'a>),
    Goto(GotoStatement),
    Swap(SwapStatement<'a>),
    Assignment(AssignmentStatement<'a>),
    If(IfStatement<'a>),
    While(WhileStatement<'a>),
    For(ForStatement<'a>),
    Return(ReturnStatement<'a>),
    BareExpr(Spanned<Expr<'a>>),
    Debug(DebugStatement<'a>),
    DebugStack,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ProcedureDefinition<'a> {
    pub name: Spanned<&'a str>,
    pub parameters: Spanned<Vec<Spanned<&'a str>>>,
    pub body: Block<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AstRoot<'a> {
    pub procedures: Vec<Spanned<ProcedureDefinition<'a>>>,
    pub main_algorithm: Spanned<Block<'a>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum Mode {
    JumpyImp,
    StructuredImp,
    #[default]
    ProceduralImp,
}

pub fn parse_pseudocode_program<
    'src,
    I: ValueInput<'src, Token = Token<'src>, Span = SourceSpan>,
>(
    mode: Mode,
) -> impl chumsky::prelude::Parser<
    'src,
    I,
    AstRoot<'src>,
    extra::Full<Rich<'src, Token<'src>, SourceSpan>, (), ()>,
> {
    let variable_access =
        select! { token::Token::Identifier(ident) => ident }.map(Expr::VariableAccess);

    let boolean_literal =
        select! { token::Token::BoolLiteral(value) => value }.map(Expr::BooleanLiteral);

    let number_literal = select! { token::Token::NumberLiteral(value) => value.parse().unwrap() }
        .map(Expr::NumberLiteral);

    let value = choice((boolean_literal, number_literal, variable_access));

    let expr = recursive(|expr| {
        let paren_expr = expr
            .clone()
            .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
            .map(|span: Spanned<Expr>| span.inner);

        let array_literal = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSquareBracket), just(Token::RSquareBracket))
            .map(Expr::ArrayLiteral);

        let function_call = select! { token::Token::Identifier(name) => name }
            .spanned()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
                    .spanned(),
            )
            .try_map(
                |(name, args): (Spanned<&str>, Spanned<Vec<Spanned<Expr>>>), _| {
                    Ok(match name.inner {
                        "build" => {
                            if args.inner.len() != 1 {
                                // TODO: Any way to return more detailed info rather than just a string?
                                return Err(chumsky::error::Rich::custom(
                                    args.span,
                                    "build() takes exactly one argument",
                                ));
                            }
                            Expr::Build(Box::new(args.inner[0].clone()))
                        }
                        _ => Expr::FunctionCall {
                            left: name,
                            arguments: args,
                        },
                    })
                },
            );

        let atom_common = choice((
            paren_expr.spanned(),
            array_literal.spanned(),
            value.spanned(),
        ));

        let atom = if mode == Mode::ProceduralImp {
            choice((function_call.spanned(), atom_common)).boxed()
        } else {
            atom_common.boxed()
        };

        let array_index = choice((
            expr.clone()
                .or_not()
                .then(just(Token::Colon).map(drop).spanned())
                .then(expr.clone().or_not())
                .map(
                    |((start, colon), end): ((_, Spanned<()>), _)| ArrayIndex::Slice {
                        start: start.map(Box::new),
                        colon,
                        end: end.map(Box::new),
                    },
                ),
            expr.clone().map(|e| ArrayIndex::SingleIndex(Box::new(e))),
        ))
        .delimited_by(just(Token::LSquareBracket), just(Token::RSquareBracket))
        .spanned();

        // Boolean operators

        fn fold_binary_operation<
            'src,
            'b,
            I: ValueInput<'src, Token = Token<'src>, Span = SourceSpan>,
        >(
            lhs: Spanned<Expr<'src>>,
            op: Spanned<Token<'src>>,
            rhs: Spanned<Expr<'src>>,
            extra: &mut chumsky::input::MapExtra<
                'src,
                'b,
                I,
                extra::Full<Rich<'src, Token<'src>, SourceSpan>, (), ()>,
            >,
        ) -> Spanned<Expr<'src>> {
            Spanned {
                inner: Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: Spanned {
                        inner: BinaryOperator::try_from(op.inner).unwrap(),
                        span: op.span,
                    },
                    right: Box::new(rhs),
                },
                span: extra.span(),
            }
        }

        let or_operation = infix(left(1), just(Token::Or).spanned(), fold_binary_operation);
        let and_operation = infix(left(2), just(Token::And).spanned(), fold_binary_operation);
        let comparison_operations = infix(
            left(3),
            one_of([
                Token::Lt,
                Token::Gt,
                Token::Lte,
                Token::Gte,
                Token::Eq,
                Token::Neq,
            ])
            .spanned(),
            fold_binary_operation,
        );
        let add_sub_operation = infix(
            left(4),
            one_of([Token::Add, Token::Subtract]).spanned(),
            fold_binary_operation,
        );
        let mul_div_operation = infix(
            left(5),
            one_of([Token::Multiply, Token::Divide]).spanned(),
            fold_binary_operation,
        );
        let unary_operation = prefix(
            6,
            one_of([Token::Subtract, Token::Not]).spanned(),
            |op: Spanned<Token>, rhs, extra| Spanned {
                inner: Expr::UnaryOp {
                    op: Spanned {
                        inner: match op.inner {
                            Token::Subtract => UnaryOperator::Neg,
                            Token::Not => UnaryOperator::Not,
                            _ => unreachable!(),
                        },
                        span: op.span,
                    },
                    expr: Box::new(rhs),
                },
                span: extra.span(),
            },
        );
        let array_access = postfix(7, array_index, |lhs, rhs, extra| Spanned {
            inner: Expr::ArrayAccess {
                left: Box::new(lhs),
                right: rhs,
            },
            span: extra.span(),
        });

        let is_ascending = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("ascending")))
                .map(|_| UnaryOperator::IsAscending)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_strictly_ascending = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("strictly")))
                .then(just(token::Token::Identifier("ascending")))
                .map(|_| UnaryOperator::IsStrictlyAscending)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_descending = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("descending")))
                .map(|_| UnaryOperator::IsDescending)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_strictly_descending = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("strictly")))
                .then(just(token::Token::Identifier("descending")))
                .map(|_| UnaryOperator::IsStrictlyDescending)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_even = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("even")))
                .map(|_| UnaryOperator::IsEven)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_odd = postfix(
            6,
            just(Token::Is)
                .then(just(token::Token::Identifier("odd")))
                .map(|_| UnaryOperator::IsOdd)
                .spanned(),
            |expr: Spanned<Expr>, op, extra| Spanned {
                inner: Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                },
                span: extra.span(),
            },
        );
        let is_in = infix(
            left(0),
            just([Token::Is, Token::In]).spanned(),
            |lhs, _op, rhs, extra| Spanned {
                inner: Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: Spanned {
                        inner: BinaryOperator::IsIn,
                        span: extra.span(),
                    },
                    right: Box::new(rhs),
                },
                span: extra.span(),
            },
        );

        atom.pratt((
            or_operation,
            and_operation,
            comparison_operations,
            add_sub_operation,
            mul_div_operation,
            unary_operation,
            array_access,
            is_ascending,
            is_strictly_ascending,
            is_descending,
            is_strictly_descending,
            is_even,
            is_odd,
            is_in,
        ))
    });

    let statement = recursive(|statement| {
        let block = statement
            .spanned()
            .repeated()
            .collect::<Vec<_>>()
            .map(Block);

        let indented_block = block.delimited_by(just(Token::Indent), just(Token::Dedent));

        let assert_statement = expr
            .clone()
            .delimited_by(just([Token::Assert, Token::Colon]), just(Token::Newline))
            .map(|condition| Statement::from(AssertStatement { condition }));

        let goto_statement = select! {
            token::Token::NumberLiteral(line_num) =>  line_num.parse().unwrap()
        }
        .spanned()
        .map(|line_number| Statement::from(GotoStatement { line_number }))
        .delimited_by(just([Token::Goto, Token::Line]), just(Token::Newline));

        let debug_statement = choice((
            one_of([Token::DebugLn, Token::Debug])
                .spanned()
                .then(
                    choice((
                        select! { token::Token::StringLiteral(s) => s }
                            .map(DebugArgument::String)
                            .spanned(),
                        expr.clone().map(DebugArgument::Expr).spanned(),
                    ))
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Newline))
                .map(
                    |(token, args): (Spanned<Token>, Vec<Spanned<DebugArgument>>)| {
                        Statement::from(DebugStatement {
                            with_newline: matches!(token.inner, Token::DebugLn),
                            args: args.into_iter().map(|s| s.inner).collect(),
                        })
                    },
                ),
            just(Token::DebugStack)
                .spanned()
                .then_ignore(just(Token::Newline))
                .map(|_| Statement::DebugStack),
        ));

        let assignment_statement = choice((
            select! { token::Token::Identifier(ident) => ident }
                .then_ignore(just(Token::Assign))
                .spanned()
                .map(AssignmentLhs::Variable),
            expr.clone()
                .then_ignore(just(Token::Assign))
                .try_map(|expr, _span| match expr.inner {
                    Expr::ArrayAccess {
                        left,
                        right:
                            Spanned {
                                inner: ArrayIndex::SingleIndex(right),
                                ..
                            },
                    } => Ok(AssignmentLhs::ArrayAccess { left, right }),
                    _ => Err(chumsky::error::Rich::custom(
                        expr.span,
                        "Left-hand side of assignment must be a variable or array access",
                    )),
                }),
        ))
        .then(expr.clone())
        .then_ignore(just(Token::Newline))
        .map(|(lhs, expression)| Statement::from(AssignmentStatement { lhs, expression }));

        let swap_statement = choice((
            select! { token::Token::Identifier(ident) => ident }
                .then_ignore(just(Token::Swap))
                .spanned()
                .map(AssignmentLhs::Variable),
            expr.clone()
                .then_ignore(just(Token::Swap))
                .try_map(|expr, _span| match expr.inner {
                    Expr::ArrayAccess {
                        left,
                        right:
                            Spanned {
                                inner: ArrayIndex::SingleIndex(right),
                                ..
                            },
                    } => Ok(AssignmentLhs::ArrayAccess { left, right }),
                    _ => Err(chumsky::error::Rich::custom(
                        expr.span,
                        "Left-hand side of assignment must be a variable or array access",
                    )),
                }),
        ))
        .then(choice((
            select! { token::Token::Identifier(ident) => ident }
                .then_ignore(just(Token::Newline))
                .spanned()
                .map(AssignmentLhs::Variable),
            expr.clone()
                .then_ignore(just(Token::Newline))
                .try_map(|expr, _span| match expr.inner {
                    Expr::ArrayAccess {
                        left,
                        right:
                            Spanned {
                                inner: ArrayIndex::SingleIndex(right),
                                ..
                            },
                    } => Ok(AssignmentLhs::ArrayAccess { left, right }),
                    _ => Err(chumsky::error::Rich::custom(
                        expr.span,
                        "Left-hand side of assignment must be a variable or array access",
                    )),
                }),
        )))
        .map(|(lhs, rhs)| Statement::from(SwapStatement { lhs, rhs }));

        let if_statement = recursive(|if_statement| {
            expr.clone()
                .delimited_by(just(Token::If), just([Token::Then, Token::Newline]))
                .then(indented_block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(choice((
                            if_statement.clone().spanned().map(|stmt| Block(vec![stmt])),
                            just(Token::Newline).ignore_then(indented_block.clone()),
                        )))
                        .or_not(),
                )
                .map(|((condition, then_branch), else_branch)| {
                    Statement::from(IfStatement {
                        condition,
                        then_branch,
                        else_branch,
                    })
                })
        });

        let while_statement = expr
            .clone()
            .delimited_by(just(Token::While), just([Token::Do, Token::Newline]))
            .then(indented_block.clone())
            .map(|(condition, body)| Statement::from(WhileStatement { condition, body }));

        let for_statement = just(Token::For)
            .ignore_then(select! { token::Token::Identifier(loop_var) => loop_var })
            .then_ignore(just(token::Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::To))
            .then(expr.clone())
            .then_ignore(just([Token::Do, Token::Newline]))
            .then(indented_block.clone())
            .map(|(((loop_variable, start_expr), end_expr), body)| {
                Statement::from(ForStatement {
                    loop_variable,
                    start_expr,
                    end_expr,
                    body,
                })
            });

        let return_statement = expr
            .clone()
            .or_not()
            .delimited_by(just(Token::Return), just(Token::Newline))
            .map(|expr_opt| Statement::from(ReturnStatement { expr: expr_opt }));

        let common_statement = choice((
            assert_statement,
            debug_statement,
            return_statement,
            if_statement,
            assignment_statement,
            swap_statement,
            expr.clone()
                .then_ignore(just(Token::Newline))
                .map(Statement::BareExpr),
        ));

        let jumpy_statement = choice((common_statement.clone(), goto_statement));
        let structured_statement = choice((common_statement, for_statement, while_statement));

        if mode == Mode::JumpyImp {
            jumpy_statement.boxed()
        } else {
            structured_statement.boxed()
        }
    });

    let block = statement
        .spanned()
        .repeated()
        .collect::<Vec<_>>()
        .map(Block);
    let indented_block = block
        .clone()
        .delimited_by(just(Token::Indent), just(Token::Dedent));

    let procedure = just(Token::Procedure)
        .ignore_then(select! { token::Token::Identifier(name) => name }.spanned())
        .then(
            select! { token::Token::Identifier(param) => param }
                .spanned()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .or_not()
                .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
                .map(|params| params.unwrap_or_default())
                .spanned(),
        )
        .then_ignore(just([Token::Colon, Token::Newline]))
        .then(indented_block.clone())
        .map(|((name, parameters), body)| ProcedureDefinition {
            name,
            parameters,
            body,
        });

    if mode == Mode::ProceduralImp {
        procedure
            .spanned()
            .repeated()
            .collect::<Vec<_>>()
            .then(
                just([Token::Algorithm, Token::Colon, Token::Newline])
                    .ignore_then(indented_block)
                    .spanned(),
            )
            .map(|(procedures, statements)| AstRoot {
                procedures,
                main_algorithm: statements,
            })
            .boxed()
    } else {
        block
            .spanned()
            .map(|main_algorithm| AstRoot {
                procedures: vec![],
                main_algorithm,
            })
            .boxed()
    }
}

pub fn parse_str(
    src: &str,
    mode: Mode,
) -> ParseResult<AstRoot<'_>, Rich<'_, Token<'_>, SourceSpan>> {
    let lexer =
        crate::lexer::lex_str(src).map(|(res, span)| (res.unwrap_or_else(Token::Error), span));

    let token_stream = chumsky::input::Stream::from_iter(lexer)
        .map(SourceSpan::eof(), |(token, span): (Token, SourceSpan)| {
            (token, span)
        });

    let parser = parse_pseudocode_program(mode);

    parser.parse(token_stream)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse() {
        let source = r##"
Procedure SyntaxTest(a, b):
    if x > 10 then
        goto line 1
    else if x = 10 then
        x <- x + 1
        y <- true
        z <- a[1:2]
    else
        _ <- test

    while true do
        goto line 10
        a <- 5

    for i <- 1 to 10 do
        goto line 20
        x <-> y

    return SyntaxTest(a + b, b - a)
    
    return 10
    return

Algorithm:
    _ <- SyntaxTest(5, 10)
"##;

        let result = parse_str(source, Mode::default());

        println!("{:#?}", result);

        assert!(result.output().is_some());
        let ast_root = result.unwrap();

        todo!();
    }
}

use chumsky::{
    input::ValueInput,
    pratt::{infix, left, postfix, prefix},
    prelude::*,
    span::Spanned,
};

use crate::epic_token::{self, Operator, Token};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct GotoStatement {
    line_number: usize,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct ReturnStatement<'a> {
    expr: Option<Spanned<Expr<'a>>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct SwapStatement<'a> {
    ident_1: &'a str,
    ident_2: &'a str,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct IfStatement<'a> {
    condition: Spanned<Expr<'a>>,
    then_branch: Vec<Statement<'a>>,
    else_branch: Option<Vec<Statement<'a>>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct ForStatement<'a> {
    loop_variable: &'a str,
    start_expr: Spanned<Expr<'a>>,
    end_expr: Spanned<Expr<'a>>,
    body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct WhileStatement<'a> {
    condition: Spanned<Expr<'a>>,
    body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct AssignmentStatement<'a> {
    identifier: &'a str,
    expression: Spanned<Expr<'a>>,
}

enum SingleValue {
    Number(f64),
    Boolean(bool),
}

enum Value {
    None,
    Single(SingleValue),
    Array(Vec<SingleValue>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,

    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum ArrayIndex<'a> {
    SingleIndex(Box<Spanned<Expr<'a>>>),
    Slice {
        start: Option<Box<Spanned<Expr<'a>>>>,
        end: Option<Box<Spanned<Expr<'a>>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum Expr<'a> {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    VariableAccess(&'a str),
    FunctionCall {
        left: &'a str,
        arguments: Vec<Spanned<Expr<'a>>>,
    },
    ArrayAccess {
        left: Box<Spanned<Expr<'a>>>,
        right: ArrayIndex<'a>,
    },
    BinaryOp {
        left: Box<Spanned<Expr<'a>>>,
        op: Spanned<BinaryOperator>,
        right: Box<Spanned<Expr<'a>>>,
    },
    UnaryOp {
        op: Spanned<UnaryOperator>,
        expr: Box<Spanned<Expr<'a>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug, derive_more::From)]
enum Statement<'a> {
    Goto(GotoStatement),
    Swap(SwapStatement<'a>),
    Assignment(AssignmentStatement<'a>),
    If(IfStatement<'a>),
    While(WhileStatement<'a>),
    For(ForStatement<'a>),
    Return(ReturnStatement<'a>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ProcedureDefinition<'a> {
    pub name: &'a str,
    pub parameters: Vec<&'a str>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AstRoot<'a> {
    pub procedures: Vec<ProcedureDefinition<'a>>,
    pub statements: Vec<Statement<'a>>,
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
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
>(
    mode: Mode,
) -> impl chumsky::prelude::Parser<'src, I, AstRoot<'src>, chumsky::extra::Err<Rich<'src, Token<'src>>>>
{
    let variable_access = select! { epic_token::Token::Identifier(ident) => ident }
        .map(Expr::VariableAccess)
        .spanned();

    let boolean_literal = select! { epic_token::Token::BoolLiteral(value) => value }
        .map(Expr::BooleanLiteral)
        .spanned();

    let number_literal =
        select! { epic_token::Token::NumberLiteral(value) => value.parse().unwrap() }
            .map(Expr::NumberLiteral)
            .spanned();

    let value = boolean_literal.or(number_literal).or(variable_access);

    let expr = recursive(|expr| {
        let paren_expr = expr
            .clone()
            .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
            .map(|Spanned { inner: e, span }| Spanned {
                inner: e.inner,
                span,
            });

        let function_call = select! { epic_token::Token::Identifier(name) => name }
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket)),
            )
            .map(|(name, args)| Expr::FunctionCall {
                left: name,
                arguments: args,
            })
            .spanned();

        let atom = choice((paren_expr, function_call, value));

        let array_index = choice((
            expr.clone()
                .or_not()
                .then_ignore(just(Token::Colon))
                .then(expr.clone().or_not())
                .map(|(start, end)| ArrayIndex::Slice {
                    start: start.map(Box::new),
                    end: end.map(Box::new),
                }),
            expr.clone().map(|e| ArrayIndex::SingleIndex(Box::new(e))),
        ))
        .delimited_by(just(Token::LSquareBracket), just(Token::RSquareBracket));

        atom.pratt((
            infix(
                left(0),
                select! { epic_token::Token::Op(Operator::Or) =>  {}}.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::Or, span: op.span},
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(1),
                select! { epic_token::Token::Op(Operator::And) => {} }.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::And, span: op.span},
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(2),
                select! { epic_token::Token::Op(op) if matches!(op, Operator::Lt | Operator::Gt | Operator::Lte | Operator::Gte | Operator::Eq | Operator::Neq) => op }.spanned(),
                |lhs, op, rhs, extra| {
                    let bin_op = match op {
                        Operator::Lt => BinaryOperator::Lt,
                        Operator::Gt => BinaryOperator::Gt,
                        Operator::Lte => BinaryOperator::Lte,
                        Operator::Gte => BinaryOperator::Gte,
                        Operator::Eq => BinaryOperator::Eq,
                        Operator::Neq => BinaryOperator::Neq,
                        _ => unreachable!(),
                    };
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: bin_op, span: op.span },
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(3),
                select! { epic_token::Token::Op(Operator::Add) => {} }.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::Add, span: op.span },
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(3),
                select! { epic_token::Token::Op(Operator::Subtract) => {} }.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::Sub, span: op.span },
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(4),
                select! { epic_token::Token::Op(Operator::Multiply) => {} }.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::Mul, span: op.span },
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(4),
                select! { epic_token::Token::Op(Operator::Divide) => {} }.spanned(),
                |lhs, op, rhs, _| {
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: Spanned { inner: BinaryOperator::Div, span: op.span },
                        right: Box::new(rhs),
                    }
                },
            ),
            prefix(
                5,
                select! { epic_token::Token::Op(Operator::Subtract) => {} }.spanned(),
                |op, rhs, _| {
                    Expr::UnaryOp {
                        op: Spanned { inner: UnaryOperator::Neg, span: op.span },
                        expr: Box::new(rhs),
                    }
                },
            ),
            prefix(
                5,
                select! { epic_token::Token::Op(Operator::Not) => {} }.spanned(),
                |op, rhs, _| {
                    Expr::UnaryOp {
                        op: Spanned { inner: UnaryOperator::Not, span: op.span },
                        expr: Box::new(rhs),
                    }
                },
            ),
            postfix(
                6,
                array_index,
                |lhs, rhs, _| {
                    Expr::ArrayAccess {
                        left: Box::new(lhs),
                        right: rhs,
                    }
                },
            ),
        )).spanned()
    });

    let statement = recursive(|statement| {
        let block = statement.repeated().collect::<Vec<_>>();

        let indented_block = block.delimited_by(just(Token::Indent), just(Token::Dedent));

        let goto_statement = select! {
            epic_token::Token::NumberLiteral(line_num) => Statement::from(GotoStatement { line_number: line_num.parse().unwrap() })
        }
            .delimited_by(
                just([Token::Goto, Token::Line]),
                just(Token::Newline)
            );

        let assignment_statement = select! { epic_token::Token::Identifier(ident) => ident }
            .then_ignore(select! {epic_token::Token::Op(Operator::Assign) => {}})
            .then(expr.clone())
            .then_ignore(just(Token::Newline))
            .map(|(identifier, expression)| {
                Statement::from(AssignmentStatement {
                    identifier,
                    expression,
                })
            });

        let swap_statement = select! { epic_token::Token::Identifier(ident) => ident }
            .then_ignore(select! {epic_token::Token::Op(Operator::Swap) => {} })
            .then(select! { epic_token::Token::Identifier(ident) => ident })
            .then_ignore(just(Token::Newline))
            .map(|(ident_1, ident_2)| Statement::from(SwapStatement { ident_1, ident_2 }));

        let if_statement = expr
            .clone()
            .delimited_by(just(Token::If), just([Token::Then, Token::Newline]))
            .then(indented_block.clone())
            .then(
                just(Token::Else)
                    .ignore_then(just(Token::Newline))
                    .ignore_then(indented_block.clone())
                    .or_not(),
            )
            .map(|((condition, then_branch), else_branch)| {
                Statement::from(IfStatement {
                    condition,
                    then_branch,
                    else_branch,
                })
            });

        let while_statement = expr
            .clone()
            .delimited_by(just(Token::While), just([Token::Do, Token::Newline]))
            .then(indented_block.clone())
            .map(|(condition, body)| Statement::from(WhileStatement { condition, body }));

        let for_statement = just(Token::For)
            .ignore_then(select! { epic_token::Token::Identifier(loop_var) => loop_var })
            .then_ignore(select! { epic_token::Token::Op(Operator::Assign) => {} })
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
            .or_not()
            .delimited_by(just(Token::Return), just(Token::Newline))
            .map(|expr_opt| Statement::from(ReturnStatement { expr: expr_opt }));

        choice((
            goto_statement,
            return_statement,
            if_statement,
            while_statement,
            for_statement,
            assignment_statement,
            swap_statement,
        ))
    });

    let block = statement.repeated().collect::<Vec<_>>();
    let indented_block = block
        .clone()
        .delimited_by(just(Token::Indent), just(Token::Dedent));

    let procedure = just(Token::Procedure)
        .ignore_then(select! { epic_token::Token::Identifier(name) => name })
        .then(
            select! { epic_token::Token::Identifier(param) => param }
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .or_not()
                .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
                .map(|params| params.unwrap_or_default()),
        )
        .then_ignore(just([Token::Colon, Token::Newline]))
        .then(indented_block.clone())
        .map(|((name, parameters), body)| ProcedureDefinition {
            name,
            parameters,
            body,
        });

    procedure
        .repeated()
        .collect::<Vec<_>>()
        .then(block)
        .map(|(procedures, statements)| AstRoot {
            procedures,
            statements,
        })
}

#[cfg(test)]
mod tests {
    use chumsky::input::Stream;

    use crate::epic_token::{SourceSpan, Token};

    use super::*;
    #[test]
    fn test_parse_goto_statement() {
        let source = r##"
Procedure SyntaxTest(a, b):
    if x > 10 then
        goto line 1
    else
        x <- x + 1
        y <- true
        z <- a[1:2]

    while true do
        goto line 10
        a <- 5

    for i <- 1 to 10 do
        goto line 20
        x <-> y

    return SyntaxTest(a + b, b - a)
    
    return 10
    return
"##;

        let lexer = crate::lexer::lex_str(source)
            .map(|(res, span)| (res.unwrap_or_else(Token::Error), span));

        let token_stream = Stream::from_iter(lexer).map(
            SimpleSpan::from(source.len()..source.len()),
            |(token, span): (Token, SourceSpan)| (token, (span.start.bytes..span.end.bytes).into()),
        );

        let parser = parse_pseudocode_program(Mode::default());
        let result = parser.parse(token_stream);

        println!("{:#?}", result);

        assert!(result.output().is_some());
        let ast_root = result.unwrap();
        assert_eq!(
            ast_root,
            AstRoot {
                statements: vec![Statement::Goto(GotoStatement { line_number: 10 })],
                procedures: vec![]
            }
        );
    }
}

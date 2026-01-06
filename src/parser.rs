use chumsky::{
    input::ValueInput,
    pratt::{self, Associativity, Operator, infix, left, postfix, prefix},
    prelude::*,
};

use crate::epic_token::{self, Token};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct GotoStatement {
    line_number: usize,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct ReturnStatement<'a> {
    expr: Option<Expr<'a>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct SwapStatement<'a> {
    ident_1: &'a str,
    ident_2: &'a str,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct IfStatement<'a> {
    condition: Expr<'a>,
    then_branch: Vec<Statement<'a>>,
    else_branch: Option<Vec<Statement<'a>>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
// for ... to ... do
struct ForStatement<'a> {
    loop_variable: &'a str,
    start_expr: Expr<'a>,
    end_expr: Expr<'a>,
    body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct WhileStatement<'a> {
    condition: Expr<'a>,
    body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct AssignmentStatement<'a> {
    identifier: &'a str,
    expression: Expr<'a>,
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
    SingleIndex(Box<Expr<'a>>),
    Slice {
        start: Option<Box<Expr<'a>>>,
        end: Option<Box<Expr<'a>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum Expr<'a> {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    VariableAccess(&'a str),
    FunctionCall {
        left: &'a str,
        arguments: Vec<Expr<'a>>,
    },
    ArrayAccess {
        left: Box<Expr<'a>>,
        right: ArrayIndex<'a>,
    },
    BinaryOp {
        left: Box<Expr<'a>>,
        op: BinaryOperator,
        right: Box<Expr<'a>>,
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr<'a>>,
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
struct ProcedureDefinition<'a> {
    name: &'a str,
    parameters: Vec<&'a str>,
    body: Vec<Statement<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
struct AstRoot<'a> {
    procedures: Vec<ProcedureDefinition<'a>>,
    statements: Vec<Statement<'a>>,
}

enum Mode {
    JumpyImp,
    StructuredImp,
    ProceduralImp,
}

fn parse_pseudocode_program<
    'src,
    I: ValueInput<'src, Token = epic_token::Token<'src>, Span = SimpleSpan>,
>() -> impl chumsky::prelude::Parser<
    'src,
    I,
    AstRoot<'src>,
    chumsky::extra::Err<Rich<'src, epic_token::Token<'src>>>,
> {
    let variable_access = select! { epic_token::Token::Identifier(ident) => ident }
        .map(|ident| Expr::VariableAccess(ident));

    let boolean_literal = select! {
        epic_token::Token::BoolLiteral(value) => Expr::BooleanLiteral(value)
    };

    let number_literal = select! {
        epic_token::Token::NumberLiteral(value) => Expr::NumberLiteral(value.parse().unwrap())
    };

    let value = boolean_literal.or(number_literal).or(variable_access);

    let expr = recursive(|expr| {
        let paren_expr = expr.clone().delimited_by(
            just(epic_token::Token::LRoundBracket),
            just(epic_token::Token::RRoundBracket),
        );

        let function_call = select! { epic_token::Token::Identifier(name) => name }
            .then(
                expr.clone()
                    .separated_by(just(epic_token::Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(
                        just(epic_token::Token::LRoundBracket),
                        just(epic_token::Token::RRoundBracket),
                    ),
            )
            .map(|(name, args)| Expr::FunctionCall {
                left: name,
                arguments: args,
            });

        let atom = choice((paren_expr, function_call, value));

        let array_index = choice((
            expr.clone()
                .or_not()
                .then_ignore(just(epic_token::Token::Colon))
                .then(expr.clone().or_not())
                .map(|(start, end)| ArrayIndex::Slice {
                    start: start.map(Box::new),
                    end: end.map(Box::new),
                }),
            expr.clone().map(|e| ArrayIndex::SingleIndex(Box::new(e))),
        ))
        .delimited_by(
            just(epic_token::Token::LSquareBracket),
            just(epic_token::Token::RSquareBracket),
        );

        atom.pratt((
            infix(
                left(0),
                select! { epic_token::Token::Op(epic_token::Operator::Or) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::Or,
                    right: Box::new(rhs),
                },
            ),
            infix(
                left(1),
                select! { epic_token::Token::Op(epic_token::Operator::And) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::And,
                    right: Box::new(rhs),
                },
            ),
            infix(
                left(2),
                select! { epic_token::Token::Op(op) if matches!(op, epic_token::Operator::Lt | epic_token::Operator::Gt | epic_token::Operator::Lte | epic_token::Operator::Gte | epic_token::Operator::Eq | epic_token::Operator::Neq) => op },
                |lhs, op, rhs, _| {
                    let bin_op = match op {
                        epic_token::Operator::Lt => BinaryOperator::Lt,
                        epic_token::Operator::Gt => BinaryOperator::Gt,
                        epic_token::Operator::Lte => BinaryOperator::Lte,
                        epic_token::Operator::Gte => BinaryOperator::Gte,
                        epic_token::Operator::Eq => BinaryOperator::Eq,
                        epic_token::Operator::Neq => BinaryOperator::Neq,
                        _ => unreachable!(),
                    };
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op: bin_op,
                        right: Box::new(rhs),
                    }
                },
            ),
            infix(
                left(3),
                select! { epic_token::Token::Op(epic_token::Operator::Add) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::Add,
                    right: Box::new(rhs),
                },
            ),
            infix(
                left(3),
                select! { epic_token::Token::Op(epic_token::Operator::Subtract) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::Sub,
                    right: Box::new(rhs),
                },
            ),
            infix(
                left(4),
                select! { epic_token::Token::Op(epic_token::Operator::Multiply) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::Mul,
                    right: Box::new(rhs),
                },
            ),
            infix(
                left(4),
                select! { epic_token::Token::Op(epic_token::Operator::Divide) => {} },
                |lhs, _, rhs, _| Expr::BinaryOp {
                    left: Box::new(lhs),
                    op: BinaryOperator::Div,
                    right: Box::new(rhs),
                },
            ),
            prefix(
                5,
                select! { epic_token::Token::Op(epic_token::Operator::Subtract) => {} },
                |_, rhs, _| Expr::UnaryOp {
                    op: UnaryOperator::Neg,
                    expr: Box::new(rhs),
                },
            ),
            prefix(
                5,
                select! { epic_token::Token::Op(epic_token::Operator::Not) => {} },
                |_, rhs, _| Expr::UnaryOp {
                    op: UnaryOperator::Not,
                    expr: Box::new(rhs),
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
        ))
    });

    let statement = recursive(|statement| {
        let block = statement.repeated().collect::<Vec<_>>();

        let indented_block = block.delimited_by(
            just(epic_token::Token::Indent),
            just(epic_token::Token::Dedent),
        );

        let goto_statement = select! {
            epic_token::Token::NumberLiteral(line_num) => Statement::from(GotoStatement { line_number: line_num.parse().unwrap() })
        }
            .delimited_by(
                just([epic_token::Token::Goto, epic_token::Token::Line]),
                just(epic_token::Token::Newline)
            );

        let assignment_statement = select! { epic_token::Token::Identifier(ident) => ident }
            .then_ignore(select! {epic_token::Token::Op(epic_token::Operator::Assign) => {}})
            .then(expr.clone())
            .then_ignore(just(epic_token::Token::Newline))
            .map(|(identifier, expression)| {
                Statement::from(AssignmentStatement {
                    identifier,
                    expression,
                })
            });

        let swap_statement = select! { epic_token::Token::Identifier(ident) => ident }
            .then_ignore(select! {epic_token::Token::Op(epic_token::Operator::Swap) => {} })
            .then(select! { epic_token::Token::Identifier(ident) => ident })
            .then_ignore(just(epic_token::Token::Newline))
            .map(|(ident_1, ident_2)| Statement::from(SwapStatement { ident_1, ident_2 }));

        let if_statement = expr
            .clone()
            .delimited_by(
                just(epic_token::Token::If),
                just([epic_token::Token::Then, epic_token::Token::Newline]),
            )
            .then(indented_block.clone())
            .then(
                just(epic_token::Token::Else)
                    .ignore_then(just(epic_token::Token::Newline))
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
            .delimited_by(
                just(epic_token::Token::While),
                just([epic_token::Token::Do, epic_token::Token::Newline]),
            )
            .then(indented_block.clone())
            .map(|(condition, body)| Statement::from(WhileStatement { condition, body }));

        let for_statement = just(epic_token::Token::For)
            .ignore_then(select! { epic_token::Token::Identifier(loop_var) => loop_var })
            .then_ignore(select! { epic_token::Token::Op(epic_token::Operator::Assign) => {} })
            .then(expr.clone())
            .then_ignore(just(epic_token::Token::To))
            .then(expr.clone())
            .then_ignore(just([epic_token::Token::Do, epic_token::Token::Newline]))
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
            .delimited_by(
                just(epic_token::Token::Return),
                just(epic_token::Token::Newline),
            )
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
    let indented_block = block.clone().delimited_by(
        just(epic_token::Token::Indent),
        just(epic_token::Token::Dedent),
    );

    let procedure = just(epic_token::Token::Procedure)
        .ignore_then(select! { epic_token::Token::Identifier(name) => name })
        .then(
            select! { epic_token::Token::Identifier(param) => param }
                .separated_by(just(epic_token::Token::Comma))
                .collect::<Vec<_>>()
                .or_not()
                .delimited_by(
                    just(epic_token::Token::LRoundBracket),
                    just(epic_token::Token::RRoundBracket),
                )
                .map(|params| params.unwrap_or_default()),
        )
        .then_ignore(just([epic_token::Token::Colon, epic_token::Token::Newline]))
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

        let parser = parse_pseudocode_program();
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

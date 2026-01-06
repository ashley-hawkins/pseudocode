use chumsky::{
    input::ValueInput,
    pratt::{infix, left, postfix, prefix},
    prelude::*,
    span::Spanned,
};

use crate::epic_token::{self, Token};

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

#[derive(Debug, Clone, thiserror::Error)]
enum BinaryOperationFromTokenError<'a> {
    #[error("Token is not a binary operator: {0:?}")]
    NotABinaryOperator(Token<'a>),
}

impl<'a> TryFrom<Token<'a>> for BinaryOperator {
    type Error = BinaryOperationFromTokenError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Add => Ok(BinaryOperator::Add),
            Token::Subtract => Ok(BinaryOperator::Sub),
            Token::Multiply => Ok(BinaryOperator::Mul),
            Token::Divide => Ok(BinaryOperator::Div),
            Token::And => Ok(BinaryOperator::And),
            Token::Or => Ok(BinaryOperator::Or),
            Token::Lt => Ok(BinaryOperator::Lt),
            Token::Gt => Ok(BinaryOperator::Gt),
            Token::Lte => Ok(BinaryOperator::Lte),
            Token::Gte => Ok(BinaryOperator::Gte),
            Token::Eq => Ok(BinaryOperator::Eq),
            Token::Neq => Ok(BinaryOperator::Neq),
            _ => Err(BinaryOperationFromTokenError::NotABinaryOperator(value)),
        }
    }
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
        colon: Spanned<()>,
        end: Option<Box<Spanned<Expr<'a>>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum Expr<'a> {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    VariableAccess(&'a str),
    FunctionCall {
        left: Spanned<&'a str>,
        arguments: Spanned<Vec<Spanned<Expr<'a>>>>,
    },
    ArrayAccess {
        left: Box<Spanned<Expr<'a>>>,
        right: Spanned<ArrayIndex<'a>>,
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
    let variable_access =
        select! { epic_token::Token::Identifier(ident) => ident }.map(Expr::VariableAccess);

    let boolean_literal =
        select! { epic_token::Token::BoolLiteral(value) => value }.map(Expr::BooleanLiteral);

    let number_literal =
        select! { epic_token::Token::NumberLiteral(value) => value.parse().unwrap() }
            .map(Expr::NumberLiteral);

    let value = choice((boolean_literal, number_literal, variable_access));

    let expr = recursive(|expr| {
        let paren_expr = expr
            .clone()
            .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
            .map(|span: Spanned<Expr>| span.inner);

        let function_call = select! { epic_token::Token::Identifier(name) => name }
            .spanned()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LRoundBracket), just(Token::RRoundBracket))
                    .spanned(),
            )
            .map(|(name, args)| Expr::FunctionCall {
                left: name,
                arguments: args,
            });

        let atom = choice((
            paren_expr.spanned(),
            function_call.spanned(),
            value.spanned(),
        ));

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

        let fold_binary_operation = move |lhs,
                                     op: Spanned<Token>,
                                     rhs,
                                     extra: &mut chumsky::input::MapExtra<
            '_,
            '_,
            I,
            extra::Full<Rich<'_, Token<'_>>, (), ()>,
        >| Spanned {
            inner: Expr::BinaryOp {
                left: Box::new(lhs),
                op: Spanned {
                    inner: match BinaryOperator::try_from(op.inner) {
                        Ok(bin_op) => bin_op,
                        Err(_) => unreachable!(),
                    },
                    span: op.span,
                },
                right: Box::new(rhs),
            },
            span: extra.span(),
        };

        let or_operation = infix(left(0), just(Token::Or).spanned(), fold_binary_operation);
        let and_operation = infix(left(1), just(Token::And).spanned(), fold_binary_operation);
        let comparison_operations = infix(
            left(2),
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
            left(3),
            one_of([Token::Add, Token::Subtract]).spanned(),
            fold_binary_operation,
        );
        let mul_div_operation = infix(
            left(4),
            one_of([Token::Multiply, Token::Divide]).spanned(),
            fold_binary_operation,
        );
        let unary_operation = prefix(
            5,
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
        let array_access = postfix(6, array_index, |lhs, rhs, extra| Spanned {
            inner: Expr::ArrayAccess {
                left: Box::new(lhs),
                right: rhs,
            },
            span: extra.span(),
        });

        atom.pratt((
            or_operation,
            and_operation,
            comparison_operations,
            add_sub_operation,
            mul_div_operation,
            unary_operation,
            array_access,
        ))
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
            .then_ignore(just(epic_token::Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Newline))
            .map(|(identifier, expression)| {
                Statement::from(AssignmentStatement {
                    identifier,
                    expression,
                })
            });

        let swap_statement = select! { epic_token::Token::Identifier(ident) => ident }
            .then_ignore(just(epic_token::Token::Swap))
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
            .then_ignore(just(epic_token::Token::Assign))
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

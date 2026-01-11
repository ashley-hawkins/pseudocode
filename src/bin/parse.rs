use std::fs;
use std::path::PathBuf;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::error::RichReason;
use clap::Parser;
use pseudocode::{
    expr::{ArrayIndex, Expr},
    type_checker::{TypeErrorContext, ValidateTypes},
    util::SourceSpan,
};

/// Simple CLI: read a file and parse it, reporting parse errors with ariadne.
#[derive(Debug, Parser)]
#[command(name = "pseudocode-parse")]
struct Cli {
    /// Input file to parse
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let src = match fs::read_to_string(&cli.file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file {}: {}", cli.file.display(), e);
            std::process::exit(1);
        }
    };

    let file_name = cli.file.to_string_lossy().into_owned();

    // Parse using library parser
    let result = pseudocode::parser::parse_str(&src, pseudocode::parser::Mode::default());

    if let Some(ast) = result.output() {
        println!("{:#?}", ast);
        let type_errors = ast.validate_types();

        for e in type_errors {
            let report = Report::build(
                    ReportKind::Error,
                    (
                        file_name.clone(),
                        e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes,
                    ),
                )
                .with_message(format!(
                    "Type mismatch: expected {}, found {}",
                    e.expected, e.found
                ))
                .with_labels([
                    Label::new((
                        file_name.clone(),
                        e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes,
                    ))
                    .with_message(format!("expected type {}, but this expression results in type {}", e.expected, e.found))
                    .with_color(Color::Red),
                    Label::new((
                        file_name.clone(),
                        match &e.context {
                            TypeErrorContext::SubExprOf(context) => match &context.inner {
                                Expr::BinaryOp { op, .. } => op.span.start.bytes..op.span.end.bytes,
                                Expr::ArrayAccess { left, right } => right.span.start.bytes..right.span.end.bytes,
                                _ => context.span.start.bytes..context.span.end.bytes,
                            }
                            TypeErrorContext::IfStatementCond(span) => span.start.bytes..span.end.bytes,
                            TypeErrorContext::WhileStatementCond(span) => span.start.bytes..span.end.bytes,
                            TypeErrorContext::ForStatementRange(span) => span.start.bytes..span.end.bytes,
                            TypeErrorContext::Other => e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes,
                        },
                    ))
                    .with_color(Color::Yellow)
                    .with_message(match &e.context {
                        TypeErrorContext::SubExprOf(context) => match &context.inner {
                        Expr::BinaryOp { op, .. } => {
                            let op_name = op.inner.to_string();

                            format!(
                                "hint: operator '{}' expects operands of type {}",
                                op_name, e.expected
                            )
                        }
                        Expr::UnaryOp { op, .. } => {
                            format!(
                                "hint: unary operator '{}' expects operand of type {}",
                                op.inner, e.expected
                            )
                        }
                        Expr::FunctionCall { .. } => {
                            panic!("function args are dynamic so this code should never run")
                        }
                        Expr::ArrayAccess { left, right } => match &right.inner {
                            ArrayIndex::SingleIndex(idx) => {
                                if e.origin_expr.span == idx.span {
                                    format!(
                                        "hint: the right side of this subscript operation must be of type {}",
                                        e.expected
                                    )
                                } else if e.origin_expr.span == left.span {
                                    format!(
                                        "hint: the left side of this subscript operation must be of type {}",
                                        e.expected
                                    )
                                } else {
                                    panic!("type error context does not match any sub-expression")
                                }
                            }
                            ArrayIndex::Slice { start, end, .. } => {
                                if Some(e.origin_expr.span) == start.as_ref().map(|s| s.span)
                                    || Some(e.origin_expr.span) == end.as_ref().map(|s| s.span)
                                {
                                    format!(
                                        "hint: indices of this slice operation must be of type {}",
                                        e.expected
                                    )
                                } else if e.origin_expr.span == left.span {
                                    format!(
                                        "hint: the left side of this slice operation must be of type {}",
                                        e.expected
                                    )
                                } else {
                                    panic!("type error context does not match any sub-expression")
                                }
                            }
                        },
                        _ => panic!("type error context does not match any sub-expression"),
                                            }
                        TypeErrorContext::IfStatementCond(_) => format!("hint: the condition for this if statement must be of type {}", e.expected),
                        TypeErrorContext::WhileStatementCond(_) => format!("hint: the condition for this while loop must be of type {}", e.expected),
                        TypeErrorContext::ForStatementRange(_) => format!("hint: the bounds for this for loop must be of type {}", e.expected),
                        TypeErrorContext::Other => todo!(),
                    }),
                ])
                .finish();

            report
                .print(sources([(file_name.clone(), src.clone())]))
                .unwrap();
        }
    } else {
        // Use the debug representation of the parse result as a message and
        // print it with ariadne over the whole source range.

        for error in result.errors() {
            let into_range = |span: &SourceSpan| {
                if span.is_eof() {
                    src.len()..src.len()
                } else {
                    span.start.bytes + 1..span.end.bytes
                }
            };

            Report::build(
                ReportKind::Error,
                (file_name.clone(), into_range(error.span())),
            )
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(match error.reason() {
                x @ RichReason::ExpectedFound {
                    expected: _,
                    found: _,
                } => "Encountered unexpected token".to_string(),
                RichReason::Custom(s) => s.clone(),
            })
            .with_label(
                Label::new((file_name.clone(), into_range(error.span())))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .print(sources([(file_name.clone(), src.clone())]))
            .unwrap()
        }

        std::process::exit(1);
    }
}

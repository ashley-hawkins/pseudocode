use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::error::RichReason;
use pseudocode::{
    expr::{ArrayIndex, Expr},
    interpreter::RuntimeError,
    type_checker::{TypeError, TypeErrorContext},
};

pub fn output_type_errors<'a>(src: &'a str, file_name: String, type_errors: &[TypeError<'a>]) {
    for e in type_errors {
        let hint_span = (
            file_name.clone(),
            match &e.context {
                TypeErrorContext::SubExprOf(context) => match &context.inner {
                    Expr::BinaryOp { op, .. } => op.span.start.bytes..op.span.end.bytes,
                    Expr::ArrayAccess { left: _, right } => {
                        right.span.start.bytes..right.span.end.bytes
                    }
                    _ => context.span.start.bytes..context.span.end.bytes,
                },
                TypeErrorContext::IfStatementCond(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::WhileStatementCond(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::ForStatementRange(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::Other => {
                    e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes
                }
            },
        );

        let hint_message = match &e.context {
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
            },
            TypeErrorContext::IfStatementCond(_) => format!(
                "hint: the condition for this if statement must be of type {}",
                e.expected
            ),
            TypeErrorContext::WhileStatementCond(_) => format!(
                "hint: the condition for this while loop must be of type {}",
                e.expected
            ),
            TypeErrorContext::ForStatementRange(_) => format!(
                "hint: the bounds for this for loop must be of type {}",
                e.expected
            ),
            TypeErrorContext::Other => todo!(),
        };

        let hint = Label::new(hint_span)
            .with_color(Color::Yellow)
            .with_message(hint_message);

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
            .with_message(format!(
                "expected type {}, but this expression results in type {}",
                e.expected, e.found
            ))
            .with_color(Color::Red),
            hint,
        ])
        .finish();

        report.print(sources([(file_name.clone(), src)])).unwrap();
    }
}

pub fn output_parse_errors(src: &str, file_name: String, errors: &[&pseudocode::parser::Error]) {
    for error in errors {
        Report::build(
            ReportKind::Error,
            (file_name.clone(), error.span().into_range()),
        )
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(match error.reason() {
            RichReason::ExpectedFound { expected: _, found } => format!(
                "Encountered unexpected {}",
                match found {
                    Some(f) => format!("token {:?}", f),
                    None => "end of input".to_string(),
                }
            ),
            RichReason::Custom(s) => s.clone(),
        })
        .with_label(
            Label::new((file_name.clone(), error.span().into_range()))
                .with_message(error.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
        .print(sources([(file_name.clone(), src)]))
        .unwrap()
    }
}

pub fn output_runtime_error(src: &str, file_name: String, error: &RuntimeError) {
    match error {
        RuntimeError::TypeError {
            expected,
            found,
            span,
        } => {
            let report = Report::build(
                ReportKind::Error,
                (file_name.clone(), span.start.bytes..span.end.bytes),
            )
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(format!(
                "Type error: expected {}, found {}",
                expected, found
            ))
            .with_label(
                Label::new((file_name.clone(), span.start.bytes..span.end.bytes))
                    .with_message(format!(
                        "Expected {}, but the result type of this expression is {}",
                        expected, found
                    ))
                    .with_color(Color::Red),
            )
            .finish();

            report.print(sources([(file_name.clone(), src)])).unwrap();
        }
    }
}

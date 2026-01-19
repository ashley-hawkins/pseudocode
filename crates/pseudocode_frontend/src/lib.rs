use std::borrow::Borrow;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::{error::RichReason, span::WrappingSpan};
use pseudocode::{
    expr::{ArrayIndex, Expr},
    instruction::generate_instructions_for_ast,
    interpreter::{Environment, RuntimeError, run_program},
    parser::{AstRoot, Mode, parse_cmdline_assignment_from_str},
    statement::{Block, Statement},
    type_checker::{TypeError, TypeErrorContext, ValidateTypes},
    util::{SourceSpan, Spanned},
};

pub fn write_type_errors<'a>(
    src: &'a str,
    file_name: String,
    type_errors: &[impl Borrow<TypeError<'a>>],
    w: &mut impl std::io::Write,
) {
    for e in type_errors {
        let e = e.borrow();
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

        report
            .write(sources([(file_name.clone(), src)]), &mut *w)
            .unwrap();
    }
}

pub fn print_type_errors<'a>(
    src: &'a str,
    file_name: String,
    type_errors: &[impl Borrow<TypeError<'a>>],
) {
    write_type_errors(src, file_name, type_errors, &mut std::io::stdout());
}

pub fn eprint_type_errors<'a>(
    src: &'a str,
    file_name: String,
    type_errors: &[impl Borrow<TypeError<'a>>],
) {
    write_type_errors(src, file_name, type_errors, &mut std::io::stderr());
}

pub fn write_parse_errors<'src>(
    src: &'src str,
    file_name: String,
    errors: &[impl Borrow<pseudocode::parser::Error<'src>>],
    w: &mut impl std::io::Write,
) {
    for error in errors {
        let error = error.borrow();
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
        .write(sources([(file_name.clone(), src)]), &mut *w)
        .unwrap()
    }
}

pub fn print_parse_errors<'src>(
    src: &'src str,
    file_name: String,
    errors: &[impl Borrow<pseudocode::parser::Error<'src>>],
) {
    write_parse_errors(src, file_name, errors, &mut std::io::stdout());
}

pub fn eprint_parse_errors<'src>(
    src: &'src str,
    file_name: String,
    errors: &[impl Borrow<pseudocode::parser::Error<'src>>],
) {
    write_parse_errors(src, file_name, errors, &mut std::io::stderr());
}

pub fn write_runtime_error(
    src: &str,
    file_name: String,
    error: &RuntimeError,
    w: &mut impl std::io::Write,
) {
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

            report
                .write(sources([(file_name.clone(), src)]), &mut *w)
                .unwrap();
        }
    }
}

pub fn print_runtime_error(src: &str, file_name: String, error: &RuntimeError) {
    write_runtime_error(src, file_name, error, &mut std::io::stdout());
}

pub fn eprint_runtime_error(src: &str, file_name: String, error: &RuntimeError) {
    write_runtime_error(src, file_name, error, &mut std::io::stderr());
}

pub fn create_initial_environment(
    initializers: &[String],
    w: &mut impl std::io::Write,
) -> Option<Environment> {
    let initializers = initializers
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let result = parse_cmdline_assignment_from_str(s, i as u32);

            let errors = result.errors().collect::<Vec<_>>();

            write_parse_errors(s, format!("initializer {}", i + 1), &errors, &mut *w);

            result
                .into_output()
                .map(|spanned| spanned.span.make_wrapped(Statement::from(spanned.inner)))
        })
        .collect::<Option<Vec<_>>>()?;

    Some(if initializers.is_empty() {
        Environment::default()
    } else {
        let init_prog_ast = AstRoot {
            procedures: vec![],
            main_algorithm: Spanned {
                span: SourceSpan::eof(),
                inner: Block(initializers),
            },
        };

        run_program(&generate_instructions_for_ast(&init_prog_ast))
            .unwrap()
            .environment
    })
}

pub fn parse_source_to_ast<'src>(
    src: &'src str,
    file_name: String,
    mode: Mode,
    w: &mut impl std::io::Write,
) -> Option<AstRoot<'src>> {
    let result = pseudocode::parser::parse_program_from_str(src, mode);

    let ast = match result.into_result() {
        Ok(ast) => ast,
        Err(errors) => {
            write_parse_errors(src, file_name.clone(), &errors, w);

            return None;
        }
    };

    let type_errors = ast.validate_types();

    if !type_errors.is_empty() {
        write_type_errors(src, file_name, &type_errors, w);
        return None;
    }

    Some(ast)
}

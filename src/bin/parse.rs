use std::fs;
use std::path::PathBuf;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::error::RichReason;
use clap::Parser;
use pseudocode::{
    instruction::generate_instructions_for_ast,
    interpreter::RuntimeError,
    type_checker::{ValidateTypes, process_type_errors},
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
        process_type_errors(&src, file_name.clone(), &type_errors);
        if !type_errors.is_empty() {
            std::process::exit(1);
        }

        let program = generate_instructions_for_ast(ast);
        println!("{:#?}", program.into_iter().enumerate().collect::<Vec<_>>());

        match pseudocode::interpreter::run_program(&generate_instructions_for_ast(ast)) {
            Ok(value) => {
                println!("Program finished with value: {:#?}", value);
            }
            Err(e) => {
                match e {
                    RuntimeError::TypeError {
                        expected,
                        found,
                        span,
                    } => {
                        let report = Report::build(
                            ReportKind::Error,
                            (file_name.clone(), span.start.bytes..span.end.bytes),
                        )
                        .with_config(
                            ariadne::Config::new().with_index_type(ariadne::IndexType::Byte),
                        )
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
                            .print(sources([(file_name.clone(), src.clone())]))
                            .unwrap();
                    }
                }
                std::process::exit(1);
            }
        }
    } else {
        // Use the debug representation of the parse result as a message and
        // print it with ariadne over the whole source range.

        for error in result.errors() {
            let into_range = |span: &SourceSpan| {
                if span.is_eof() {
                    src.len()..src.len()
                } else {
                    span.start.bytes..span.end.bytes
                }
            };

            Report::build(
                ReportKind::Error,
                (file_name.clone(), into_range(error.span())),
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

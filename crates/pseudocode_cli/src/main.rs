pub mod error_handling;

use std::path::PathBuf;
use std::{fs, process::ExitCode};

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::error::RichReason;
use clap::Parser;
use pseudocode::{
    instruction::generate_instructions_for_ast, interpreter::RuntimeError,
    type_checker::ValidateTypes, util::SourceSpan,
};

use crate::error_handling::{output_parse_errors, output_runtime_error, output_type_errors};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum ParseMode {
    Jumpy,
    Structured,
    Procedural,
}

#[derive(Debug, Parser)]
#[command(name = "pseudocode-parse")]
struct Cli {
    file: PathBuf,
    #[arg(short, long, value_enum, default_value_t = ParseMode::Procedural)]
    mode: ParseMode,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let src = match fs::read_to_string(&cli.file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file {}: {}", cli.file.display(), e);
            return ExitCode::FAILURE;
        }
    };

    let file_name = cli.file.to_string_lossy().into_owned();

    let result = pseudocode::parser::parse_str(
        &src,
        match cli.mode {
            ParseMode::Jumpy => pseudocode::parser::Mode::JumpyImp,
            ParseMode::Structured => pseudocode::parser::Mode::StructuredImp,
            ParseMode::Procedural => pseudocode::parser::Mode::ProceduralImp,
        },
    );

    let Some(ast) = result.output() else {
        let errors = result.errors().collect::<Vec<_>>();
        output_parse_errors(&src, file_name.clone(), &errors);

        return ExitCode::FAILURE;
    };

    println!("{:#?}", ast);

    let type_errors = ast.validate_types();

    if !type_errors.is_empty() {
        output_type_errors(&src, file_name.clone(), &type_errors);
        return ExitCode::FAILURE;
    }

    let program = generate_instructions_for_ast(ast);
    println!("{:#?}", program.into_iter().enumerate().collect::<Vec<_>>());

    match pseudocode::interpreter::run_program(&generate_instructions_for_ast(ast)) {
        Ok(value) => {
            println!("Program finished with value: {:#?}", value);
        }
        Err(e) => {
            output_runtime_error(&src, file_name.clone(), &e);
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}

pub mod error_handling;

use std::path::PathBuf;
use std::{fs, process::ExitCode};

use chumsky::span::WrappingSpan;
use clap::Parser;
use pseudocode::interpreter::{Environment, run_program, run_program_with_environment};
use pseudocode::parser::{AstRoot, parse_cmdline_assignment_from_str};
use pseudocode::statement::{Block, Statement};
use pseudocode::util::Spanned;
use pseudocode::{
    instruction::generate_instructions_for_ast, type_checker::ValidateTypes, util::SourceSpan,
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

    #[arg(short, long, value_enum)]
    mode: Option<ParseMode>,

    initializers: Vec<String>,
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

    let mode = match cli.mode {
        Some(m) => m,
        None => {
            let ext = cli.file.extension().map(|s| s.to_string_lossy());
            match ext.as_ref().map(|s| s.as_ref()) {
                Some("ji") | Some("jumpyimp") => ParseMode::Jumpy,
                Some("si") | Some("structimp") | Some("structuredimp") => ParseMode::Structured,
                Some("pi")
                | Some("procimp")
                | Some("proceduralimp")
                | Some("ri")
                | Some("recimp")
                | Some("recursiveimp") => ParseMode::Procedural,
                _ => {
                    println!(
                        "Warning: Could not determine pseudocode variant of input file. Please specify explicitly with --mode. Falling back to ProceduralImp."
                    );
                    ParseMode::Procedural
                }
            }
        }
    };

    let file_name = cli.file.to_string_lossy().into_owned();

    let Some(initializers) = cli
        .initializers
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let result = parse_cmdline_assignment_from_str(s, i as u32);

            let errors = result.errors().collect::<Vec<_>>();

            output_parse_errors(s, format!("initializer {}", i + 1), &errors);

            result
                .into_output()
                .map(|spanned| spanned.span.make_wrapped(Statement::from(spanned.inner)))
        })
        .collect::<Option<Vec<_>>>()
    else {
        return ExitCode::FAILURE;
    };

    let environment = match initializers.is_empty() {
        true => Environment::default(),
        false => {
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
        }
    };

    let initial_environment = environment;

    println!("Initial environment: {:#?}", initial_environment);

    let result = pseudocode::parser::parse_program_from_str(
        &src,
        match mode {
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

    match run_program_with_environment(&generate_instructions_for_ast(ast), initial_environment) {
        Ok(result) => {
            println!("Program finished with value: {}", result.return_value);
        }
        Err(e) => {
            output_runtime_error(&src, file_name.clone(), &e);
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}

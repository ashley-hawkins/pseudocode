use std::path::PathBuf;
use std::{fs, process::ExitCode};

use clap::Parser;
use pseudocode::instruction::generate_instructions_for_ast;
use pseudocode::interpreter::run_program_with_environment;
use pseudocode_frontend::{create_initial_environment, eprint_runtime_error};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum ParseMode {
    Jumpy,
    Structured,
    Procedural,
}

#[derive(Debug, Parser)]
struct Cli {
    file: PathBuf,

    #[arg(long, default_value_t = false)]
    debug: bool,

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

    let Some(initial_environment) =
        create_initial_environment(&cli.initializers, &mut std::io::stdout())
    else {
        return ExitCode::FAILURE;
    };

    if cli.debug {
        println!("Initial environment:");
        println!("{:#?}", initial_environment);
    }

    let Some(ast) = pseudocode_frontend::parse_source_to_ast(
        &src,
        file_name.clone(),
        match mode {
            ParseMode::Jumpy => pseudocode::parser::Mode::JumpyImp,
            ParseMode::Structured => pseudocode::parser::Mode::StructuredImp,
            ParseMode::Procedural => pseudocode::parser::Mode::ProceduralImp,
        },
        &mut std::io::stdout(),
    ) else {
        return ExitCode::FAILURE;
    };

    if cli.debug {
        println!("Parsed AST:");
        println!("{:#?}", ast);
    }

    let program = generate_instructions_for_ast(&ast);
    if cli.debug {
        println!("Generated instructions:");
        println!("{:#?}", program.iter().enumerate().collect::<Vec<_>>());
    }

    match run_program_with_environment(&program, initial_environment) {
        Ok(result) => {
            println!("Program finished with value: {}", result.return_value);
        }
        Err(e) => {
            eprint_runtime_error(&src, file_name.clone(), &e);
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}

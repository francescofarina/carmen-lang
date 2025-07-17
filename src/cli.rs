//! Command-line interface.
//!
//! This module provides the main CLI entry point and command handlers for:
//! - Running Carmen scripts
//! - Interactive REPL mode
//! - Parsing and tokenizing files
//! - AST inspection
//! - Exporting scores to various formats (text, LilyPond)

use crate::{
    errors::{reporter, ErrorSource},
    parse, run, tokenize, ExportConfig, ExportFormat, ExportManager, ExportOutput, Value,
};
use std::io::{self};

/// Main CLI entry point that handles all command-line operations.
///
/// This function parses command-line arguments and delegates to appropriate handlers.
/// If any operation fails, it prints an error message and exits with code 1.
pub fn run_cli() {
    if let Err(e) = run_cli_app() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

/// Parses command-line arguments and routes to appropriate functionality.
///
/// Supported modes:
/// - No args: Start REPL
/// - 1 arg: Run Carmen file
/// - 2 args: Parse, tokenize, or inspect operations
/// - 3+ args: Inspect with output file or export operations
fn run_cli_app() -> Result<(), ErrorSource> {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => {
            crate::repl::repl();
            Ok(())
        }
        2 => {
            let filename = &args[1];
            run_file(filename)
        }
        3 => match args[1].as_str() {
            "--parse" => {
                let filename = &args[2];
                parse_file(filename)
            }
            "--tokenize" => {
                let filename = &args[2];
                tokenize_file(filename)
            }
            "--inspect" => {
                let filename = &args[2];
                inspect_file(filename, None)
            }
            _ => {
                print_usage(&args[0]);
                Ok(())
            }
        },
        4 => match args[1].as_str() {
            "--inspect" => {
                let input_file = &args[2];
                let output_file = &args[3];
                inspect_file(input_file, Some(output_file))
            }
            "--export" => {
                let format = &args[2];
                let input_file = &args[3];
                export_file(format, input_file, None)
            }
            _ => {
                print_usage(&args[0]);
                Ok(())
            }
        },
        5 => match args[1].as_str() {
            "--export" => {
                let format = &args[2];
                let input_file = &args[3];
                let output_file = &args[4];
                export_file(format, input_file, Some(output_file))
            }
            _ => {
                print_usage(&args[0]);
                Ok(())
            }
        },
        _ => {
            print_usage(&args[0]);
            Ok(())
        }
    }
}

/// Prints comprehensive usage information for the CLI.
fn print_usage(program_name: &str) {
    eprintln!("Usage: {program_name} [options] [file]");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --parse <file>              Parse file and show AST");
    eprintln!("  --tokenize <file>           Tokenize file and show tokens");
    eprintln!("  --inspect <file>            Inspect AST (output to stdout)");
    eprintln!("  --inspect <file> <output>   Inspect AST (output to file)");
    eprintln!("  --export <format> <file>    Export to stdout");
    eprintln!("  --export <format> <file> <output>  Export to file");
    eprintln!();
    eprintln!("Export formats:");
    eprintln!("  text                        Plain text format");
    eprintln!("  lilypond, ly                LilyPond notation format");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  {program_name} script.carmen             Run carmen script");
    eprintln!("  {program_name} --export text score.carmen Export score as text");
    eprintln!("  {program_name} --export text score.carmen output.txt  Export to file");
    eprintln!("  {program_name} --export lilypond score.carmen Export score as LilyPond");
}

/// Reads the contents of a file and returns it as a string.
///
/// # Arguments
/// * `filename` - Path to the file to read
///
/// # Returns
/// * `Ok(String)` - File contents as a string
/// * `Err(ErrorSource)` - IO error wrapped in CLI error
fn read_file_source(filename: &str) -> Result<String, ErrorSource> {
    std::fs::read_to_string(filename)
        .map_err(|e| ErrorSource::Cli(format!("Error reading file '{filename}': {e}")))
}

/// Executes a Carmen script file and prints the result.
///
/// # Arguments
/// * `filename` - Path to the Carmen script file to execute
fn run_file(filename: &str) -> Result<(), ErrorSource> {
    let source = read_file_source(filename)?;

    match run(&source) {
        Ok(value) => {
            if !matches!(value, Value::Nil) {
                println!("{value}");
            }
            Ok(())
        }
        Err(error) => {
            reporter::report(&mut io::stderr(), &source, &error, filename).unwrap();
            Err(ErrorSource::Cli("".to_string()))
        }
    }
}

/// Parses a Carmen file and prints the resulting AST in debug format.
///
/// # Arguments
/// * `filename` - Path to the Carmen file to parse
fn parse_file(filename: &str) -> Result<(), ErrorSource> {
    let source = read_file_source(filename)?;

    match parse(&source) {
        Ok(program) => {
            println!("{program:#?}");
            Ok(())
        }
        Err(error) => {
            reporter::report(&mut io::stderr(), &source, &error, filename).unwrap();
            Err(ErrorSource::Cli("".to_string()))
        }
    }
}

/// Tokenizes a Carmen file and prints each token in debug format.
///
/// # Arguments
/// * `filename` - Path to the Carmen file to tokenize
fn tokenize_file(filename: &str) -> Result<(), ErrorSource> {
    let source = read_file_source(filename)?;

    match tokenize(&source) {
        Ok(tokens) => {
            for token in tokens {
                println!("{token:?}");
            }
            Ok(())
        }
        Err(error) => {
            reporter::report(&mut io::stderr(), &source, &error, filename).unwrap();
            Err(ErrorSource::Cli("".to_string()))
        }
    }
}

/// Inspects the AST of a Carmen file and outputs detailed analysis.
///
/// # Arguments
/// * `filename` - Path to the Carmen file to inspect
/// * `output_file` - Optional path to write inspection output (uses stdout if None)
fn inspect_file(filename: &str, output_file: Option<&str>) -> Result<(), ErrorSource> {
    let source = read_file_source(filename)?;

    let program = match parse(&source) {
        Ok(program) => program,
        Err(error) => {
            reporter::report(&mut io::stderr(), &source, &error, filename).unwrap();
            return Err(ErrorSource::Cli("".to_string()));
        }
    };

    match output_file {
        Some(output_path) => match crate::inspect_ast_to_file(&program, output_path) {
            Ok(()) => {
                println!("AST inspection saved to: {output_path}");
                println!(
                    "Analyzed {} statements and {} comments",
                    program.statements.len(),
                    program.comments.len()
                );
                Ok(())
            }
            Err(error) => Err(ErrorSource::Cli(format!(
                "Error writing to file '{output_path}': {error}"
            ))),
        },
        None => {
            let inspection = crate::inspect_ast(&program);
            println!("{inspection}");
            Ok(())
        }
    }
}

/// Exports a Carmen score to various formats.
///
/// The input file must evaluate to a Score value when interpreted.
///
/// # Arguments
/// * `format_str` - Export format ("text", "lilypond", or "ly")
/// * `input_file` - Path to the Carmen file containing the score
/// * `output_file` - Optional output file path (uses stdout if None)
fn export_file(
    format_str: &str,
    input_file: &str,
    output_file: Option<&str>,
) -> Result<(), ErrorSource> {
    // Parse and validate the export format
    let format = match format_str.to_lowercase().as_str() {
        "text" => ExportFormat::Text,
        "lilypond" | "ly" => ExportFormat::LilyPond,
        _ => {
            return Err(ErrorSource::Cli(format!(
                "Unsupported export format '{format_str}'"
            )));
        }
    };

    // Read the source file
    let source = read_file_source(input_file)?;

    // Interpret the source code and extract the resulting score
    let mut interpreter = crate::Interpreter::new();
    let score = match crate::run_with_interpreter(&mut interpreter, &source) {
        Ok(Value::Score(score)) => score,
        Ok(other_value) => {
            return Err(ErrorSource::Cli(format!(
                "Input does not produce a score, got: {}",
                other_value.type_name()
            )));
        }
        Err(error) => {
            reporter::report(&mut io::stderr(), &source, &error, input_file).unwrap();
            return Err(ErrorSource::Cli("".to_string()));
        }
    };

    // Configure the export operation
    let output = match output_file {
        Some(path) => ExportOutput::File(path.to_string()),
        None => ExportOutput::Stdout,
    };

    let config = ExportConfig { format, output };

    // Perform the export using the configured manager
    let manager = ExportManager::new();
    if let Err(error) = manager.export_score(&score, &config) {
        return Err(ErrorSource::Cli(format!(
            "Error during export: {}",
            error.source
        )));
    }

    // Print success message if exporting to file
    if let Some(output_path) = output_file {
        println!("Successfully exported to: {output_path}");
    }

    Ok(())
}

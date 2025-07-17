//! # Carmen - Programmatic Music Composition Language
//!
//! Carmen is a novel, programmatic language designed for music composition. This library provides
//! the core functionality to parse, interpret, and execute Carmen source code, as well as export
//! compositions to various formats.
//!
//! ## Features
//!
//! - **Lexical Analysis**: Tokenize Carmen source code into a stream of tokens
//! - **Parsing**: Parse tokens into an Abstract Syntax Tree (AST)
//! - **Interpretation**: Execute Carmen programs and evaluate musical expressions
//! - **Export**: Convert compositions to formats like LilyPond for sheet music generation

pub mod ast;
pub mod cli;
pub mod common;
pub mod core;
pub mod errors;
pub mod exporter;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod repl;

use ast::inspector::{inspect_ast, inspect_ast_to_file};
use errors::Result;
use exporter::{ExportConfig, ExportFormat, ExportManager, ExportOutput};
use interpreter::{Interpreter, Value};
use lexer::{Lexer, Token};
use parser::Parser;

/// Executes Carmen source code and returns the resulting value.
///
/// This is the main entry point for running Carmen programs. It performs the complete
/// compilation and execution pipeline: lexical analysis, parsing, and interpretation.
///
/// # Arguments
///
/// * `source` - A string slice containing the Carmen source code to execute
///
/// # Returns
///
/// Returns a `Result<Value>` where:
/// - `Ok(Value)` contains the result of executing the Carmen program
/// - `Err(CarmenError)` contains any compilation or runtime errors
///
/// # Errors
///
/// This function will return an error if:
/// - The source code contains lexical errors (invalid tokens)
/// - The source code contains syntax errors (invalid grammar)
/// - The program encounters runtime errors during execution
pub fn run(source: &str) -> Result<Value> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let parser = Parser::new(tokens);
    let program = parser.parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program)
}

/// Executes Carmen source code using an existing interpreter instance.
///
/// This function allows you to maintain state between multiple Carmen program executions
/// by reusing the same interpreter. This is useful for REPL environments or when you want
/// to execute multiple related programs that share variables and functions.
///
/// # Arguments
///
/// * `interpreter` - A mutable reference to an existing `Interpreter` instance
/// * `source` - A string slice containing the Carmen source code to execute
///
/// # Returns
///
/// Returns a `Result<Value>` where:
/// - `Ok(Value)` contains the result of executing the Carmen program
/// - `Err(CarmenError)` contains any compilation or runtime errors
///
/// # Errors
///
/// This function will return an error if:
/// - The source code contains lexical errors (invalid tokens)
/// - The source code contains syntax errors (invalid grammar)
/// - The program encounters runtime errors during execution
pub fn run_with_interpreter(interpreter: &mut Interpreter, source: &str) -> Result<Value> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let parser = Parser::new(tokens);
    let program = parser.parse()?;

    interpreter.interpret(&program)
}

/// Parses Carmen source code into an Abstract Syntax Tree (AST).
///
/// This function performs lexical analysis and parsing without executing the code.
/// It's useful for code analysis, debugging, or when you want to inspect the structure
/// of a Carmen program before execution.
///
/// # Arguments
///
/// * `source` - A string slice containing the Carmen source code to parse
///
/// # Returns
///
/// Returns a `Result<ast::Program>` where:
/// - `Ok(ast::Program)` contains the parsed AST representing the program structure
/// - `Err(CarmenError)` contains any lexical or syntax errors
///
/// # Errors
///
/// This function will return an error if:
/// - The source code contains invalid tokens
/// - The source code contains syntax errors
pub fn parse(source: &str) -> Result<ast::Program> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let parser = Parser::new(tokens);
    parser.parse()
}

/// Tokenizes Carmen source code into a vector of tokens.
///
/// This function performs lexical analysis on the source code, converting it into
/// a sequence of tokens that can be consumed by the parser. It's useful for debugging
/// lexical issues or understanding how Carmen code is tokenized.
///
/// # Arguments
///
/// * `source` - A string slice containing the Carmen source code to tokenize
///
/// # Returns
///
/// Returns a `Result<Vec<Token>>` where:
/// - `Ok(Vec<Token>)` contains the sequence of tokens representing the source code
/// - `Err(CarmenError)` contains any lexical errors
///
/// # Errors
///
/// This function will return an error if:
/// - The source code contains invalid characters or token sequences
/// - String literals are not properly terminated
/// - Numeric literals are malformed
pub fn tokenize(source: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

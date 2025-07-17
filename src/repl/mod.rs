//! Carmen REPL Module
//!
//! This module provides an interactive Read-Eval-Print Loop (REPL) for the Carmen
//! music programming language. The REPL offers a rich interactive environment with:
//!
//! - **Code Execution**: Execute Carmen expressions and statements interactively
//! - **Variable Inspection**: Detailed examination of musical objects and their structure
//! - **Visualization**: Piano roll display of musical scores, parts, and timelines
//! - **Auto-completion**: Context-aware completion for commands and variable names
//! - **History**: Persistent command history across sessions
//! - **Syntax Highlighting**: Bracket matching and formatting
//!
//! ## REPL Commands
//!
//! The REPL supports several special commands:
//! - `\help` - Show help information
//! - `\clear` - Clear the screen
//! - `\quit` - Exit the REPL
//! - `\view <variable>` - Visualize a musical object as a piano roll
//! - `\inspect <variable>` - Show detailed structure of a variable
//!
//! ## Module Structure
//!
//! - `command` - Command parsing and representation
//! - `engine` - Main REPL loop and execution engine
//! - `helper` - Auto-completion, highlighting, and input assistance
//! - `ui` - User interface functions and formatting
//! - `visualize` - Piano roll visualization for musical objects

mod command;
mod engine;
mod helper;
mod ui;
mod visualize;

use engine::Repl;

/// Starts the Carmen language REPL with full interactive features.
///
/// This function initializes and runs the complete REPL environment, including:
/// - Command line editing with history
/// - Auto-completion for commands and variables
/// - Syntax highlighting and bracket matching
/// - Error reporting and graceful error handling
///
/// The REPL will continue running until the user exits with `\quit`, Ctrl+C, or Ctrl+D.
/// Command history is automatically saved and restored between sessions.
///
/// # Errors
///
/// If REPL initialization fails (e.g., due to terminal compatibility issues),
/// an error message is displayed but the program doesn't panic.
pub fn repl() {
    match Repl::new() {
        Ok(mut repl) => repl.run(),
        Err(err) => ui::print_error(&format!("[REPL Error] {err}")),
    }
}

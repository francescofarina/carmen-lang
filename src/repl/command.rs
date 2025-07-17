use rustyline::error::ReadlineError;
use rustyline::{history::History, Editor, Helper};

use super::ui;

/// Represents a command entered by the user in the REPL.
///
/// Commands starting with backslash (\) are REPL-specific commands,
/// while other input is treated as Carmen language code to execute.
pub enum ReplCommand {
    /// Show help information about available REPL commands
    Help,
    /// Clear the terminal screen and redisplay the welcome message
    Clear,
    /// Exit the REPL
    Quit,
    /// Visualize a musical object (Score, Part, Timeline, or MusicalEvent) as a piano roll
    View(String),
    /// Display detailed inspection of a variable's structure and content
    Inspect(String),
    /// Execute Carmen language code and display the result
    Execute(String),
    /// Empty input (user pressed enter without typing anything)
    Empty,
}

/// Reads and parses a single command from the user.
///
/// This function handles the interactive input loop, parsing REPL commands
/// that start with backslash (\) and treating everything else as Carmen code
/// to execute. It also handles readline errors and interrupts gracefully.
///
/// # Arguments
/// * `editor` - The rustyline editor instance that handles input, history, and completion
///
/// # Returns
/// A `ReplCommand` representing the parsed user input
pub fn read_command<H: Helper, I: History>(editor: &mut Editor<H, I>) -> ReplCommand {
    let prompt = ui::prompt();
    match editor.readline(&prompt) {
        Ok(line) => {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                ReplCommand::Empty
            } else {
                match trimmed {
                    "\\help" => ReplCommand::Help,
                    "\\clear" => ReplCommand::Clear,
                    "\\quit" => ReplCommand::Quit,
                    // Parse \view command with variable name argument
                    s if s.starts_with("\\view") => {
                        let var_name = s.trim_start_matches("\\view").trim();
                        if var_name.is_empty() {
                            ui::print_error("Usage: \\view <variable>");
                            ReplCommand::Empty
                        } else {
                            ReplCommand::View(var_name.to_string())
                        }
                    }
                    // Parse \inspect command with variable name argument
                    s if s.starts_with("\\inspect") => {
                        let var_name = s.trim_start_matches("\\inspect").trim();
                        if var_name.is_empty() {
                            ui::print_error("Usage: \\inspect <variable>");
                            ReplCommand::Empty
                        } else {
                            ReplCommand::Inspect(var_name.to_string())
                        }
                    }
                    // Anything else is treated as Carmen code to execute
                    _ => ReplCommand::Execute(trimmed.to_string()),
                }
            }
        }
        // Handle Ctrl+C and Ctrl+D as quit signals
        Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => ReplCommand::Quit,
        Err(err) => {
            ui::print_error(&format!("Error reading line: {err}"));
            ReplCommand::Quit
        }
    }
}

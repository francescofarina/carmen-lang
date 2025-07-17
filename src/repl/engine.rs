use rustyline::history::DefaultHistory;
use rustyline::Editor;
use std::cell::RefCell;
use std::rc::Rc;

use crate::core::{Part, Score, Timeline};
use crate::errors::reporter;
use crate::errors::Result;
use crate::interpreter::{Interpreter, Value};
use crate::run_with_interpreter;

use super::command::{self, ReplCommand};
use super::helper::ReplHelper;
use super::ui;
use super::visualize;

/// The main REPL structure for the Carmen language.
///
/// This structure manages the interactive environment where users can:
/// - Execute Carmen language expressions and statements
/// - Inspect variables and their contents
/// - Visualize musical objects as piano rolls
/// - Navigate command history and get auto-completion
///
/// The REPL maintains its own interpreter instance and command history,
/// providing a persistent environment for interactive music programming.
pub struct Repl {
    /// The rustyline editor that handles input, completion, and history
    editor: Editor<ReplHelper, DefaultHistory>,
    /// Shared interpreter instance that maintains variable state across commands
    interpreter: Rc<RefCell<Interpreter>>,
    /// Path to the file where command history is persisted
    history_file: String,
}

impl Repl {
    /// Creates a new REPL instance with editor, interpreter, and history setup.
    ///
    /// This initializes:
    /// - A rustyline editor with Carmen-specific helper for completion and highlighting
    /// - A fresh interpreter instance for executing Carmen code
    /// - Command history loaded from disk (if available)
    ///
    /// # Returns
    /// - `Ok(Repl)` if initialization succeeds
    /// - `Err(String)` if editor initialization fails
    pub fn new() -> std::result::Result<Self, String> {
        let mut editor: Editor<ReplHelper, DefaultHistory> =
            Editor::new().map_err(|e| format!("Failed to initialize editor: {e}"))?;
        let history_file = "carmen_history.txt";
        if editor.load_history(history_file).is_err() {
            // History file probably doesn't exist yet, which is fine for first run.
        }

        let interpreter = Rc::new(RefCell::new(Interpreter::new()));
        let helper = ReplHelper::new(interpreter.clone());
        editor.set_helper(Some(helper));

        Ok(Self {
            editor,
            interpreter,
            history_file: history_file.to_string(),
        })
    }

    /// Runs the main REPL loop until the user exits.
    ///
    /// This is the core interactive loop that:
    /// 1. Displays the welcome message
    /// 2. Reads and parses user commands
    /// 3. Executes appropriate actions (help, clear, view, inspect, or code execution)
    /// 4. Handles errors gracefully
    /// 5. Saves command history on exit
    pub fn run(&mut self) {
        ui::print_welcome();

        loop {
            match command::read_command(&mut self.editor) {
                ReplCommand::Quit => break,
                ReplCommand::Help => ui::print_help(),
                ReplCommand::Clear => {
                    ui::clear_screen();
                    ui::print_welcome();
                }
                ReplCommand::View(var_name) => self.view(&var_name),
                ReplCommand::Inspect(var_name) => self.inspect(&var_name),
                ReplCommand::Empty => continue,
                ReplCommand::Execute(line) => {
                    let _ = self.editor.add_history_entry(&line);
                    self.execute(&line);
                }
            }
        }

        self.save_history();
        ui::print_goodbye();
    }

    /// Displays detailed inspection of a variable's structure and content.
    ///
    /// This provides a hierarchical view of complex musical objects,
    /// showing their internal structure, nested components, and metadata.
    ///
    /// # Arguments
    /// * `var_name` - The name of the variable to inspect
    fn inspect(&self, var_name: &str) {
        let interpreter = self.interpreter.borrow();
        if let Some(value) = interpreter.get_variable(var_name) {
            ui::print_inspected_value(&value);
        } else {
            ui::print_error(&format!("Undefined variable '{var_name}'"));
        }
    }

    /// Visualizes a musical object as a piano roll display.
    ///
    /// This function converts various Carmen musical types into a visual
    /// piano roll representation. It handles:
    /// - `Score`: Direct visualization
    /// - `Part`: Wrapped in a minimal Score for display
    /// - `Timeline`: Wrapped in a minimal Score for display
    /// - `MusicalEvent`: Wrapped in a Part->Timeline->Score hierarchy
    ///
    /// Non-musical types will show an error message.
    ///
    /// # Arguments
    /// * `var_name` - The name of the variable to visualize
    fn view(&self, var_name: &str) {
        let interpreter = self.interpreter.borrow();
        if let Some(value) = interpreter.get_variable(var_name) {
            match &value {
                Value::Score(score) => visualize::print_score(score),
                Value::Part(part) => {
                    let mut score = Score::default();
                    let mut timeline = Timeline::default();
                    timeline.add_part((*part).clone());
                    score.set_timeline(timeline);
                    visualize::print_score(&score);
                }
                Value::Timeline(timeline) => {
                    let mut score = Score::default();
                    score.set_timeline((*timeline).clone());
                    visualize::print_score(&score);
                }
                Value::MusicalEvent(event) => {
                    let mut score = Score::default();
                    let mut part = Part::default();
                    part.add_event((*event).clone());
                    let mut timeline = Timeline::default();
                    timeline.add_part(part);
                    score.set_timeline(timeline);
                    visualize::print_score(&score);
                }
                _ => ui::print_error(&format!(
                    "Cannot visualize value of type '{}'",
                    value.type_name()
                )),
            }
        } else {
            ui::print_error(&format!("Undefined variable '{var_name}'"));
        }
    }

    /// Executes a line of Carmen language code and displays the result.
    ///
    /// This function handles both expressions and statements:
    /// - Expressions (without semicolon): Result is printed
    /// - Statements (with semicolon): Executed silently unless there's an error
    ///
    /// Errors are formatted and displayed with context information.
    ///
    /// # Arguments
    /// * `line` - The Carmen code to execute
    fn execute(&mut self, line: &str) {
        match run_repl_line(&mut self.interpreter.borrow_mut(), line) {
            Ok((value, should_print)) => {
                if should_print && !matches!(value, Value::Nil) {
                    ui::print_value(&value);
                }
            }
            Err(error) => {
                let mut buffer = Vec::new();
                reporter::report(&mut buffer, line, &error, "<stdin>").unwrap();
                ui::print_error(&String::from_utf8_lossy(&buffer));
            }
        }
    }

    /// Saves the current command history to disk for persistence across sessions.
    ///
    /// This ensures that command history is available when the REPL is restarted.
    /// Failures to save are reported as warnings but don't prevent REPL exit.
    fn save_history(&mut self) {
        if let Err(err) = self.editor.save_history(&self.history_file) {
            ui::print_error(&format!("Warning: Could not save history: {err}"));
        }
    }
}

/// Parses and executes a single line of Carmen code in REPL context.
///
/// This function implements REPL-specific behavior for handling user input:
/// - Lines ending with semicolon are treated as statements (no output)
/// - Lines without semicolon are treated as expressions (result is printed)
///
/// This allows users to quickly evaluate expressions without explicit printing,
/// while still supporting full statement syntax when desired.
///
/// # Arguments
/// * `interpreter` - The interpreter instance to execute the code
/// * `source` - The Carmen source code to execute
///
/// # Returns
/// * `Ok((value, should_print))` - The result value and whether it should be displayed
/// * `Err(error)` - Compilation or runtime error
fn run_repl_line(interpreter: &mut Interpreter, source: &str) -> Result<(Value, bool)> {
    let trimmed = source.trim();
    let has_semicolon = trimmed.ends_with(';');

    if has_semicolon {
        // Statement: execute but don't print result
        let result = run_with_interpreter(interpreter, trimmed)?;
        Ok((result, false))
    } else {
        // Expression: add semicolon and print result
        let expr_source = format!("{trimmed};");
        let result = run_with_interpreter(interpreter, &expr_source)?;
        Ok((result, true))
    }
}

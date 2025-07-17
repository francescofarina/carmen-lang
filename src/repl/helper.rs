use std::borrow::Cow::{self, Borrowed, Owned};
use std::cell::RefCell;
use std::rc::Rc;

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Context, Helper};

use crate::interpreter::Interpreter;

/// Helper struct that provides completion, highlighting, hinting, and validation
/// for the Carmen REPL using the rustyline library.
///
/// This integrates with rustyline to provide an enhanced interactive experience:
/// - Auto-completion for REPL commands and variable names
/// - Syntax highlighting with bracket matching
/// - Command history hints
/// - Input validation
pub struct ReplHelper {
    /// Shared interpreter instance used to get available variables for completion
    pub interpreter: Rc<RefCell<Interpreter>>,
    /// Provides bracket matching highlighting (parentheses, braces, etc.)
    highlighter: MatchingBracketHighlighter,
    /// Provides hints based on command history
    hinter: HistoryHinter,
}

impl ReplHelper {
    /// Creates a new REPL helper with the given interpreter reference.
    ///
    /// # Arguments
    /// * `interpreter` - Shared reference to the interpreter for variable completion
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        Self {
            interpreter,
            highlighter: MatchingBracketHighlighter::new(),
            hinter: HistoryHinter {},
        }
    }

    /// Finds the word boundary for completion at the cursor position.
    ///
    /// This identifies the start of the current word being typed, considering
    /// alphanumeric characters, underscores, and backslashes (for REPL commands)
    /// as valid word characters.
    ///
    /// # Arguments
    /// * `line` - The complete input line
    /// * `pos` - The cursor position within the line
    ///
    /// # Returns
    /// A tuple of (word_start_position, word_text)
    fn find_word_to_complete<'a>(&self, line: &'a str, pos: usize) -> (usize, &'a str) {
        let line_before_cursor = &line[..pos];
        let mut word_start = 0;
        // Scan backwards to find the start of the current word
        for (i, c) in line_before_cursor.char_indices().rev() {
            if !(c.is_alphanumeric() || c == '_' || c == '\\') {
                word_start = i + c.len_utf8();
                break;
            }
        }
        (word_start, &line[word_start..pos])
    }

    /// Generates completion suggestions for the given word prefix.
    ///
    /// This provides completion for:
    /// - REPL commands (\\help, \\clear, \\quit, \\view, \\inspect)
    /// - Variable names from the current interpreter scope
    ///
    /// # Arguments
    /// * `word` - The partial word to complete
    ///
    /// # Returns
    /// Vector of completion pairs (display text and replacement text)
    fn get_completions(&self, word: &str) -> Vec<Pair> {
        let mut completions = Vec::new();
        if word.is_empty() {
            return completions;
        }

        // Available REPL commands
        let commands = ["\\help", "\\clear", "\\quit", "\\view", "\\inspect"];

        // Get all variable names from the interpreter
        let symbols = self.interpreter.borrow().get_all_symbols();

        // Add matching REPL commands
        for cmd in commands.iter() {
            if cmd.starts_with(word) {
                completions.push(Pair {
                    display: (*cmd).to_string(),
                    replacement: (*cmd).to_string(),
                });
            }
        }

        // Add matching variable names
        for symbol in symbols {
            if symbol.starts_with(word) {
                completions.push(Pair {
                    display: symbol.clone(),
                    replacement: symbol,
                });
            }
        }

        completions
    }
}

impl Helper for ReplHelper {}

impl Completer for ReplHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
        let (start, word) = self.find_word_to_complete(line, pos);
        let completions = self.get_completions(word);
        Ok((start, completions))
    }
}

impl Hinter for ReplHelper {
    type Hint = String;

    /// Provides hints based on command history.
    ///
    /// This delegates to the built-in history hinter to show potential
    /// completions based on previously entered commands.
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for ReplHelper {
    /// Provides syntax highlighting with bracket matching.
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    /// Highlights the prompt (currently no special highlighting).
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Borrowed(prompt)
    }

    /// Highlights hints with bold formatting.
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    /// Highlights completion candidates (currently no special highlighting).
    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        _completion: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        Borrowed(candidate)
    }
}

impl Validator for ReplHelper {
    /// Validates input (currently accepts all input as valid).
    ///
    /// This could be extended to provide real-time syntax validation
    /// for Carmen language constructs in the future.
    fn validate(&self, _ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        Ok(ValidationResult::Valid(None))
    }
}

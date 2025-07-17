use crate::core::{EventContent, MusicalEvent, Part, Score, Timeline};
use crate::interpreter::Value;

// ANSI Color Codes for terminal output formatting
pub const BLUE: &str = "\x1b[34m";
pub const GREEN: &str = "\x1b[32m";
pub const RED: &str = "\x1b[31m";
pub const YELLOW: &str = "\x1b[33m";
pub const RESET: &str = "\x1b[0m";

/// Prints the welcome message when the REPL starts.
pub fn print_welcome() {
    println!("{BLUE}carmen Language REPL{RESET}");
    println!("Type '\\help' for help, '\\quit' to quit.");
}

/// Prints comprehensive help information about available REPL commands and navigation.
pub fn print_help() {
    println!("{YELLOW}carmen REPL Commands:{RESET}");
    println!("  \\help     - Show this help message");
    println!("  \\clear    - Clear the screen");
    println!("  \\quit     - Quit the REPL");
    println!("  \\view     - Visualize a score, part, or timeline");
    println!("  \\inspect  - Inspect a variable's content");
    println!();
    println!("{YELLOW}Navigation:{RESET}");
    println!("  ↑/↓      - Navigate command history");
    println!("  Ctrl+D or Ctrl+C   - Exit REPL");
}

/// Prints the goodbye message when the REPL exits.
pub fn print_goodbye() {
    println!("{BLUE}Goodbye!{RESET}");
}

/// Prints a Carmen language value to the console with green formatting.
pub fn print_value(value: &Value) {
    println!("{GREEN}{value}{RESET}");
}

/// Prints a detailed inspection of a Carmen value, showing its hierarchical structure.
///
/// This provides a formatted view of complex musical objects with proper indentation
/// and color coding to show relationships between components.
pub fn print_inspected_value(value: &Value) {
    println!("{}", format_inspected_value(value, 0));
}

/// Recursively formats a Carmen value for inspection display.
///
/// This function handles different value types specially:
/// - Musical objects (Score, Part, Timeline, MusicalEvent) get detailed breakdown
/// - Lists show item count and recurse into elements
/// - Other values use their standard string representation
///
/// # Arguments
/// * `value` - The Carmen value to format
/// * `indent` - Current indentation level for nested display
fn format_inspected_value(value: &Value, indent: usize) -> String {
    let indentation = "  ".repeat(indent);
    match value {
        Value::Score(score) => format_score(score, indent),
        Value::Part(part) => format_part(part, indent),
        Value::Timeline(timeline) => format_timeline(timeline, indent),
        Value::MusicalEvent(event) => format_musical_event(event, indent),
        Value::List(items) => {
            let mut s = format!(
                "{}{}List ({} items):{}\n",
                indentation,
                YELLOW,
                items.len(),
                RESET
            );
            for item in items {
                s.push_str(&format_inspected_value(item, indent + 1));
            }
            s
        }
        _ => format!("{indentation}{GREEN}{value}{RESET}\n"),
    }
}

/// Formats a Score for detailed inspection display.
fn format_score(score: &Score, indent: usize) -> String {
    let indentation = "  ".repeat(indent);
    let name = score.name.as_deref().unwrap_or("Untitled");
    let mut s = format!("{indentation}{YELLOW}Score \"{name}\"{RESET}:\n");

    if let Some(timeline) = &score.timeline {
        s.push_str(&format_timeline(timeline, indent + 1));
    }

    if !score.movements.is_empty() {
        s.push_str(&format!(
            "{}{}Movements ({}):{}\n",
            "  ".repeat(indent + 1),
            YELLOW,
            score.movements.len(),
            RESET
        ));
    }

    s
}

/// Formats a Timeline for detailed inspection display.
fn format_timeline(timeline: &Timeline, indent: usize) -> String {
    let indentation = "  ".repeat(indent);
    let mut s = format!(
        "{}{}Timeline ({} parts):{}\n",
        indentation,
        YELLOW,
        timeline.parts.len(),
        RESET
    );
    for part in &timeline.parts {
        s.push_str(&format_part(part, indent + 1));
    }
    s
}

/// Formats a Part for detailed inspection display.
fn format_part(part: &Part, indent: usize) -> String {
    let indentation = "  ".repeat(indent);
    let name = part.name.as_deref().unwrap_or("Unnamed");
    let mut s = format!("{indentation}{YELLOW}Part \"{name}\"{RESET}:\n");

    if !part.events.is_empty() {
        for event in &part.events {
            s.push_str(&format_musical_event(event, indent + 1));
        }
    }

    if !part.staves.is_empty() {
        s.push_str(&format!(
            "{}{}Staves ({}):{}\n",
            "  ".repeat(indent + 1),
            YELLOW,
            part.staves.len(),
            RESET
        ));
    }

    s
}

/// Formats a MusicalEvent for detailed inspection display.
///
/// For sequence events, this creates a formatted table showing the content,
/// duration, dynamics, and attributes of each nested event.
fn format_musical_event(event: &MusicalEvent, indent: usize) -> String {
    let indentation = "  ".repeat(indent);
    let mut s = String::new();

    let event_type = match &event.content {
        EventContent::Note(_) => "Note",
        EventContent::Chord(_) => "Chord",
        EventContent::Rest => "Rest",
        EventContent::Sequence(_) => "Sequence",
    };

    s.push_str(&format!(
        "{}{}{} @ {}{}",
        indentation,
        YELLOW,
        event_type,
        event.duration.to_fractional_string(),
        RESET
    ));

    if let Some(dynamic) = &event.dynamic {
        s.push_str(&format!(" {BLUE}{dynamic:?}{RESET}"));
    }
    s.push('\n');

    match &event.content {
        EventContent::Note(pitch) => {
            s.push_str(&format!(
                "{}  {}{}{}\n",
                indentation,
                GREEN,
                pitch.to_note_name(),
                RESET
            ));
        }
        EventContent::Chord(chord) => {
            let notes: Vec<String> = chord.pitches.iter().map(|p| p.to_note_name()).collect();
            s.push_str(&format!(
                "{}  {}{}{}\n",
                indentation,
                GREEN,
                notes.join(", "),
                RESET
            ));
        }
        EventContent::Sequence(events) => {
            // Create a formatted table for sequence events
            let header_indent = "  ".repeat(indent + 1);
            s.push_str(&format!(
                "{}{:<12} {:<10} {:<10} {:<15}{}\n",
                header_indent, "Content", "Duration", "Dynamic", "Attributes", RESET
            ));
            s.push_str(&format!(
                "{}{:-<12} {:-<10} {:-<10} {:-<15}{}\n",
                header_indent, "", "", "", "", RESET
            ));

            for e in events {
                let content_str = match &e.content {
                    EventContent::Note(p) => p.to_note_name(),
                    EventContent::Chord(c) => {
                        let notes: Vec<String> =
                            c.pitches.iter().map(|p| p.to_note_name()).collect();
                        format!("({})", notes.join(", "))
                    }
                    _ => "".to_string(),
                };

                let dynamic_str = e
                    .dynamic
                    .map(|d| format!("{d:?}"))
                    .unwrap_or_else(|| "-".to_string());

                let attributes_str = if e.attributes.is_empty() {
                    "-".to_string()
                } else {
                    e.attributes
                        .iter()
                        .map(|a| format!("{a:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                };

                s.push_str(&format!(
                    "{}{GREEN}{:<12}{} {:<10} {}{:<10}{} {}{:<15}{}\n",
                    header_indent,
                    content_str,
                    RESET,
                    e.duration.to_fractional_string(),
                    BLUE,
                    dynamic_str,
                    RESET,
                    YELLOW,
                    attributes_str,
                    RESET
                ));
            }
        }
        EventContent::Rest => {}
    }

    s
}

/// Prints an error message to the console with red formatting.
pub fn print_error(err: &str) {
    eprintln!("{RED}{err}{RESET}");
}

/// Clears the console screen using ANSI escape sequences.
pub fn clear_screen() {
    print!("\x1B[2J\x1B[1;1H");
}

/// Returns the REPL prompt string with blue formatting.
pub fn prompt() -> String {
    format!("{BLUE}carmen> {RESET}")
}

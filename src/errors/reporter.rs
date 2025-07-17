//! Error reporting functionality for displaying formatted error messages.
//!
//! This module provides utilities for creating user-friendly error reports
//! that show the source code context and highlight the problematic area.

use crate::errors::CarmenError;
use std::io::Write;

/// Reports a Carmen error in a formatted, user-friendly way.
///
/// This function generates a diagnostic message similar to those produced by
/// modern compilers like Rust or TypeScript, showing:
/// - The error type and message
/// - The source file location (filename:line:column)
/// - The relevant line of source code
/// - Visual indicators (carets) pointing to the error location
///
/// # Arguments
///
/// * `writer` - The output destination (e.g., stderr, stdout, or a buffer)
/// * `source` - The complete source code being analyzed
/// * `error` - The error to report, containing both the error details and location
/// * `filename` - The name of the file being processed (for display purposes)
///
/// # Example Output
///
/// ```text
/// error: Parsing error
///   --> example.carmen:3:5
///    |
///  3 | let x = ;
///    |     ^ unexpected token
/// ```
pub fn report<W: Write>(
    writer: &mut W,
    source: &str,
    error: &CarmenError,
    filename: &str,
) -> std::io::Result<()> {
    let error_source = &error.source;
    let span = &error.span;
    let start_pos = span.start;
    let end_pos = span.end;

    // Extract the line containing the error
    let line = source.lines().nth(start_pos.line - 1).unwrap_or("");

    // Calculate formatting widths for consistent alignment
    let line_num_str = start_pos.line.to_string();
    let line_num_width = line_num_str.len();

    writeln!(writer, "error: {}", error_source.type_str())?;
    writeln!(
        writer,
        "{:>width$}--> {}:{}:{}",
        "",
        filename,
        start_pos.line,
        start_pos.column,
        width = line_num_width + 1
    )?;

    writeln!(writer, "{:>width$} |", "", width = line_num_width + 1)?;
    writeln!(writer, " {line_num_str} | {line}")?;

    // Create visual indicators pointing to the error location
    let padding = " ".repeat(start_pos.column.saturating_sub(1));
    let span_width = std::cmp::max(1, end_pos.offset - start_pos.offset);
    let carets = if span_width == 1 {
        "^".to_string() // Single character error
    } else {
        format!("^{}", "~".repeat(span_width - 1)) // Multi-character span
    };

    writeln!(
        writer,
        "{:>width$} | {}{} {}",
        "",
        padding,
        carets,
        error_source,
        width = line_num_width + 1
    )?;
    writeln!(writer)?;
    Ok(())
}

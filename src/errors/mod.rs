//! Error handling module.
//!
//! This module provides the core error types and utilities used throughout the Carmen
//! language implementation. It includes position tracking, error categorization, and
//! convenient error handling patterns.

pub mod reporter;

use std::fmt;

/// Represents a position in source code with line, column, and absolute offset.
///
/// Positions use 1-based indexing for line and column numbers to match
/// conventional text editor behavior.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based)
    pub column: usize,
    /// Absolute character offset from the beginning of the source (0-based)
    pub offset: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

impl Position {
    /// Creates a new position with the given line, column, and offset.
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }

    /// Creates a position representing the start of a source file.
    pub fn start() -> Self {
        Self {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Represents a range in source code from a start position to an end position.
///
/// Used to track the location of tokens, expressions, and errors in the source.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    /// Creates a span that covers a single position (zero-width span).
    pub fn single(pos: Position) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }
}

/// A span-less error representing the source of the problem.
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorSource {
    Lexical(String),
    Parsing(String),
    Runtime(String),
    Type(String),
    Music(String),
    Argument(String),
    Cli(String),
}

impl ErrorSource {
    /// Returns a human-readable string describing the error type.
    pub fn type_str(&self) -> &'static str {
        match self {
            ErrorSource::Lexical(_) => "Lexical error",
            ErrorSource::Parsing(_) => "Parsing error",
            ErrorSource::Runtime(_) => "Runtime error",
            ErrorSource::Type(_) => "Type error",
            ErrorSource::Music(_) => "Music error",
            ErrorSource::Argument(_) => "Argument error",
            ErrorSource::Cli(_) => "CLI error",
        }
    }
}

impl fmt::Display for ErrorSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorSource::Lexical(s)
            | ErrorSource::Parsing(s)
            | ErrorSource::Runtime(s)
            | ErrorSource::Type(s)
            | ErrorSource::Music(s)
            | ErrorSource::Argument(s)
            | ErrorSource::Cli(s) => write!(f, "{s}"),
        }
    }
}

impl std::error::Error for ErrorSource {}

/// The main error type for the application, combining a source and a span.
#[derive(Debug, Clone, PartialEq)]
pub struct CarmenError {
    pub source: ErrorSource,
    pub span: Span,
}

impl CarmenError {
    /// Creates a new Carmen error with the given source and location span.
    pub fn new(source: ErrorSource, span: Span) -> Self {
        Self { source, span }
    }
}

impl fmt::Display for CarmenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} (at {})",
            self.source.type_str(),
            self.source,
            self.span.start
        )
    }
}

impl std::error::Error for CarmenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.source)
    }
}

/// A trait for conveniently adding a span to an `ErrorSource`.
pub trait AddSpan {
    /// Converts an `ErrorSource` into a `CarmenError` by adding span information.
    fn with_span(self, span: Span) -> CarmenError;
}

impl AddSpan for ErrorSource {
    fn with_span(self, span: Span) -> CarmenError {
        CarmenError::new(self, span)
    }
}

/// The default result type for most fallible operations in the language pipeline.
pub type Result<T> = std::result::Result<T, CarmenError>;

/// A result type for core logic that does not have access to span information.
pub type CoreResult<T> = std::result::Result<T, ErrorSource>;

//! Score export functionality for converting Carmen files to various formats.
//!
//! This module provides a pluggable export system that can convert musical scores
//! into different formats like LilyPond notation files and plain text representations.
//!
//! # Architecture
//!
//! The export system is built around the [`Exporter`] trait, which defines a common
//! interface for all export formats. Specific exporters implement this trait to
//! handle format-specific conversion logic.
//!
//! # Available Formats
//!
//! - **LilyPond**: Exports scores as `.ly` files for professional music typesetting
//! - **Text**: Exports scores as human-readable text files for debugging and inspection

pub mod lilypond;
pub mod text;

use crate::core::Score;
use crate::errors::{AddSpan, ErrorSource, Position, Result, Span};
use std::io::Write;

/// Core trait for score exporters that convert musical scores to specific output formats.
///
/// All export formats must implement this trait to provide a consistent interface
/// for score conversion. The trait handles both the conversion logic and metadata
/// about the export format.
pub trait Exporter {
    /// Export a score to the given writer.
    ///
    /// This is the main conversion method that takes a parsed musical score and
    /// writes it in the target format to the provided writer.
    ///
    /// # Arguments
    ///
    /// * `score` - The musical score to export
    /// * `writer` - The destination for the exported data
    ///
    /// # Errors
    ///
    /// Returns an error if the score cannot be converted to the target format
    /// or if writing to the output fails.
    fn export(&self, score: &Score, writer: &mut dyn Write) -> Result<()>;

    /// Get the file extension for this export format.
    ///
    /// Returns the standard file extension (without the dot) that should be
    /// used for files in this format.
    fn file_extension(&self) -> &'static str;

    /// Get a human-readable name for this export format.
    ///
    /// Returns a descriptive name that can be displayed to users when
    /// selecting export formats.
    fn format_name(&self) -> &'static str;
}

/// Configuration for score export operations.
///
/// Specifies both the target format and the output destination for the exported score.
pub struct ExportConfig {
    pub format: ExportFormat,
    pub output: ExportOutput,
}

/// Available export formats for musical scores.
#[derive(Debug, Clone)]
pub enum ExportFormat {
    /// Plain text format for human-readable output and debugging
    Text,
    /// LilyPond format for professional music typesetting
    LilyPond,
}

/// Output destinations for exported scores.
#[derive(Debug, Clone)]
pub enum ExportOutput {
    /// Write to standard output
    Stdout,
    /// Write to a file at the specified path
    File(String),
}

/// Manager for coordinating score export operations.
///
/// The export manager provides a high-level interface for exporting scores
/// to different formats and destinations. It handles the creation of appropriate
/// exporters and manages the export process.
pub struct ExportManager;

impl ExportManager {
    /// Create a new export manager.
    pub fn new() -> Self {
        Self
    }

    /// Export a score according to the provided configuration.
    ///
    /// This is the main entry point for score export. It creates the appropriate
    /// exporter for the target format and handles writing to the specified output.
    ///
    /// # Arguments
    ///
    /// * `score` - The musical score to export
    /// * `config` - Export configuration specifying format and output destination
    pub fn export_score(&self, score: &Score, config: &ExportConfig) -> Result<()> {
        let exporter = self.get_exporter(&config.format);

        match &config.output {
            ExportOutput::Stdout => {
                let mut stdout = std::io::stdout();
                exporter.export(score, &mut stdout)
            }
            ExportOutput::File(path) => {
                let mut file = std::fs::File::create(path).map_err(|e| {
                    ErrorSource::Runtime(format!("Failed to create output file '{path}': {e}"))
                        .with_span(Span::single(Position::start()))
                })?;
                exporter.export(score, &mut file)
            }
        }
    }

    /// Get an exporter instance for the specified format.
    ///
    /// Creates and returns the appropriate exporter implementation based on
    /// the requested format.
    fn get_exporter(&self, format: &ExportFormat) -> Box<dyn Exporter> {
        match format {
            ExportFormat::Text => Box::new(text::TextExporter::new()),
            ExportFormat::LilyPond => Box::new(lilypond::LilyPondExporter {}),
        }
    }

    /// Generate a default filename for the given export format.
    ///
    /// Creates a filename by combining the base name with the appropriate
    /// file extension for the export format.
    ///
    /// # Arguments
    ///
    /// * `format` - The export format to generate a filename for
    /// * `base_name` - Optional base name for the file (defaults to "score")
    ///
    /// # Returns
    ///
    /// A filename string with the appropriate extension for the format
    pub fn get_default_filename(&self, format: &ExportFormat, base_name: Option<&str>) -> String {
        let exporter = self.get_exporter(format);
        let base = base_name.unwrap_or("score");
        format!("{}.{}", base, exporter.file_extension())
    }
}

impl Default for ExportManager {
    fn default() -> Self {
        Self::new()
    }
}

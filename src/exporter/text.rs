//! Plain text export functionality for converting Carmen files to human-readable text format.
//!
//! This module provides a text-based exporter that converts musical scores into structured,
//! human-readable text output. It's particularly useful for debugging, inspection, and
//! situations where a simple text representation of the musical content is needed.
//!
//! # Features
//!
//! - **Hierarchical structure**: Clearly shows the score's organizational hierarchy
//! - **Configurable output**: Options to include/exclude metadata, timing, and detailed events
//! - **Timing information**: Optional display of absolute timing for each event
//! - **Detailed event information**: Shows notes, chords, rests with full attribute information
//! - **Multi-movement support**: Handles complex scores with multiple movements
//!
//! # Output Format
//!
//! The text exporter generates output in this hierarchical structure:
//! ```text
//! Score: "Title"
//!   Composer: John Doe
//!   Total Duration: 4.00 (whole notes)
//!
//!   Part 1: "Piano"
//!     Instrument: Piano
//!     Duration: 4.00 (whole notes)
//!     Events: 16 total
//!     Staff 1:
//!       Voice 1:
//!         [T: 0.00] Note: C4, Duration: 1/4, Dynamic: F
//! ```

use std::fmt::Write as FmtWrite;
use std::io::Write;

use super::Exporter;
use crate::common::fraction::Fraction;
use crate::core::{
    Attribute, Chord, Dynamic, EventContent, MusicalEvent, Part, Pitch, Score, Staff, Timeline,
    Voice,
};
use crate::errors::{AddSpan, ErrorSource, Position, Result, Span};

/// Size of indentation units for hierarchical text formatting.
const INDENT_SIZE: usize = 2;

/// Plain text exporter for converting musical scores to human-readable text format.
///
/// The text exporter provides a configurable way to convert musical scores into
/// structured text output. It's designed for debugging, inspection, and situations
/// where a simple textual representation of musical content is needed.
///
/// # Configuration Options
///
/// - **Metadata inclusion**: Control whether score metadata (title, composer, etc.) is included
/// - **Timing information**: Control whether absolute timing information is displayed
/// - **Event detail level**: Control the level of detail shown for individual musical events
#[derive(Debug, Clone, Copy)]
pub struct TextExporter {
    /// Whether to include metadata (title, composer, etc.) in the output
    include_metadata: bool,
    /// Whether to include timing information for events
    include_timing: bool,
    /// Whether to show detailed information for individual events
    detailed_events: bool,
}

impl TextExporter {
    /// Create a new text exporter with default settings.
    pub fn new() -> Self {
        Self {
            include_metadata: true,
            include_timing: true,
            detailed_events: true,
        }
    }

    /// Configure whether to include metadata in the output.
    ///
    /// # Arguments
    ///
    /// * `include` - Whether to include metadata in the output
    pub fn with_metadata(mut self, include: bool) -> Self {
        self.include_metadata = include;
        self
    }

    /// Configure whether to include timing information in the output.
    ///
    /// When enabled, shows absolute timing offsets for each musical event,
    /// which is useful for debugging timing-related issues.
    ///
    /// # Arguments
    ///
    /// * `include` - Whether to include timing information
    pub fn with_timing(mut self, include: bool) -> Self {
        self.include_timing = include;
        self
    }

    /// Configure the level of detail for individual musical events.
    ///
    /// When enabled, shows comprehensive information about each event
    /// including pitch, duration, dynamics, and articulations.
    ///
    /// # Arguments
    ///
    /// * `detailed` - Whether to show detailed event information
    pub fn with_detailed_events(mut self, detailed: bool) -> Self {
        self.detailed_events = detailed;
        self
    }
}

impl Default for TextExporter {
    fn default() -> Self {
        Self::new()
    }
}

impl Exporter for TextExporter {
    fn export(&self, score: &Score, writer: &mut dyn Write) -> Result<()> {
        let mut buffer = String::new();
        self.format_score(score, &mut buffer).map_err(|e| {
            ErrorSource::Runtime(format!("Failed to format score: {e}"))
                .with_span(Span::single(Position::start()))
        })?;

        writer.write_all(buffer.as_bytes()).map_err(|e| {
            ErrorSource::Runtime(format!("Failed to write export data: {e}"))
                .with_span(Span::single(Position::start()))
        })?;

        Ok(())
    }

    fn file_extension(&self) -> &'static str {
        "txt"
    }

    fn format_name(&self) -> &'static str {
        "Plain Text"
    }
}

impl TextExporter {
    /// Format a complete musical score as structured text.
    ///
    /// This is the main formatting method that converts a Carmen score into
    /// hierarchical text output, respecting the configured options for metadata,
    /// timing, and event detail levels.
    ///
    /// # Arguments
    ///
    /// * `score` - The musical score to format
    /// * `out` - The output string buffer to write to
    ///
    /// # Returns
    ///
    /// A formatting result, or an error if string formatting fails
    fn format_score(&self, score: &Score, out: &mut String) -> std::fmt::Result {
        // Display score name or generic title
        if let Some(name) = &score.name {
            writeln!(out, "Score: {name}")?;
        } else {
            writeln!(out, "Score:")?;
        }

        // Include metadata if configured
        if self.include_metadata && !score.context_changes.is_empty() {
            for change in &score.context_changes {
                writeln!(out, "  {}: {}", change.key, change.value)?;
            }
        }

        // Include timing information if configured
        if self.include_timing {
            writeln!(
                out,
                "Total Duration: {:.2} (whole notes)\n",
                score.total_duration()
            )?;
        }

        // Handle multi-movement vs single-movement scores
        if score.is_multi_movement() {
            writeln!(out, "Movements: {}", score.movements.len())?;
            for (i, movement) in score.movements.iter().enumerate() {
                writeln!(out)?;
                self.format_movement(movement, i + 1, out)?;
            }
        } else if let Some(timeline) = &score.timeline {
            self.format_timeline(timeline, "", out)?;
        }

        Ok(())
    }

    /// Format a movement within a multi-movement score.
    ///
    /// Displays movement-specific information including name, duration,
    /// and the number of parts, then delegates to timeline formatting.
    ///
    /// # Arguments
    ///
    /// * `movement` - The movement to format
    /// * `index` - The movement number (1-indexed)
    /// * `out` - The output string buffer to write to
    fn format_movement(
        &self,
        movement: &crate::core::Movement,
        index: usize,
        out: &mut String,
    ) -> std::fmt::Result {
        let indent = " ".repeat(INDENT_SIZE);

        // Display movement title
        if let Some(name) = &movement.name {
            writeln!(out, "Movement {index}: \"{name}\"")?;
        } else {
            writeln!(out, "Movement {index}")?;
        }

        // Include duration if timing is enabled
        if self.include_timing {
            writeln!(
                out,
                "{indent}Duration: {:.2} (whole notes)",
                movement.total_duration()
            )?;
        }

        writeln!(out, "{indent}Parts: {}", movement.timeline.parts.len())?;

        self.format_timeline(&movement.timeline, &indent, out)?;

        Ok(())
    }

    /// Format a timeline containing multiple musical parts.
    ///
    /// Handles timeline-specific context changes and delegates to individual
    /// part formatting. Timeline context changes are prefixed with '@' to
    /// distinguish them from score-level metadata.
    ///
    /// # Arguments
    ///
    /// * `timeline` - The timeline containing musical parts
    /// * `base_indent` - The base indentation level for this timeline
    /// * `out` - The output string buffer to write to
    fn format_timeline(
        &self,
        timeline: &Timeline,
        base_indent: &str,
        out: &mut String,
    ) -> std::fmt::Result {
        let indent = format!("{base_indent}{}", " ".repeat(INDENT_SIZE));

        // Include timeline-specific context changes if metadata is enabled
        if self.include_metadata && !timeline.context_changes.is_empty() {
            for change in &timeline.context_changes {
                writeln!(out, "{indent}@{}: {}", change.key, change.value)?;
            }
        }

        // Format each part in the timeline
        for (i, part) in timeline.parts.iter().enumerate() {
            writeln!(out)?;
            self.format_part(part, i + 1, &indent, out)?;
        }

        Ok(())
    }

    /// Format a musical part with its instrument and event information.
    ///
    /// Displays part-specific information including name, instrument, duration,
    /// and event count. If detailed events are enabled, also shows individual
    /// events and staff structure.
    ///
    /// # Arguments
    ///
    /// * `part` - The musical part to format
    /// * `index` - The part number (1-indexed)
    /// * `base_indent` - The base indentation level for this part
    /// * `out` - The output string buffer to write to
    fn format_part(
        &self,
        part: &Part,
        index: usize,
        base_indent: &str,
        out: &mut String,
    ) -> std::fmt::Result {
        let indent = format!("{base_indent}{}", " ".repeat(INDENT_SIZE));

        // Display part name
        if let Some(name) = &part.name {
            writeln!(out, "{base_indent}Part {index}: \"{name}\"")?;
        } else {
            writeln!(out, "{base_indent}Part {index}")?;
        }

        // Display instrument if specified
        if let Some(instrument) = &part.instrument {
            writeln!(out, "{indent}Instrument: {instrument}")?;
        }

        // Include duration if timing is enabled
        if self.include_timing {
            writeln!(
                out,
                "{indent}Duration: {:.2} (whole notes)",
                part.total_duration()
            )?;
        }

        writeln!(out, "{indent}Events: {} total", part.total_event_count())?;

        // Show detailed event information if configured
        if self.detailed_events {
            // Format part-level events if any exist
            if !part.events.is_empty() {
                writeln!(out, "{indent}Events:")?;
                for event in &part.events {
                    self.format_event(event, &format!("{indent}  "), out, Fraction::new(0, 1))?;
                }
            }

            // Format each staff in the part
            for staff in &part.staves {
                self.format_staff(staff, &indent, out)?;
            }
        }

        Ok(())
    }

    /// Format a musical staff with its voices and context information.
    ///
    /// Displays staff-specific context changes (like clef changes) and
    /// delegates to individual voice formatting.
    ///
    /// # Arguments
    ///
    /// * `staff` - The staff to format
    /// * `base_indent` - The base indentation level for this staff
    /// * `out` - The output string buffer to write to
    fn format_staff(&self, staff: &Staff, base_indent: &str, out: &mut String) -> std::fmt::Result {
        let indent = format!("{base_indent}{}", " ".repeat(INDENT_SIZE));
        writeln!(out, "{base_indent}Staff {}:", staff.number)?;

        // Include staff-specific context changes if metadata is enabled
        if self.include_metadata && !staff.context_changes.is_empty() {
            for change in &staff.context_changes {
                writeln!(out, "{indent}@{}: {}", change.key, change.value)?;
            }
        }

        // Format each voice in the staff
        for (i, voice) in staff.voices.iter().enumerate() {
            self.format_voice(voice, i + 1, &indent, out)?;
        }

        Ok(())
    }

    /// Format a musical voice with its sequence of events.
    ///
    /// A voice represents a single melodic line within a staff. This method
    /// displays the voice number and formats all events within the voice.
    ///
    /// # Arguments
    ///
    /// * `voice` - The voice to format
    /// * `index` - The voice number (1-indexed)
    /// * `base_indent` - The base indentation level for this voice
    /// * `out` - The output string buffer to write to
    fn format_voice(
        &self,
        voice: &Voice,
        index: usize,
        base_indent: &str,
        out: &mut String,
    ) -> std::fmt::Result {
        writeln!(out, "{base_indent}Voice {index}:")?;
        let indent = format!("{base_indent}{}", " ".repeat(INDENT_SIZE));

        // Format each event in the voice
        for event in &voice.events {
            self.format_event(event, &indent, out, Fraction::new(0, 1))?;
        }

        Ok(())
    }

    /// Format an individual musical event with full detail information.
    ///
    /// Handles the formatting of musical events including notes, chords, rests,
    /// and sequences. For sequence events, recursively formats contained events
    /// with proper timing offset calculations.
    ///
    /// # Arguments
    ///
    /// * `event` - The musical event to format
    /// * `indent` - The indentation string for this event
    /// * `out` - The output string buffer to write to
    /// * `time_offset` - Cumulative time offset from parent sequences
    fn format_event(
        &self,
        event: &MusicalEvent,
        indent: &str,
        out: &mut String,
        time_offset: Fraction,
    ) -> std::fmt::Result {
        let absolute_offset = time_offset + event.offset;

        // Handle sequence events by recursively formatting contained events
        if let EventContent::Sequence(events) = &event.content {
            for seq_event in events {
                self.format_event(seq_event, indent, out, absolute_offset)?;
            }
            return Ok(());
        }

        // Build timing information if enabled
        let timing = if self.include_timing {
            format!("[T: {absolute_offset:.2}] ")
        } else {
            String::new()
        };

        // Format the main event components
        let content = self.format_event_content(&event.content, indent);
        let duration = event.duration.to_fractional_string();
        let dynamic = self.format_dynamic(&event.dynamic);
        let attributes = self.format_attributes(&event.attributes);

        writeln!(
            out,
            "{indent}{timing}{content}, Duration: {duration}{dynamic}{attributes}"
        )?;

        Ok(())
    }

    /// Format the content portion of a musical event.
    ///
    /// Converts event content (note, chord, rest, or sequence) into a
    /// human-readable string representation.
    ///
    /// # Arguments
    ///
    /// * `content` - The event content to format
    /// * `_indent` - The indentation (currently unused)
    ///
    /// # Returns
    ///
    /// A formatted string describing the event content
    fn format_event_content(&self, content: &EventContent, _indent: &str) -> String {
        match content {
            EventContent::Note(pitch) => format!("Note: {}", self.format_pitch(pitch)),
            EventContent::Chord(chord) => format!("Chord: {}", self.format_chord(chord)),
            EventContent::Rest => "Rest".to_string(),
            EventContent::Sequence(_) => "Sequence".to_string(),
        }
    }

    /// Format a musical pitch as a note name string.
    ///
    /// # Arguments
    ///
    /// * `pitch` - The pitch to format
    ///
    /// # Returns
    ///
    /// A note name string (e.g., "c4", "f#3")
    fn format_pitch(&self, pitch: &Pitch) -> String {
        pitch.to_note_name()
    }

    /// Format a chord as a bracketed list of pitches.
    ///
    /// # Arguments
    ///
    /// * `chord` - The chord to format
    ///
    /// # Returns
    ///
    /// A formatted chord string (e.g., "[c4, e4, g4]")
    fn format_chord(&self, chord: &Chord) -> String {
        let pitches: Vec<String> = chord.pitches.iter().map(|p| self.format_pitch(p)).collect();
        format!("[{}]", pitches.join(", "))
    }

    /// Format dynamic markings for display.
    ///
    /// # Arguments
    ///
    /// * `dynamic` - Optional dynamic marking
    ///
    /// # Returns
    ///
    /// A formatted dynamic string or empty string if no dynamic
    fn format_dynamic(&self, dynamic: &Option<Dynamic>) -> String {
        if let Some(d) = dynamic {
            format!(", Dynamic: {d:?}")
        } else {
            String::new()
        }
    }

    /// Format articulation attributes for display.
    ///
    /// # Arguments
    ///
    /// * `attributes` - Slice of articulation attributes
    ///
    /// # Returns
    ///
    /// A formatted attributes string or empty string if no attributes
    fn format_attributes(&self, attributes: &[Attribute]) -> String {
        if !attributes.is_empty() {
            let attrs: Vec<String> = attributes.iter().map(|a| format!("{a:?}")).collect();
            format!(", Attributes: {}", attrs.join(", "))
        } else {
            String::new()
        }
    }
}

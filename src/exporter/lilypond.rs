//! LilyPond export functionality for converting Carmen scores to LilyPond notation files.
//!
//! This module provides a comprehensive exporter that converts musical scores into
//! LilyPond format, a powerful music typesetting system. The exporter handles complex
//! musical notation including tuplets, ties across measure boundaries, multiple voices,
//! and various musical attributes.
//!
//! # Features
//!
//! - **Multi-staff scores**: Supports parts with multiple staves (e.g., piano)
//! - **Tuplet handling**: Automatic grouping and formatting of tuplets
//! - **Measure-aware tying**: Automatically ties notes that cross measure boundaries
//! - **Dynamic markings**: Converts dynamics to LilyPond markup
//! - **Articulations**: Supports staccato, accent, and tenuto markings
//! - **Metadata**: Includes title, composer, tempo, and time signature information
//!
//! # LilyPond Output Structure
//!
//! The generated LilyPond files follow this structure:
//! ```lilypond
//! \version "2.24.3"
//!
//! \header {
//!   title = "Song Title"
//!   composer = "Composer Name"
//! }
//!
//! \score {
//!   % Musical content with proper staff grouping
//!   \layout { }
//!   \midi { }
//! }
//! ```

use super::Exporter;
use crate::common::fraction::Fraction;
use crate::core::{
    Attribute, Chord, Clef, ContextChange, Duration, EventContent, MetadataKey, MetadataValue,
    MusicalEvent, Part, Pitch, Score, Staff, Timeline,
};
use crate::errors::{AddSpan, ErrorSource, Position, Result, Span};
use std::io::Write;

/// Recursively flatten nested musical event sequences into a linear sequence.
///
/// This function takes a slice of musical events that may contain nested sequences
/// and flattens them into a single linear sequence. It properly handles timing
/// offsets and preserves musical attributes and dynamics from parent events.
///
/// # Arguments
///
/// * `events` - The events to flatten, which may contain nested sequences
///
/// # Returns
///
/// A vector of flattened musical events with properly calculated offsets
///
/// # Behavior
///
/// - Recursively processes `EventContent::Sequence` events
/// - Calculates cumulative offsets for nested events
/// - Preserves dynamics and attributes from parent sequence events
/// - Merges attributes without duplication
fn flatten_events(events: &[MusicalEvent]) -> Vec<MusicalEvent> {
    events
        .iter()
        .flat_map(|event| {
            if let EventContent::Sequence(inner_events) = &event.content {
                if inner_events.is_empty() {
                    return Vec::new();
                }

                let mut flattened_inner = flatten_events(inner_events);
                if flattened_inner.is_empty() {
                    return Vec::new();
                }

                // Set cumulative offsets for the flattened inner events
                let mut current_offset = event.offset;
                for inner in &mut flattened_inner {
                    inner.offset = current_offset;
                    current_offset += inner.duration.fraction;
                }

                if event.dynamic.is_some() {
                    flattened_inner[0].dynamic = event.dynamic;
                }
                if !event.attributes.is_empty() {
                    let mut new_attrs = event.attributes.clone();
                    for attr in &flattened_inner[0].attributes {
                        if !new_attrs.contains(attr) {
                            new_attrs.push(*attr);
                        }
                    }
                    flattened_inner[0].attributes = new_attrs;
                }
                flattened_inner
            } else {
                vec![event.clone()]
            }
        })
        .collect()
}

/// LilyPond format exporter for converting musical scores to LilyPond notation files.
///
/// This exporter generates LilyPond files (.ly) that can be processed by the LilyPond
/// typesetting system to create high-quality sheet music. It handles complex musical
/// notation including multiple staves, voices, tuplets, ties, and articulations.
pub struct LilyPondExporter {}

impl Exporter for LilyPondExporter {
    fn export(&self, score: &Score, writer: &mut dyn Write) -> Result<()> {
        let formatted_score = self.format_score(score)?;
        writer.write_all(formatted_score.as_bytes()).map_err(|e| {
            ErrorSource::Runtime(format!("Failed to write to output: {e}"))
                .with_span(Span::single(Position::start()))
        })?;
        Ok(())
    }

    fn file_extension(&self) -> &'static str {
        "ly"
    }

    fn format_name(&self) -> &'static str {
        "LilyPond"
    }
}

impl LilyPondExporter {
    /// Format a complete musical score as a LilyPond file.
    ///
    /// This is the main formatting method that converts a Carmen score into a complete
    /// LilyPond file with proper version header, metadata, and musical content.
    ///
    /// # Arguments
    ///
    /// * `score` - The musical score to format
    ///
    /// # Returns
    ///
    /// A formatted LilyPond file as a string, or an error if formatting fails
    fn format_score(&self, score: &Score) -> Result<String> {
        let mut output = String::new();

        // LilyPond version declaration
        output.push_str("\\version \"2.24.3\"\n\n");

        // Header section with metadata
        output.push_str("\\header {\n");
        if let Some(title) = score
            .context_changes
            .iter()
            .find(|c| c.key == crate::core::MetadataKey::Title)
        {
            if let crate::core::MetadataValue::String(s) = &title.value {
                output.push_str(&format!("  title = \"{s}\"\n"));
            }
        }
        if let Some(composer) = score
            .context_changes
            .iter()
            .find(|c| c.key == crate::core::MetadataKey::Composer)
        {
            if let crate::core::MetadataValue::String(s) = &composer.value {
                output.push_str(&format!("  composer = \"{s}\"\n"));
            }
        }
        output.push_str("}\n\n");

        output.push_str("\\score {\n");

        let mut is_first_staff = true;
        if let Some(timeline) = &score.timeline {
            output.push_str(&self.format_timeline(
                timeline,
                &score.context_changes,
                &mut is_first_staff,
                1,
            )?);
        } else if !score.movements.is_empty() {
            if let Some(first_movement) = score.movements.first() {
                let context: Vec<_> = score
                    .context_changes
                    .iter()
                    .chain(&first_movement.context_changes)
                    .cloned()
                    .collect();
                output.push_str(&self.format_timeline(
                    &first_movement.timeline,
                    &context,
                    &mut is_first_staff,
                    1,
                )?);
            }
        }

        output.push_str("  \\layout { }\n");
        output.push_str("  \\midi { }\n");
        output.push_str("}\n");

        Ok(output)
    }

    /// Format a timeline containing multiple musical parts.
    ///
    /// Generates LilyPond notation for a timeline, which may contain multiple parts
    /// that need to be grouped appropriately (using `<<` `>>` brackets for simultaneous parts).
    ///
    /// # Arguments
    ///
    /// * `timeline` - The timeline containing musical parts
    /// * `parent_context` - Context changes from parent scope
    /// * `is_first_staff` - Mutable reference tracking if this is the first staff (for tempo)
    /// * `indent_level` - Current indentation level for formatting
    ///
    /// # Returns
    ///
    /// Formatted LilyPond notation for the timeline
    fn format_timeline(
        &self,
        timeline: &Timeline,
        parent_context: &[ContextChange],
        is_first_staff: &mut bool,
        indent_level: usize,
    ) -> Result<String> {
        let indent = "  ".repeat(indent_level);
        let mut output = String::new();

        if timeline.parts.is_empty() {
            return Ok(output);
        }

        // Use double angle brackets for multiple simultaneous parts
        let use_double_angle = timeline.parts.len() > 1;
        if use_double_angle {
            output.push_str(&format!("{indent}<<\n"));
        }

        let context: Vec<_> = parent_context
            .iter()
            .chain(&timeline.context_changes)
            .cloned()
            .collect();
        for part in &timeline.parts {
            output.push_str(&self.format_part(part, &context, is_first_staff, indent_level + 1)?);
        }

        if use_double_angle {
            output.push_str(&format!("{indent}>>\n"));
        }

        Ok(output)
    }

    /// Format a musical part, which may contain multiple staves.
    ///
    /// Handles the conversion of a musical part to LilyPond notation, including
    /// staff grouping (GrandStaff for multi-staff parts), instrument names,
    /// and context-specific settings like time signature engravers.
    ///
    /// # Arguments
    ///
    /// * `part` - The musical part to format
    /// * `parent_context` - Context changes from parent scope
    /// * `is_first_staff` - Mutable reference tracking if this is the first staff
    /// * `indent_level` - Current indentation level for formatting
    ///
    /// # Returns
    ///
    /// Formatted LilyPond notation for the part
    fn format_part(
        &self,
        part: &Part,
        parent_context: &[ContextChange],
        is_first_staff: &mut bool,
        indent_level: usize,
    ) -> Result<String> {
        let indent = "  ".repeat(indent_level);
        let mut output = String::new();

        // Add comment identifying the part
        output.push_str(&format!(
            "{}% Part: {}\n",
            indent,
            part.name.as_deref().unwrap_or("Unnamed")
        ));

        // Choose appropriate staff container based on number of staves
        let staff_container = if part.staves.len() > 1 {
            "GrandStaff" // For multi-staff instruments like piano
        } else {
            "Staff" // For single-staff instruments
        };
        let mut staff_line = format!("{indent} \\new {staff_container} ");

        let name_to_use = part.instrument.as_deref().or(part.name.as_deref());

        // Check if time signature is defined in context
        // If not, we'll remove time signature and bar engravers
        let time_signature_from_context = parent_context
            .iter()
            .chain(&part.context_changes)
            .any(|c| matches!(c.key, MetadataKey::TimeSignature));

        // Build the 'with' block content for staff customization
        let mut with_block_content = String::new();
        if let Some(instrument_name) = name_to_use {
            let short_name = instrument_name
                .split_whitespace()
                .next()
                .unwrap_or(instrument_name);
            with_block_content.push_str(&format!(
                "instrumentName = #\"{instrument_name}\" shortInstrumentName = #\"{short_name}\" "
            ));
        }

        // Remove time signature and bar engravers if no time signature is defined
        if !time_signature_from_context {
            with_block_content
                .push_str("\\remove \"Time_signature_engraver\" \\remove \"Bar_engraver\" ");
        }

        if !with_block_content.is_empty() {
            staff_line.push_str(&format!("\\with {{ {with_block_content} }} "));
        }

        output.push_str(&staff_line);
        output.push_str("{\n");

        let context: Vec<_> = parent_context
            .iter()
            .chain(&part.context_changes)
            .cloned()
            .collect();

        if part.staves.len() > 1 {
            output.push_str(&format!("{indent}  <<\n"));
            for staff in &part.staves {
                output.push_str(&format!(
                    "{} \\new Staff {{\n",
                    "  ".repeat(indent_level + 2)
                ));
                output.push_str(&self.format_staff_internals(
                    staff,
                    &context,
                    is_first_staff,
                    indent_level + 3,
                )?);
                output.push_str(&format!("{} }}\n", "  ".repeat(indent_level + 2)));
            }
            output.push_str(&format!("{indent}  >>\n"));
        } else if let Some(staff) = part.staves.first() {
            output.push_str(&self.format_staff_internals(
                staff,
                &context,
                is_first_staff,
                indent_level + 1,
            )?);
        } else if !part.events.is_empty() {
            let mut staff = Staff::new(1);
            let mut voice = crate::core::Voice::new();
            voice.events = part.events.clone();
            staff = staff.add_voice(voice);
            output.push_str(&self.format_staff_internals(
                &staff,
                &context,
                is_first_staff,
                indent_level + 1,
            )?);
        }

        output.push_str(&format!("{indent}}}\n"));
        Ok(output)
    }

    /// Format the internal content of a musical staff.
    ///
    /// Handles the formatting of staff-specific content including clef, tempo,
    /// time signature, and voice content. Also manages multi-voice notation
    /// using LilyPond's voice separation syntax.
    ///
    /// # Arguments
    ///
    /// * `staff` - The staff to format
    /// * `parent_context` - Context changes from parent scope
    /// * `is_first_staff` - Mutable reference tracking if this is the first staff
    /// * `indent_level` - Current indentation level for formatting
    ///
    /// # Returns
    ///
    /// Formatted LilyPond notation for the staff internals
    fn format_staff_internals(
        &self,
        staff: &Staff,
        parent_context: &[ContextChange],
        is_first_staff: &mut bool,
        indent_level: usize,
    ) -> Result<String> {
        let indent = "  ".repeat(indent_level);
        let mut output = String::new();
        let context: Vec<_> = parent_context
            .iter()
            .chain(&staff.context_changes)
            .cloned()
            .collect();

        // Set clef based on context (defaults to treble clef)
        let clef = context
            .iter()
            .rev()
            .find_map(|c| match (&c.key, &c.value) {
                (MetadataKey::Clef, MetadataValue::Clef(clef_val)) => Some(clef_val),
                _ => None,
            })
            .copied()
            .unwrap_or_default();
        output.push_str(&format!("{}  \\clef {}\n", indent, self.format_clef(clef)));

        // Add tempo marking only to the first staff to avoid duplication
        if *is_first_staff {
            let tempo = context.iter().rev().find_map(|c| {
                if let (MetadataKey::Tempo, MetadataValue::Number(val)) = (&c.key, &c.value) {
                    Some(val)
                } else {
                    None
                }
            });

            if let Some(t) = tempo {
                output.push_str(&format!("{}  \\tempo 4 = {}\n", indent, *t as u32));
            }
            *is_first_staff = false;
        }

        let time_signature = context.iter().rev().find_map(|c| match (&c.key, &c.value) {
            (MetadataKey::TimeSignature, MetadataValue::TimeSignature(num, den)) => {
                Some((*num, *den))
            }
            _ => None,
        });

        // Add time signature if defined
        if let Some((num, den)) = time_signature {
            output.push_str(&format!("{indent}  \\time {num}/{den}\n"));
        }

        // Begin absolute pitch notation block
        output.push_str(&format!("{indent}  \\absolute {{\n"));
        let inner_indent = "  ".repeat(indent_level + 1);

        // Handle multiple voices with proper voice separation
        if staff.voices.len() > 1 {
            output.push_str(&format!("{inner_indent}<<\n"));
            for (i, voice) in staff.voices.iter().enumerate() {
                let music = self.format_musical_sequence(&voice.events, time_signature)?;
                output.push_str(&format!("{inner_indent}  {{ {music} }}\n"));
                // Add voice separator except after the last voice
                if i < staff.voices.len() - 1 {
                    output.push_str(&format!("{inner_indent}  \\\\\n"));
                }
            }
            output.push_str(&format!("{inner_indent}>>\n"));
        } else if let Some(voice) = staff.voices.first() {
            let music_string = self.format_musical_sequence(&voice.events, time_signature)?;
            output.push_str(&format!("{}{}\n", inner_indent, music_string.trim()));
        }

        output.push_str(&format!("{indent}  }}\n"));
        Ok(output)
    }

    /// Segment events into groups based on tuplet boundaries.
    ///
    /// This function groups musical events that should be formatted together,
    /// particularly for tuplet notation. Events are grouped when they share
    /// the same tuplet ratio and fit within tuplet duration boundaries.
    ///
    /// # Arguments
    ///
    /// * `events` - Events with tie information to segment
    /// * `_time_signature` - Time signature context (currently unused)
    ///
    /// # Returns
    ///
    /// A vector of event groups, where each group represents events that
    /// should be formatted together (e.g., within the same tuplet)
    fn segment_events<'a>(
        &self,
        events: &'a [(MusicalEvent, bool)],
        _time_signature: Option<(u32, u32)>,
    ) -> Vec<Vec<&'a (MusicalEvent, bool)>> {
        let mut groups: Vec<Vec<&'a (MusicalEvent, bool)>> = Vec::new();
        if events.is_empty() {
            return groups;
        }

        let mut current_group: Vec<&'a (MusicalEvent, bool)> = Vec::new();
        let mut current_tuplet_duration = Fraction::new(0, 1);
        let mut tuplet_total_duration = None;

        for event_tuple in events {
            let (event, _) = event_tuple;
            let event_tuplet_info = event.duration.to_tuplet_info();

            // Check if we need to start a new group
            if !current_group.is_empty() {
                let (last_event_in_group, _) = current_group.last().unwrap();
                let last_tuplet_info = last_event_in_group.duration.to_tuplet_info();

                let mut split = false;

                // Compare tuplet ratios to determine if this event belongs in the same group
                let last_ratio = last_tuplet_info
                    .as_ref()
                    .map(|i| (i.tuplet_number, i.normal_number));
                let current_ratio = event_tuplet_info
                    .as_ref()
                    .map(|i| (i.tuplet_number, i.normal_number));

                if current_ratio != last_ratio {
                    split = true;
                } else if let Some(total_dur) = tuplet_total_duration {
                    // Split if we've exceeded the tuplet duration
                    if current_tuplet_duration >= total_dur {
                        split = true;
                    }
                }

                if split {
                    groups.push(std::mem::take(&mut current_group));
                    current_tuplet_duration = Fraction::new(0, 1);
                    tuplet_total_duration = None;
                }
            }

            // Initialize tuplet tracking for new groups
            if current_group.is_empty() {
                if let Some(info) = &event_tuplet_info {
                    let tuplet_unit_duration = info.tuplet_duration();
                    tuplet_total_duration = Some(tuplet_unit_duration * info.tuplet_number);
                }
            }

            // Track cumulative duration within tuplets
            if event_tuplet_info.is_some() {
                current_tuplet_duration += event.duration.fraction;
            }

            current_group.push(event_tuple);
        }

        if !current_group.is_empty() {
            groups.push(current_group);
        }

        groups
    }

    /// Process events to add ties where necessary.
    ///
    /// This function handles two types of tie insertion:
    /// 1. **Measure ties**: Notes that cross measure boundaries are split and tied
    /// 2. **Tuplet ties**: Notes that extend beyond tuplet boundaries are split and tied
    ///
    /// The function ensures that the resulting notation follows LilyPond conventions
    /// for proper measure organization and tuplet grouping.
    ///
    /// # Arguments
    ///
    /// * `events` - The musical events to process
    /// * `time_signature` - Optional time signature for measure boundary calculations
    ///
    /// # Returns
    ///
    /// A vector of events with tie information, where the boolean indicates
    /// whether the event should be tied to the next event
    fn tie_events(
        &self,
        events: &[MusicalEvent],
        time_signature: Option<(u32, u32)>,
    ) -> Vec<(MusicalEvent, bool)> {
        let mut processed_events: Vec<(MusicalEvent, bool)> = flatten_events(events)
            .into_iter()
            .map(|e| (e, false))
            .collect();

        // Phase 1: Handle measure boundary ties
        if let Some((num, den)) = time_signature {
            let measure_duration = Fraction::new(num, den);
            let mut i = 0;
            while i < processed_events.len() {
                let (event, _) = processed_events[i].clone();
                // Skip rests - they don't need to be tied
                if matches!(event.content, EventContent::Rest) {
                    i += 1;
                    continue;
                }

                let event_offset = event.offset;
                let event_end = event_offset + event.duration.fraction;

                // Calculate which measure this event starts in
                let measure_index = (event_offset / measure_duration).to_f64().floor() as u32;
                let measure_start_offset = measure_duration * measure_index;
                let measure_end_offset = measure_start_offset + measure_duration;

                // If the event crosses a measure boundary, split it
                if event_end > measure_end_offset {
                    let duration_in_measure = measure_end_offset - event_offset;
                    if duration_in_measure.numerator > 0 {
                        // Create first part (in current measure)
                        let mut event1 = event.clone();
                        event1.duration = Duration::from_fraction(
                            duration_in_measure.numerator,
                            duration_in_measure.denominator,
                        );

                        // Create second part (in next measure)
                        let remaining_duration = event.duration.fraction - duration_in_measure;
                        let mut event2 = event.clone();
                        event2.duration = Duration::from_fraction(
                            remaining_duration.numerator,
                            remaining_duration.denominator,
                        );
                        event2.offset = event.offset + duration_in_measure;

                        // Mark first event as tied
                        processed_events[i] = (event1, true);
                        processed_events.insert(i + 1, (event2, false));
                    }
                }
                i += 1;
            }
        }

        // Phase 2: Handle tuplet boundary ties
        let mut i = 0;
        let mut tuplet_debt = Fraction::new(0, 1);

        while i < processed_events.len() {
            let (event, _) = processed_events[i].clone();

            if tuplet_debt.numerator > 0 {
                if event.duration.fraction > tuplet_debt {
                    let mut event1 = event.clone();
                    event1.duration =
                        Duration::from_fraction(tuplet_debt.numerator, tuplet_debt.denominator);

                    let mut event2 = event.clone();
                    event2.duration = Duration::from_fraction(
                        (event.duration.fraction - tuplet_debt).numerator,
                        (event.duration.fraction - tuplet_debt).denominator,
                    );
                    event2.offset = event.offset + tuplet_debt;

                    processed_events[i] = (event1, true);
                    processed_events.insert(i + 1, (event2, false));

                    tuplet_debt = Fraction::new(0, 1);
                } else {
                    tuplet_debt -= event.duration.fraction;
                }
            }

            if let Some(tuplet_info) = event.duration.to_tuplet_info() {
                let tuplet_unit_duration = tuplet_info.tuplet_duration();
                let tuplet_total_duration = tuplet_unit_duration * tuplet_info.tuplet_number;

                let mut current_tuplet_duration = Fraction::new(0, 1);
                let mut k = i;
                let mut tuplet_start_offset = event.offset;

                while k < processed_events.len() {
                    let (current_event, _) = &processed_events[k];

                    if let Some(current_tuplet_info) = current_event.duration.to_tuplet_info() {
                        if (
                            current_tuplet_info.tuplet_number,
                            current_tuplet_info.normal_number,
                        ) != (tuplet_info.tuplet_number, tuplet_info.normal_number)
                        {
                            break;
                        }
                    }

                    if current_tuplet_duration.numerator == 0 {
                        tuplet_start_offset = current_event.offset;
                    } else {
                        let expected_offset = tuplet_start_offset + current_tuplet_duration;
                        if (current_event.offset - expected_offset).to_f64().abs() > 1e-9 {
                            break;
                        }
                    }

                    let remaining_duration_in_tuplet =
                        tuplet_total_duration - current_tuplet_duration;

                    if remaining_duration_in_tuplet.numerator == 0 {
                        break;
                    }

                    if current_event.duration.fraction <= remaining_duration_in_tuplet {
                        current_tuplet_duration += current_event.duration.fraction;
                        k += 1;
                    } else {
                        let duration_for_tuplet = remaining_duration_in_tuplet;
                        let leftover_duration =
                            current_event.duration.fraction - duration_for_tuplet;

                        let mut event1 = current_event.clone();
                        event1.duration = Duration::from_fraction(
                            duration_for_tuplet.numerator,
                            duration_for_tuplet.denominator,
                        );

                        let mut event2 = current_event.clone();
                        event2.duration = Duration::from_fraction(
                            leftover_duration.numerator,
                            leftover_duration.denominator,
                        );
                        event2.offset = current_event.offset + duration_for_tuplet;

                        processed_events[k] = (event1, true);
                        processed_events.insert(k + 1, (event2, false));
                        k += 1;
                        break;
                    }
                }
                if i == k {
                    i += 1;
                } else {
                    i = k;
                }
            } else {
                i += 1;
            }
        }

        processed_events
    }

    /// Format a sequence of musical events into LilyPond notation.
    ///
    /// This is the main method for converting a sequence of musical events into
    /// properly formatted LilyPond notation. It handles tie insertion, event
    /// segmentation for tuplets, and generates appropriate LilyPond syntax.
    ///
    /// # Arguments
    ///
    /// * `events` - The musical events to format
    /// * `time_signature` - Optional time signature for measure-aware formatting
    ///
    /// # Returns
    ///
    /// A formatted LilyPond string representing the musical sequence
    ///
    /// # Process
    ///
    /// 1. Insert ties where events cross boundaries
    /// 2. Segment events into logical groups (especially for tuplets)
    /// 3. Format each segment with appropriate LilyPond syntax
    /// 4. Combine segments into the final notation string
    fn format_musical_sequence(
        &self,
        events: &[MusicalEvent],
        time_signature: Option<(u32, u32)>,
    ) -> Result<String> {
        let tied_events = self.tie_events(events, time_signature);
        let segments = self.segment_events(&tied_events, time_signature);
        let mut music_parts = Vec::new();

        for segment in segments.iter() {
            if segment.is_empty() {
                continue;
            }

            // Format each event in the segment
            let mut segment_parts = Vec::new();
            for &(event, tied) in segment.iter() {
                let mut event_str = self.format_event(event)?;
                if *tied {
                    event_str.push('~'); // Add tie symbol
                }
                segment_parts.push(event_str);
            }

            // Handle tuplet formatting if this segment contains tuplets
            if let Some(first_tuplet_info) = segment[0].0.duration.to_tuplet_info() {
                // Find the shortest note in the tuplet to determine base duration
                let min_duration_event = segment
                    .iter()
                    .min_by(|a, b| a.0.duration.fraction.cmp(&b.0.duration.fraction))
                    .unwrap();

                let representative_tuplet_info = min_duration_event
                    .0
                    .duration
                    .to_tuplet_info()
                    .unwrap_or(first_tuplet_info);

                let tuplet_base_duration = representative_tuplet_info.base_duration
                    / representative_tuplet_info.normal_number;

                music_parts.push(format!(
                    "\\tuplet {}/{} {} {{ {} }}",
                    representative_tuplet_info.tuplet_number,
                    representative_tuplet_info.normal_number,
                    tuplet_base_duration,
                    segment_parts.join(" ")
                ));
            } else {
                // Regular (non-tuplet) notation
                music_parts.push(segment_parts.join(" "));
            }
        }
        Ok(music_parts.join(" "))
    }

    /// Format a single musical event into LilyPond notation.
    ///
    /// Converts an individual musical event (note, chord, or rest) into the
    /// corresponding LilyPond notation, including duration, dynamics, and articulations.
    ///
    /// # Arguments
    ///
    /// * `event` - The musical event to format
    ///
    /// # Returns
    ///
    /// A formatted LilyPond string for the event
    ///
    /// # Errors
    ///
    /// Returns an error if the event contains unexpected sequence content or
    /// if duration formatting fails
    fn format_event(&self, event: &MusicalEvent) -> Result<String> {
        let content_str = match &event.content {
            EventContent::Note(pitch) => self.format_pitch(pitch)?,
            EventContent::Chord(chord) => self.format_chord(chord)?,
            EventContent::Rest => "r".to_string(),
            EventContent::Sequence(_) => {
                return Err(ErrorSource::Runtime(
                    "Unexpected sequence in format_event. Events should be flattened first."
                        .to_string(),
                )
                .with_span(Span::single(Position::start())));
            }
        };

        let duration_str = self.format_duration(&event.duration)?;
        let dynamic_str = self.format_dynamic(event);
        let attribute_str = self.format_attributes(event);

        Ok(format!(
            "{content_str}{duration_str}{attribute_str}{dynamic_str}"
        ))
    }

    /// Format a musical pitch into LilyPond notation.
    ///
    /// Converts a pitch to LilyPond's absolute pitch notation, including
    /// octave marks and accidental handling.
    ///
    /// # Arguments
    ///
    /// * `pitch` - The pitch to format
    ///
    /// # Returns
    ///
    /// A LilyPond pitch string (e.g., "c'", "fis,", "bes''")
    fn format_pitch(&self, pitch: &Pitch) -> Result<String> {
        let note_name = pitch.pitch_class.to_note_name().replace('#', "is");

        let octave_marks = self.get_octave_marks(pitch.octave);
        Ok(format!("{note_name}{octave_marks}"))
    }

    /// Generate LilyPond octave marks for the given octave number.
    ///
    /// LilyPond uses apostrophes (') for higher octaves and commas (,) for lower octaves,
    /// relative to octave 3 as the reference.
    ///
    /// # Arguments
    ///
    /// * `octave` - The octave number (3 is the reference octave)
    ///
    /// # Returns
    ///
    /// A string containing the appropriate octave marks
    fn get_octave_marks(&self, octave: i8) -> String {
        let reference_octave = 3;
        let octave_diff = octave - reference_octave;
        if octave_diff > 0 {
            "'".repeat(octave_diff as usize)
        } else if octave_diff < 0 {
            ",".repeat(-octave_diff as usize)
        } else {
            "".to_string()
        }
    }

    /// Format a musical duration into LilyPond notation.
    ///
    /// Converts duration values to LilyPond's duration notation, handling both
    /// regular durations (with possible dots) and tuplet durations.
    ///
    /// # Arguments
    ///
    /// * `duration` - The duration to format
    ///
    /// # Returns
    ///
    /// A LilyPond duration string (e.g., "4", "2.", "8")
    ///
    /// # Errors
    ///
    /// Returns an error if the duration cannot be represented in standard
    /// LilyPond notation (e.g., complex fractional durations)
    fn format_duration(&self, duration: &Duration) -> Result<String> {
        // Handle tuplet durations
        if let Some(info) = duration.to_tuplet_info() {
            return Ok(format!("{}", info.base_duration));
        }

        // Try to represent as standard duration with dots
        let f = duration.fraction;
        for dots in 0..=4 {
            let multiplier = (1 << (dots + 1)) - 1; // 1, 3, 7, 15, 31 for 0-4 dots
            let num = f.denominator as u64 * multiplier as u64;
            let den = f.numerator as u64 * (1 << dots) as u64;

            if num % den == 0 {
                let d = num / den;
                // Check if d is a power of 2 (valid LilyPond duration)
                if d > 0 && (d & (d - 1)) == 0 {
                    return Ok(format!("{}{}", d, ".".repeat(dots)));
                }
            }
        }
        Err(ErrorSource::Runtime(format!(
            "Complex duration formatting for {} not supported.",
            duration.to_fractional_string()
        ))
        .with_span(Span::single(Position::start())))
    }

    /// Format a chord into LilyPond notation.
    ///
    /// Converts a chord to LilyPond's chord notation using angle brackets.
    ///
    /// # Arguments
    ///
    /// * `chord` - The chord to format
    ///
    /// # Returns
    ///
    /// A LilyPond chord string (e.g., "<c e g>")
    fn format_chord(&self, chord: &Chord) -> Result<String> {
        let pitch_strs: Result<Vec<_>> =
            chord.pitches.iter().map(|p| self.format_pitch(p)).collect();
        Ok(format!("<{}>", pitch_strs?.join(" ")))
    }

    /// Convert a clef type to its LilyPond representation.
    ///
    /// # Arguments
    ///
    /// * `clef` - The clef type to convert
    ///
    /// # Returns
    ///
    /// The LilyPond clef name as a string
    fn format_clef(&self, clef: Clef) -> &'static str {
        match clef {
            Clef::Treble => "treble",
            Clef::Bass => "bass",
            Clef::Alto => "alto",
            Clef::Tenor => "tenor",
        }
    }

    /// Format dynamic markings for a musical event.
    ///
    /// Converts dynamic markings to LilyPond dynamic notation using backslash commands.
    ///
    /// # Arguments
    ///
    /// * `event` - The musical event containing dynamic information
    ///
    /// # Returns
    ///
    /// A LilyPond dynamic string (e.g., " \\f", " \\pp") or empty string if no dynamic
    fn format_dynamic(&self, event: &MusicalEvent) -> String {
        if let Some(d) = event.dynamic {
            format!(
                " \\{}",
                match d {
                    crate::core::Dynamic::Pppp => "pppp",
                    crate::core::Dynamic::Ppp => "ppp",
                    crate::core::Dynamic::Pp => "pp",
                    crate::core::Dynamic::P => "p",
                    crate::core::Dynamic::Mp => "mp",
                    crate::core::Dynamic::Mf => "mf",
                    crate::core::Dynamic::F => "f",
                    crate::core::Dynamic::Ff => "ff",
                    crate::core::Dynamic::Fff => "fff",
                    crate::core::Dynamic::Ffff => "ffff",
                    crate::core::Dynamic::Velocity(_) => "",
                }
            )
        } else {
            "".to_string()
        }
    }

    /// Format articulation attributes for a musical event.
    ///
    /// Converts articulation markings (staccato, accent, tenuto) to LilyPond notation.
    ///
    /// # Arguments
    ///
    /// * `event` - The musical event containing attribute information
    ///
    /// # Returns
    ///
    /// A LilyPond articulation string (e.g., " -.", " ->") or empty string if no attributes
    fn format_attributes(&self, event: &MusicalEvent) -> String {
        if event.attributes.is_empty() {
            return "".to_string();
        }
        let attrs: Vec<_> = event
            .attributes
            .iter()
            .map(|a| match a {
                Attribute::Staccato => "-.",
                Attribute::Accent => "->",
                Attribute::Tenuto => "--",
            })
            .collect();
        format!(" {}\n", attrs.join(" "))
    }
}

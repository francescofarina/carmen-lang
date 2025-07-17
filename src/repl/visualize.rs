use crate::common::fraction::Fraction;
use crate::core::{Duration, EventContent, Pitch, Score};
use crate::repl::ui::{BLUE, GREEN, RESET, YELLOW};
use std::collections::{BTreeMap, BTreeSet};

/// Width of the piano roll display in characters.
/// This determines the horizontal resolution of the visualization.
const PIANO_ROLL_WIDTH: usize = 80;

/// Internal representation of a note for piano roll visualization.
///
/// This struct flattens the hierarchical Carmen musical structure into
/// a simple time-based representation suitable for visual display.
#[derive(Debug, Clone, Copy)]
struct Note {
    pitch: Pitch,
    start_time: Fraction,
    duration: Duration,
}

/// Prints a visual piano roll representation of a Carmen Score.
///
/// This function creates a horizontal piano roll display where:
/// - Each row represents a different pitch (note name)
/// - Time flows from left to right
/// - Notes are represented as filled blocks (■)
/// - The display is colored for better readability
///
/// The visualization handles complex Score structures by flattening
/// all musical events into a time-based representation.
///
/// # Arguments
/// * `score` - The Carmen Score to visualize
pub fn print_score(score: &Score) {
    let all_notes = collect_notes(score);
    if all_notes.is_empty() {
        println!("Score has no notes to visualize.");
        return;
    }

    let (min_pitch, max_pitch) = find_pitch_range(&all_notes);
    let total_duration = score.total_duration();

    if total_duration == Fraction::new(0, 1) {
        println!("Score has no duration to visualize.");
        return;
    }

    let mut piano_roll = build_piano_roll(&all_notes, min_pitch, max_pitch, total_duration);

    print_piano_roll(&mut piano_roll, min_pitch, max_pitch);
}

/// Collects all notes from a Score's timeline and movements into a flat list.
///
/// This function traverses the entire Score hierarchy:
/// - Main timeline (if present)
/// - All movements with proper time offsets
///
/// Each movement is positioned sequentially after the previous ones,
/// ensuring correct temporal relationships in the visualization.
///
/// # Arguments
/// * `score` - The Score to extract notes from
///
/// # Returns
/// A vector of all notes with their absolute timing information
fn collect_notes(score: &Score) -> Vec<Note> {
    let mut notes = Vec::new();

    // Collect notes from the main timeline
    if let Some(timeline) = &score.timeline {
        for part in &timeline.parts {
            recursively_collect_notes_from_part(part, Fraction::new(0, 1), &mut notes);
        }
    }

    // Collect notes from movements, positioned sequentially
    let mut current_offset = Fraction::new(0, 1);
    for movement in &score.movements {
        let timeline = &movement.timeline;
        for part in &timeline.parts {
            recursively_collect_notes_from_part(part, current_offset, &mut notes);
        }
        current_offset += timeline.total_duration();
    }
    notes
}

/// Recursively extracts notes from a Part and all its nested components.
///
/// This traverses the Part hierarchy:
/// - Direct events on the Part
/// - Events in each Staff
/// - Events in each Voice within each Staff
///
/// All timing calculations include the base offset to position
/// the Part correctly within the larger Score timeline.
///
/// # Arguments
/// * `part` - The Part to extract notes from
/// * `base_offset` - Time offset to add to all events in this Part
/// * `notes` - Mutable vector to accumulate notes into
fn recursively_collect_notes_from_part(
    part: &crate::core::Part,
    base_offset: Fraction,
    notes: &mut Vec<Note>,
) {
    // Collect notes from direct Part events
    for event in &part.events {
        collect_notes_from_event(event, base_offset, notes);
    }
    // Collect notes from Staff/Voice hierarchy
    for staff in &part.staves {
        for voice in &staff.voices {
            for event in &voice.events {
                collect_notes_from_event(event, base_offset, notes);
            }
        }
    }
}

/// Extracts notes from a single MusicalEvent, handling different event types.
///
/// This function processes:
/// - Individual notes: Converted directly to Note struct
/// - Sequences: Recursively processed with updated time offset
/// - Other event types (chords, rests): Currently ignored for piano roll
///
/// # Arguments
/// * `event` - The MusicalEvent to extract notes from
/// * `base_offset` - Time offset to add to this event's timing
/// * `notes` - Mutable vector to accumulate notes into
fn collect_notes_from_event(
    event: &crate::core::MusicalEvent,
    base_offset: Fraction,
    notes: &mut Vec<Note>,
) {
    match &event.content {
        EventContent::Note(pitch) => {
            notes.push(Note {
                pitch: *pitch,
                start_time: base_offset + event.offset,
                duration: event.duration,
            });
        }
        EventContent::Sequence(events) => {
            // Recursively process sequence events with updated offset
            for seq_event in events {
                collect_notes_from_event(seq_event, base_offset + event.offset, notes);
            }
        }
        // TODO: Handle chords by extracting individual pitches
        // TODO: Rests could be visualized as empty space
        _ => {}
    }
}

/// Finds the lowest and highest pitches in a collection of notes.
///
/// This determines the vertical range needed for the piano roll display.
/// MIDI numbers are used for comparison to ensure proper pitch ordering.
///
/// # Arguments
/// * `notes` - Slice of notes to analyze
///
/// # Returns
/// A tuple of (lowest_pitch, highest_pitch)
///
/// # Panics
/// Panics if the notes slice is empty
fn find_pitch_range(notes: &[Note]) -> (Pitch, Pitch) {
    let mut min_pitch = notes[0].pitch;
    let mut max_pitch = notes[0].pitch;

    for note in notes {
        if note.pitch.to_midi() < min_pitch.to_midi() {
            min_pitch = note.pitch;
        }
        if note.pitch.to_midi() > max_pitch.to_midi() {
            max_pitch = note.pitch;
        }
    }

    (min_pitch, max_pitch)
}

/// Builds the piano roll data structure for visualization.
///
/// This creates a 2D grid where:
/// - Rows represent different pitches (keyed by MIDI number)
/// - Columns represent time slices across the total duration
/// - Notes are represented as filled characters (■) spanning their duration
///
/// The algorithm:
/// 1. Creates a row for each pitch in the range
/// 2. For each note, calculates its horizontal position and span
/// 3. Fills the appropriate cells with the note character
///
/// # Arguments
/// * `notes` - All notes to visualize
/// * `min_pitch` - Lowest pitch (bottom of display)
/// * `max_pitch` - Highest pitch (top of display)
/// * `total_duration` - Total time span for horizontal scaling
///
/// # Returns
/// A BTreeMap where keys are MIDI numbers and values are character rows
fn build_piano_roll(
    notes: &[Note],
    min_pitch: Pitch,
    max_pitch: Pitch,
    total_duration: Fraction,
) -> BTreeMap<i32, Vec<char>> {
    let mut roll = BTreeMap::new();
    let pitch_range: Vec<_> = (min_pitch.to_midi()..=max_pitch.to_midi())
        .map(Pitch::from_midi)
        .collect();

    // Create a row for each pitch in the range
    for pitch in pitch_range {
        let mut row = vec![' '; PIANO_ROLL_WIDTH];

        // Fill in notes that match this pitch
        for note in notes {
            if note.pitch.to_midi() == pitch.to_midi() {
                // Calculate horizontal position as fraction of total time
                let start_col = ((note.start_time / total_duration).to_f64()
                    * PIANO_ROLL_WIDTH as f64) as usize;
                let end_col = (((note.start_time + note.duration.fraction) / total_duration)
                    .to_f64()
                    * PIANO_ROLL_WIDTH as f64) as usize;

                // Fill the duration span with note characters
                for a in row
                    .iter_mut()
                    .take(end_col.min(PIANO_ROLL_WIDTH))
                    .skip(start_col)
                {
                    *a = '■';
                }
            }
        }
        roll.insert(pitch.to_midi() as i32, row);
    }
    roll
}

/// Prints the completed piano roll to the terminal with color formatting.
///
/// # Arguments
/// * `piano_roll` - The completed piano roll data structure
/// * `min_pitch` - Lowest pitch (for generating the complete range)
/// * `max_pitch` - Highest pitch (for generating the complete range)
fn print_piano_roll(piano_roll: &mut BTreeMap<i32, Vec<char>>, min_pitch: Pitch, max_pitch: Pitch) {
    // Create sorted list of (MIDI_number, note_name) pairs
    let pitch_names: BTreeSet<_> = (min_pitch.to_midi()..=max_pitch.to_midi())
        .map(|p| {
            let pitch = Pitch::from_midi(p);
            (p, pitch.to_note_name())
        })
        .collect();

    // Print from highest to lowest pitch (musical convention)
    for (pitch_val, note_name) in pitch_names.iter().rev() {
        if let Some(row) = piano_roll.get_mut(&(*pitch_val as i32)) {
            // Apply color formatting to the row
            let colored_row: String = row
                .iter()
                .map(|&c| {
                    if c == '■' {
                        format!("{GREEN}{c}{RESET}")
                    } else {
                        c.to_string()
                    }
                })
                .collect();
            println!("{YELLOW}{note_name:>5}{RESET} |{BLUE}{colored_row}");
        }
    }
}

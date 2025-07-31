//! Core music theory types and functionality.
//!
//! This module provides fundamental music theory concepts including:
//! - Pitch classes and pitches with MIDI conversion
//! - Pitch class sets with set theory operations (normal form, prime form, interval class vectors)
//! - Musical durations with dotted notes and tuplet support
//! - Musical events, voices, staves, parts, and complete scores
//! - Key signatures, dynamics, and musical attributes
//! - Transposition and inversion operations

pub mod traits;

use crate::common::fraction::{Fraction, TupletInfo};
use crate::errors::{CoreResult, ErrorSource};
use std::cmp::Ordering;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Mul};
use std::str::FromStr;
pub use traits::{Interval, Invert, Transpose};

/// Represents a pitch class in the chromatic scale (0-11).
///
/// A pitch class is an equivalence class of pitches that differ by octaves.
/// For example, all c notes (c4, c5, c6, etc.) belong to pitch class 0.
/// The 12 pitch classes correspond to the 12 semitones in an octave:
/// c=0, c#=1, d=2, d#=3, e=4, f=5, f#=6, g=7, g#=8, a=9, a#=10, b=11.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PitchClass(pub u8);

impl PitchClass {
    /// Creates a new pitch class, automatically wrapping values to 0-11 range.
    pub fn new(value: u8) -> Self {
        Self(value % 12)
    }

    /// Creates a pitch class from a note name and accidentals.
    ///
    /// # Arguments
    /// * `note` - Base note name (c, d, e, f, g, a, b), case insensitive
    /// * `accidentals` - Number of semitones to adjust (positive for sharps, negative for flats)
    pub fn from_note_name(note: &str, accidentals: i32) -> CoreResult<Self> {
        let base_pitch_class = match note.to_lowercase().as_str() {
            "c" => 0,
            "d" => 2,
            "e" => 4,
            "f" => 5,
            "g" => 7,
            "a" => 9,
            "b" => 11,
            _ => return Err(ErrorSource::Music(format!("Invalid note name: {note}"))),
        };

        let pitch_class = ((base_pitch_class + accidentals) % 12 + 12) % 12;
        Ok(Self::new(pitch_class as u8))
    }

    /// Converts the pitch class to its enharmonic note name using sharps.
    ///
    /// Returns the simplest sharp-based representation of the pitch class.
    pub fn to_note_name(&self) -> String {
        let note_names = [
            "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b",
        ];
        note_names[self.0 as usize].to_string()
    }

    /// Calculates the interval class (smallest distance) between two pitch classes.
    ///
    /// Interval classes range from 0-6, representing the shortest path between
    /// pitch classes regardless of direction (up or down).
    pub fn interval_class_to(&self, other: &PitchClass) -> u8 {
        let interval = self.interval_to(other);
        std::cmp::min(interval, 12 - interval)
    }
}

impl Transpose for PitchClass {
    fn transpose(&self, semitones: i32) -> Self {
        let new_value = ((self.0 as i32 + semitones) % 12 + 12) % 12;
        Self(new_value as u8)
    }
}

impl Invert<PitchClass> for PitchClass {
    fn invert(&self, axis: &PitchClass) -> Self {
        let inverted = ((axis.0 as i32 - self.0 as i32) % 12 + 12) % 12;
        Self(inverted as u8)
    }
}

impl Interval for PitchClass {
    type Output = u8;
    fn interval_to(&self, other: &PitchClass) -> Self::Output {
        (((other.0 as i32 - self.0 as i32) % 12 + 12) % 12) as u8
    }
}

/// Represents a specific pitch with both pitch class and octave information.
///
/// A pitch combines a pitch class (0-11) with an octave number to specify
/// an exact frequency.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pitch {
    pub pitch_class: PitchClass,
    pub octave: i8,
}

impl Pitch {
    /// Creates a pitch from a MIDI note number (0-127).
    ///
    /// MIDI note 60 = c4 (middle C), 69 = a4 (440 Hz), etc.
    pub fn from_midi(midi_note: u8) -> Self {
        Self {
            pitch_class: PitchClass::new(midi_note % 12),
            octave: (midi_note / 12) as i8 - 1,
        }
    }

    /// Converts the pitch to a MIDI note number (0-127).
    ///
    /// Values are clamped to the valid MIDI range.
    pub fn to_midi(&self) -> u8 {
        ((self.octave + 1) as u8 * 12 + self.pitch_class.0).clamp(0, 127)
    }

    /// Creates a pitch from note name, accidentals, and octave.
    pub fn from_note_name(note: &str, accidentals: i32, octave: i8) -> CoreResult<Self> {
        Ok(Self {
            pitch_class: PitchClass::from_note_name(note, accidentals)?,
            octave,
        })
    }

    pub fn to_note_name(&self) -> String {
        format!("{}{}", self.pitch_class.to_note_name(), self.octave)
    }
}

impl Interval for Pitch {
    type Output = i32;
    fn interval_to(&self, other: &Pitch) -> Self::Output {
        other.to_midi() as i32 - self.to_midi() as i32
    }
}

impl Transpose for Pitch {
    fn transpose(&self, semitones: i32) -> Self {
        let new_midi = (self.to_midi() as i32 + semitones).clamp(0, 127) as u8;
        Self::from_midi(new_midi)
    }
}

impl Invert<PitchClass> for Pitch {
    fn invert(&self, axis: &PitchClass) -> Self {
        let inverted_pc = self.pitch_class.invert(axis);
        Self {
            pitch_class: inverted_pc,
            octave: self.octave,
        }
    }
}

impl Invert<Pitch> for Pitch {
    fn invert(&self, axis: &Pitch) -> Self {
        let axis_midi = axis.to_midi() as i32;
        let self_midi = self.to_midi() as i32;
        let inverted_midi = (2 * axis_midi - self_midi).clamp(0, 127) as u8;
        Self::from_midi(inverted_midi)
    }
}

/// A collection of unique pitch classes, used in atonal music analysis.
///
/// Provides set theory operations like normal form, prime form, and interval class vectors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PitchClassSet {
    pub classes: HashSet<PitchClass>,
}

impl Hash for PitchClassSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Convert to sorted vector for consistent hashing
        let mut classes: Vec<u8> = self.classes.iter().map(|pc| pc.0).collect();
        classes.sort();
        classes.hash(state);
    }
}

impl Ord for PitchClassSet {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare based on normal form
        let self_normal: Vec<u8> = self.classes.iter().map(|pc| pc.0).collect::<Vec<_>>();
        let other_normal: Vec<u8> = other.classes.iter().map(|pc| pc.0).collect::<Vec<_>>();

        let mut self_sorted = self_normal;
        let mut other_sorted = other_normal;
        self_sorted.sort();
        other_sorted.sort();

        self_sorted.cmp(&other_sorted)
    }
}

impl PartialOrd for PitchClassSet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PitchClassSet {
    pub fn new(classes: Vec<PitchClass>) -> Self {
        Self {
            classes: classes.into_iter().collect(),
        }
    }

    pub fn from_u8_values(values: Vec<u8>) -> Self {
        let classes = values.into_iter().map(PitchClass::new).collect();
        Self { classes }
    }

    pub fn from_pitches(pitches: &[Pitch]) -> Self {
        let classes = pitches.iter().map(|p| p.pitch_class).collect();
        Self { classes }
    }

    /// Computes the normal form of the pitch class set.
    ///
    /// Normal form is the most compact ordering of the set, starting from 0,
    /// with the smallest possible span. This provides a canonical representation
    /// for comparing sets regardless of transposition.
    pub fn normal_form(&self) -> PitchClassSet {
        if self.classes.is_empty() {
            return PitchClassSet::new(Vec::new());
        }

        let mut classes: Vec<u8> = self.classes.iter().map(|pc| pc.0).collect();
        classes.sort();

        let mut best_rotation = Vec::new();
        let mut best_span = 12; // Initialize to maximum possible span

        for i in 0..classes.len() {
            let mut rotation = classes[i..].to_vec();
            rotation.extend_from_slice(&classes[..i]);

            // Normalize to start from 0
            let first = rotation[0];
            let normalized: Vec<u8> = rotation
                .iter()
                .map(|&x| (((x as i32 - first as i32) % 12 + 12) % 12) as u8)
                .collect();

            let span = self.span(&normalized);
            if span < best_span || (span == best_span && normalized < best_rotation) {
                best_rotation = normalized;
                best_span = span;
            }
        }

        PitchClassSet::from_u8_values(best_rotation)
    }

    /// Computes the prime form of the pitch class set.
    ///
    /// Prime form is the most compact form among the set and its inversion,
    /// providing a canonical representation that's invariant under both
    /// transposition and inversion. This is used to identify set classes.
    pub fn prime_form(&self) -> PitchClassSet {
        if self.classes.is_empty() {
            return PitchClassSet::new(Vec::new());
        }

        let normal = self.normal_form();
        let inverted = self.invert(&PitchClass::new(0)).normal_form();

        if inverted < normal {
            inverted
        } else {
            normal
        }
    }

    /// Generates all members of the set class (transpositions and inversions).
    ///
    /// Returns all 24 possible transformations (12 transpositions × 2 for inversion)
    /// of the set, which together form the complete set class.
    pub fn set_class(&self) -> Vec<PitchClassSet> {
        let mut result = Vec::new();
        let mut seen = std::collections::HashSet::new();

        // Generate all transpositions and inversions
        for t in 0..12 {
            // Transposition
            let transposed = self.transpose(t);
            if seen.insert(transposed.clone()) {
                result.push(transposed);
            }

            // Inversion then transposition
            for axis in 0..12 {
                let inverted = self.invert(&PitchClass::new(axis)).transpose(t);
                if seen.insert(inverted.clone()) {
                    result.push(inverted);
                }
            }
        }

        // Sort by the set itself (using the Ord implementation)
        result.sort();
        result
    }

    fn span(&self, classes: &[u8]) -> u8 {
        if classes.is_empty() {
            return 0;
        }
        (((classes[classes.len() - 1] as i32 - classes[0] as i32) % 12 + 12) % 12) as u8
    }

    /// Calculates the interval class vector of the set.
    ///
    /// Returns a 6-element array counting occurrences of each interval class (1-6)
    /// between all pairs of pitch classes in the set. This provides a unique
    /// "fingerprint" for many set classes, useful for analysis and comparison.
    ///
    /// # Returns
    /// Array where index 0 = minor seconds/major sevenths (ic1),
    /// index 1 = major seconds/minor sevenths (ic2), etc.
    pub fn interval_class_vector(&self) -> [u8; 6] {
        let mut icv = [0; 6];
        let classes: Vec<PitchClass> = self.classes.iter().cloned().collect();

        for i in 0..classes.len() {
            for j in (i + 1)..classes.len() {
                let pc1 = classes[i];
                let pc2 = classes[j];
                let ic = pc1.interval_class_to(&pc2);
                if ic > 0 && ic <= 6 {
                    icv[ic as usize - 1] += 1;
                }
            }
        }

        icv
    }
}

impl Transpose for PitchClassSet {
    fn transpose(&self, semitones: i32) -> Self {
        let classes = self
            .classes
            .iter()
            .map(|pc| pc.transpose(semitones))
            .collect();
        Self { classes }
    }
}

impl Invert<PitchClass> for PitchClassSet {
    fn invert(&self, axis: &PitchClass) -> Self {
        let classes = self.classes.iter().map(|pc| pc.invert(axis)).collect();
        Self { classes }
    }
}

/// Represents a musical duration as a fraction of a whole note.
///
/// Supports standard note values (whole, half, quarter, etc.), dotted notes,
/// and complex tuplets. All durations are stored as exact fractions to avoid
/// floating-point precision issues.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Duration {
    pub fraction: Fraction,
}

impl Duration {
    pub fn from_fraction(numerator: u32, denominator: u32) -> Self {
        Self {
            fraction: Fraction::new(numerator, denominator),
        }
    }

    pub fn whole() -> Self {
        Self::from_fraction(1, 1)
    }

    pub fn half() -> Self {
        Self::from_fraction(1, 2)
    }

    pub fn quarter() -> Self {
        Self::from_fraction(1, 4)
    }

    pub fn eighth() -> Self {
        Self::from_fraction(1, 8)
    }

    pub fn sixteenth() -> Self {
        Self::from_fraction(1, 16)
    }

    /// Creates a dotted version of this duration.
    ///
    /// Each dot adds half the value of the previous duration.
    /// For example, a dotted quarter note = 1/4 + 1/8 = 3/8.
    pub fn with_dots(&self, dots: u32) -> Self {
        Self {
            fraction: self.fraction.with_dots(dots),
        }
    }

    pub fn fraction(&self) -> &Fraction {
        &self.fraction
    }

    /// Returns tuplet information if this duration represents a tuplet.
    pub fn to_tuplet_info(&self) -> Option<TupletInfo> {
        self.fraction.to_tuplet_info()
    }

    /// Returns true if this duration cannot be represented as a standard binary division.
    pub fn is_tuplet(&self) -> bool {
        !self.fraction.is_standard_binary()
    }

    pub fn to_fractional_string(&self) -> String {
        format!("{}", self.fraction)
    }

    pub fn to_f64(&self) -> f64 {
        self.fraction.to_f64()
    }
}

impl<'b> Add<&'b Duration> for &Duration {
    type Output = Duration;

    fn add(self, other: &'b Duration) -> Self::Output {
        Duration {
            fraction: self.fraction + other.fraction,
        }
    }
}

impl Mul<u32> for &Duration {
    type Output = Duration;

    fn mul(self, rhs: u32) -> Self::Output {
        Duration {
            fraction: self.fraction * rhs,
        }
    }
}

impl Mul<Fraction> for &Duration {
    type Output = Duration;

    fn mul(self, rhs: Fraction) -> Self::Output {
        Duration {
            fraction: self.fraction * rhs,
        }
    }
}

/// Musical dynamics (volume/intensity markings).
///
/// Represents standard dynamic markings from pppp (very very quiet) to ffff (very very loud),
/// plus the ability to specify exact MIDI velocity values.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Dynamic {
    Pppp,
    Ppp,
    Pp,
    P,
    Mp,
    Mf,
    F,
    Ff,
    Fff,
    Ffff,
    Velocity(u8), // MIDI velocity 0-127
}

impl FromStr for Dynamic {
    type Err = ErrorSource;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pppp" => Ok(Dynamic::Pppp),
            "ppp" => Ok(Dynamic::Ppp),
            "pp" => Ok(Dynamic::Pp),
            "p" => Ok(Dynamic::P),
            "mp" => Ok(Dynamic::Mp),
            "mf" => Ok(Dynamic::Mf),
            "f" => Ok(Dynamic::F),
            "ff" => Ok(Dynamic::Ff),
            "fff" => Ok(Dynamic::Fff),
            "ffff" => Ok(Dynamic::Ffff),
            _ => Err(ErrorSource::Argument(format!("Invalid dynamic: {s}"))),
        }
    }
}

impl Dynamic {
    /// Converts the dynamic marking to a MIDI velocity value (0-127).
    pub fn to_velocity(&self) -> u8 {
        match self {
            Dynamic::Pppp => 8,
            Dynamic::Ppp => 16,
            Dynamic::Pp => 32,
            Dynamic::P => 48,
            Dynamic::Mp => 64,
            Dynamic::Mf => 80,
            Dynamic::F => 96,
            Dynamic::Ff => 112,
            Dynamic::Fff => 120,
            Dynamic::Ffff => 127,
            Dynamic::Velocity(v) => *v,
        }
    }
}

/// Musical articulation and expression attributes.
///
/// These modify how notes are played or performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Attribute {
    Staccato,
    Accent,
    Tenuto,
}

impl FromStr for Attribute {
    type Err = ErrorSource;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "staccato" => Ok(Attribute::Staccato),
            "accent" => Ok(Attribute::Accent),
            "tenuto" => Ok(Attribute::Tenuto),
            _ => Err(ErrorSource::Argument(format!("Invalid attribute: {s}"))),
        }
    }
}

/// A collection of pitches played simultaneously.
#[derive(Debug, Clone, PartialEq)]
pub struct Chord {
    pub pitches: Vec<Pitch>,
}

impl Chord {
    pub fn to_pitch_class_set(&self) -> PitchClassSet {
        PitchClassSet::from_pitches(&self.pitches)
    }
}

impl Transpose for Chord {
    fn transpose(&self, semitones: i32) -> Self {
        let pitches = self
            .pitches
            .iter()
            .map(|p| p.transpose(semitones))
            .collect();
        Self { pitches }
    }
}

impl Invert<PitchClass> for Chord {
    fn invert(&self, axis: &PitchClass) -> Self {
        let pitches = self.pitches.iter().map(|p| p.invert(axis)).collect();
        Self { pitches }
    }
}

impl Invert<Pitch> for Chord {
    fn invert(&self, axis: &Pitch) -> Self {
        let pitches = self.pitches.iter().map(|p| p.invert(axis)).collect();
        Self { pitches }
    }
}

/// The musical content of an event - what is actually played or heard.
#[derive(Debug, Clone, PartialEq)]
pub enum EventContent {
    Note(Pitch),                 // A single note
    Chord(Chord),                // Multiple notes played together
    Rest,                        // Rest
    Sequence(Vec<MusicalEvent>), // A sequence of sub-events
}

/// A complete musical event with timing, content, and expression information.
///
/// This represents any musical occurrence - notes, chords, rests, or sequences
/// of other events. It includes all the information needed to render the event
/// in a musical context.
#[derive(Debug, Clone, PartialEq)]
pub struct MusicalEvent {
    pub duration: Duration,         // How long the event lasts
    pub content: EventContent,      // What is played (note, chord, rest, sequence)
    pub dynamic: Option<Dynamic>,   // Volume/intensity
    pub attributes: Vec<Attribute>, // Aattributes
    pub offset: Fraction,           // Timing offset from the expected position
}

impl MusicalEvent {
    fn new(duration: Duration, content: EventContent) -> Self {
        Self {
            duration,
            content,
            dynamic: None,
            attributes: Vec::new(),
            offset: Fraction::new(0, 1),
        }
    }

    pub fn note(pitch: Pitch, duration: Duration) -> Self {
        Self::new(duration, EventContent::Note(pitch))
    }

    pub fn chord(chord: Chord, duration: Duration) -> Self {
        Self::new(duration, EventContent::Chord(chord))
    }

    pub fn rest(duration: Duration) -> Self {
        Self::new(duration, EventContent::Rest)
    }

    pub fn set_dynamic(&mut self, dynamic: Dynamic) {
        self.dynamic = Some(dynamic);
    }

    pub fn with_dynamic(mut self, dynamic: Dynamic) -> Self {
        self.set_dynamic(dynamic);
        self
    }

    pub fn add_attribute(&mut self, attribute: Attribute) {
        self.attributes.push(attribute);
    }

    pub fn with_attribute(mut self, attribute: Attribute) -> Self {
        self.add_attribute(attribute);
        self
    }

    pub fn set_attributes(&mut self, attributes: Vec<Attribute>) {
        self.attributes = attributes;
    }

    pub fn with_attributes(mut self, attributes: Vec<Attribute>) -> Self {
        self.set_attributes(attributes);
        self
    }

    pub fn set_offset(&mut self, offset: Fraction) {
        self.offset = offset;
    }

    pub fn with_offset(mut self, offset: Fraction) -> Self {
        self.set_offset(offset);
        self
    }

    /// Counts the number of atomic (non-sequence) events contained within this event.
    pub fn atomic_event_count(&self) -> usize {
        match &self.content {
            EventContent::Sequence(events) => events.iter().map(|e| e.atomic_event_count()).sum(),
            _ => 1,
        }
    }

    /// Calculates the total duration including any nested sequences.
    pub fn total_duration(&self) -> Fraction {
        match &self.content {
            EventContent::Sequence(events) => {
                if let Some(last_event) = events.last() {
                    last_event.offset + last_event.total_duration()
                } else {
                    Fraction::new(0, 1)
                }
            }
            _ => self.duration.fraction,
        }
    }
}

impl Transpose for MusicalEvent {
    fn transpose(&self, semitones: i32) -> Self {
        let content = match &self.content {
            EventContent::Note(pitch) => EventContent::Note(pitch.transpose(semitones)),
            EventContent::Chord(chord) => EventContent::Chord(chord.transpose(semitones)),
            EventContent::Rest => EventContent::Rest,
            EventContent::Sequence(events) => {
                EventContent::Sequence(events.iter().map(|e| e.transpose(semitones)).collect())
            }
        };

        Self {
            content,
            duration: self.duration,
            dynamic: self.dynamic,
            attributes: self.attributes.clone(),
            offset: self.offset,
        }
    }
}

impl Invert<PitchClass> for MusicalEvent {
    fn invert(&self, axis: &PitchClass) -> Self {
        let content = match &self.content {
            EventContent::Note(pitch) => EventContent::Note(pitch.invert(axis)),
            EventContent::Chord(chord) => EventContent::Chord(chord.invert(axis)),
            EventContent::Rest => EventContent::Rest,
            EventContent::Sequence(events) => {
                EventContent::Sequence(events.iter().map(|e| e.invert(axis)).collect())
            }
        };

        Self {
            content,
            duration: self.duration,
            dynamic: self.dynamic,
            attributes: self.attributes.clone(),
            offset: self.offset,
        }
    }
}

impl Invert<Pitch> for MusicalEvent {
    fn invert(&self, axis: &Pitch) -> Self {
        let content = match &self.content {
            EventContent::Note(pitch) => EventContent::Note(pitch.invert(axis)),
            EventContent::Chord(chord) => EventContent::Chord(chord.invert(axis)),
            EventContent::Rest => EventContent::Rest,
            EventContent::Sequence(events) => {
                EventContent::Sequence(events.iter().map(|e| e.invert(axis)).collect())
            }
        };

        Self {
            content,
            duration: self.duration,
            dynamic: self.dynamic,
            attributes: self.attributes.clone(),
            offset: self.offset,
        }
    }
}

/// Musical key signatures defining the tonal center and scale.
#[derive(Debug, Clone, PartialEq)]
pub enum KeySignature {
    Major(u8),       // Major keys: 0=C, 1=G, 2=D, etc.
    Minor(u8),       // Minor keys: 0=A, 1=E, 2=B, etc.
    Custom(Vec<u8>), // Custom
}

impl std::fmt::Display for KeySignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeySignature::Major(val) => {
                let keys = [
                    "C", "G", "D", "A", "E", "B", "F#", "C#", "F", "Bb", "Eb", "Ab", "Db", "Gb",
                    "Cb",
                ];
                if (*val as usize) < keys.len() {
                    write!(f, "{} Major", keys[*val as usize])
                } else {
                    write!(f, "Unknown Major Key({val})")
                }
            }
            KeySignature::Minor(val) => {
                let keys = [
                    "A", "E", "B", "F#", "C#", "G#", "D#", "A#", "D", "G", "C", "F", "Bb", "Eb",
                    "Ab",
                ];
                if (*val as usize) < keys.len() {
                    write!(f, "{} Minor", keys[*val as usize])
                } else {
                    write!(f, "Unknown Minor Key({val})")
                }
            }
            KeySignature::Custom(vals) => write!(f, "Custom Key Signature {vals:?}"),
        }
    }
}

/// Keys for musical metadata that can change during a piece.
#[derive(Debug, Clone, PartialEq)]
pub enum MetadataKey {
    Title,
    Composer,
    Copyright,
    Tempo,
    TimeSignature,
    KeySignature,
    Clef,
    Instrument,
    Channel,
    Offset,
}

impl std::fmt::Display for MetadataKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetadataKey::Title => write!(f, "title"),
            MetadataKey::Composer => write!(f, "composer"),
            MetadataKey::Copyright => write!(f, "copyright"),
            MetadataKey::Tempo => write!(f, "tempo"),
            MetadataKey::TimeSignature => write!(f, "time_signature"),
            MetadataKey::KeySignature => write!(f, "key_signature"),
            MetadataKey::Clef => write!(f, "clef"),
            MetadataKey::Instrument => write!(f, "instrument"),
            MetadataKey::Channel => write!(f, "channel"),
            MetadataKey::Offset => write!(f, "offset"),
        }
    }
}

/// Values for musical metadata, supporting various data types.
#[derive(Debug, Clone, PartialEq)]
pub enum MetadataValue {
    String(String),
    Number(f64),
    TimeSignature(u32, u32),
    KeySignature(KeySignature),
    Clef(Clef),
}

impl std::fmt::Display for MetadataValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetadataValue::String(s) => write!(f, "\"{s}\""),
            MetadataValue::Number(n) => write!(f, "{n}"),
            MetadataValue::TimeSignature(num, den) => write!(f, "{num}/{den}"),
            MetadataValue::KeySignature(ks) => write!(f, "{ks}"),
            MetadataValue::Clef(c) => write!(f, "{c}"),
        }
    }
}

/// Represents a change in musical context (tempo, key signature, etc.) at a specific time.
#[derive(Debug, Clone, PartialEq)]
pub struct ContextChange {
    pub key: MetadataKey,      // What aspect of the context is changing
    pub value: MetadataValue,  // The new value
    pub time_offset: Fraction, // When the change occurs
}

/// Musical clefs that determine the pitch reference for staff notation.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Clef {
    #[default]
    Treble,
    Bass,
    Alto,
    Tenor,
}

impl std::fmt::Display for Clef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl FromStr for Clef {
    type Err = ErrorSource;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "treble" => Ok(Clef::Treble),
            "bass" => Ok(Clef::Bass),
            "alto" => Ok(Clef::Alto),
            "tenor" => Ok(Clef::Tenor),
            _ => Err(ErrorSource::Argument(format!("Invalid clef: {s}"))),
        }
    }
}

/// A single melodic line or voice within a musical staff.
///
/// In musical notation, multiple voices can share the same staff.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Voice {
    pub events: Vec<MusicalEvent>,
}

impl Voice {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_event(&mut self, event: MusicalEvent) {
        self.events.push(event);
    }

    pub fn with_event(mut self, event: MusicalEvent) -> Self {
        self.add_event(event);
        self
    }

    pub fn total_duration(&self) -> Fraction {
        self.events
            .iter()
            .map(|e| e.offset + e.total_duration())
            .max()
            .unwrap_or_else(|| Fraction::new(0, 1))
    }
}

/// A musical staff containing one or more voices.
#[derive(Debug, Clone, PartialEq)]
pub struct Staff {
    pub number: u32,                         // Staff number for ordering
    pub voices: Vec<Voice>,                  // Independent voices on this staff
    pub context_changes: Vec<ContextChange>, // Staff-specific context changes
}

impl Staff {
    pub fn new(number: u32) -> Self {
        Self {
            number,
            voices: Vec::new(),
            context_changes: Vec::new(),
        }
    }

    pub fn add_voice(mut self, voice: Voice) -> Self {
        self.voices.push(voice);
        self
    }

    pub fn add_context_change(&mut self, change: ContextChange) {
        self.context_changes.push(change);
    }

    pub fn with_context_change(mut self, change: ContextChange) -> Self {
        self.add_context_change(change);
        self
    }

    pub fn total_duration(&self) -> Fraction {
        self.voices
            .iter()
            .map(|v| v.total_duration())
            .max()
            .unwrap_or_else(|| Fraction::new(0, 1))
    }
}

/// A musical part representing a single instrument or voice in an ensemble.
///
/// Parts can contain events directly or organize them into multiple staves
/// (e.g., piano parts typically have two staves for left and right hands).
#[derive(Debug, Clone, PartialEq)]
pub struct Part {
    pub name: Option<String>,                // Display name of the part
    pub instrument: Option<String>,          // Instrument name
    pub channel: Option<u8>,                 // MIDI channel assignment
    pub offset: Fraction,                    // Timing offset for the entire part
    pub events: Vec<MusicalEvent>,           // Events not assigned to specific staves
    pub context_changes: Vec<ContextChange>, // Part-level context changes
    pub staves: Vec<Staff>,                  // Multiple staves (e.g., piano grand staff)
}

impl Default for Part {
    fn default() -> Self {
        Self {
            name: None,
            instrument: None,
            channel: None,
            offset: Fraction::new(0, 1),
            events: Vec::new(),
            context_changes: Vec::new(),
            staves: Vec::new(),
        }
    }
}

impl Part {
    pub fn new(name: Option<String>) -> Self {
        Self {
            name,
            instrument: None,
            channel: None,
            offset: Fraction::new(0, 1),
            events: Vec::new(),
            context_changes: Vec::new(),
            staves: Vec::new(),
        }
    }

    pub fn set_instrument(&mut self, instrument: String) {
        self.instrument = Some(instrument);
    }

    pub fn with_instrument(mut self, instrument: String) -> Self {
        self.set_instrument(instrument);
        self
    }

    pub fn set_channel(&mut self, channel: u8) {
        self.channel = Some(channel);
    }

    pub fn with_channel(mut self, channel: u8) -> Self {
        self.set_channel(channel);
        self
    }

    pub fn set_offset(&mut self, offset: Fraction) {
        self.offset = offset;
    }

    pub fn with_offset(mut self, offset: Fraction) -> Self {
        self.set_offset(offset);
        self
    }

    pub fn add_event(&mut self, event: MusicalEvent) {
        self.events.push(event);
    }

    pub fn with_event(mut self, event: MusicalEvent) -> Self {
        self.add_event(event);
        self
    }

    pub fn add_staff(&mut self, staff: Staff) {
        self.staves.push(staff);
    }

    pub fn with_staff(mut self, staff: Staff) -> Self {
        self.add_staff(staff);
        self
    }

    pub fn add_context_change(&mut self, change: ContextChange) {
        self.context_changes.push(change);
    }

    pub fn with_context_change(mut self, change: ContextChange) -> Self {
        self.add_context_change(change);
        self
    }

    /// Returns references to all events in this part, including those in staves.
    pub fn all_events(&self) -> Vec<&MusicalEvent> {
        let mut events = Vec::new();

        events.extend(self.events.iter());

        for staff in &self.staves {
            for voice in &staff.voices {
                events.extend(voice.events.iter());
            }
        }

        events
    }

    /// Counts all atomic events in this part (including nested sequences).
    pub fn total_event_count(&self) -> usize {
        let events_count: usize = self.events.iter().map(|e| e.atomic_event_count()).sum();
        let staves_count: usize = self
            .staves
            .iter()
            .map(|s| {
                s.voices
                    .iter()
                    .flat_map(|v| &v.events)
                    .map(|e| e.atomic_event_count())
                    .sum::<usize>()
            })
            .sum();
        events_count + staves_count
    }

    pub fn total_duration(&self) -> Fraction {
        let main_duration = self
            .events
            .iter()
            .map(|e| e.offset + e.total_duration())
            .max()
            .unwrap_or_else(|| Fraction::new(0, 1));

        let staff_duration = self
            .staves
            .iter()
            .map(|s| s.total_duration())
            .max()
            .unwrap_or_else(|| Fraction::new(0, 1));

        self.offset + main_duration.max(staff_duration)
    }

    /// Merges another part into this one, appending its content.
    ///
    /// The events, staves, and context changes from the `other` part are appended
    /// to this one, with their timing adjusted to occur after this part's content
    /// has finished.
    ///
    /// # Arguments
    ///
    /// * `other` - The part to merge into this one.
    ///
    /// # Returns
    ///
    /// A new `Part` containing the merged musical content.
    pub fn merge(&self, other: &Part) -> Part {
        let mut new_part = self.clone();
        let base_offset = self.total_duration();

        // Merge top-level events from the other part, adjusting their offsets.
        new_part.events.extend(other.events.iter().map(|e| {
            let mut new_event = e.clone();
            new_event.offset = e.offset + base_offset;
            new_event
        }));

        // Merge context changes, adjusting their time offsets.
        new_part
            .context_changes
            .extend(other.context_changes.iter().map(|c| ContextChange {
                time_offset: c.time_offset + base_offset,
                ..c.clone()
            }));

        // Merge staves from the other part.
        for other_staff in &other.staves {
            if let Some(self_staff) = new_part
                .staves
                .iter_mut()
                .find(|s| s.number == other_staff.number)
            {
                // If a staff with the same number exists, merge the voices.
                let staff_offset = self_staff.total_duration();
                for (i, other_voice) in other_staff.voices.iter().enumerate() {
                    let new_events: Vec<MusicalEvent> = other_voice
                        .events
                        .iter()
                        .map(|e| {
                            let mut new_event = e.clone();
                            new_event.offset = e.offset + staff_offset;
                            new_event
                        })
                        .collect();

                    if i < self_staff.voices.len() {
                        // Append events to the corresponding existing voice.
                        self_staff.voices[i].events.extend(new_events);
                    } else {
                        // If the other staff has more voices, add them as new voices.
                        let mut new_voice = Voice::new();
                        new_voice.events = new_events;
                        self_staff.voices.push(new_voice);
                    }
                }
            } else {
                // If the staff does not exist in the current part, add it as a new staff.
                let mut new_staff = other_staff.clone();
                for voice in &mut new_staff.voices {
                    for event in &mut voice.events {
                        event.offset += base_offset;
                    }
                }
                new_part.staves.push(new_staff);
            }
        }

        new_part
    }
}

/// A collection of musical parts that play simultaneously.
///
/// Represents the complete musical content at a given level,
/// whether for a full score or a movement within a larger work.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Timeline {
    pub parts: Vec<Part>,                    // All parts in this timeline
    pub context_changes: Vec<ContextChange>, // Global context changes
}

impl Timeline {
    pub fn add_part(&mut self, part: Part) {
        self.parts.push(part);
    }

    pub fn with_part(mut self, part: Part) -> Self {
        self.parts.push(part);
        self
    }

    pub fn add_parts(&mut self, parts: Vec<Part>) {
        self.parts.extend(parts);
    }

    pub fn with_parts(mut self, parts: Vec<Part>) -> Self {
        self.parts.extend(parts);
        self
    }

    pub fn add_context_change(&mut self, change: ContextChange) {
        self.context_changes.push(change);
    }

    pub fn with_context_change(mut self, change: ContextChange) -> Self {
        self.add_context_change(change);
        self
    }

    pub fn total_duration(&self) -> Fraction {
        self.parts
            .iter()
            .map(|p| p.total_duration())
            .max()
            .unwrap_or_else(|| Fraction::new(0, 1))
    }
}

/// A single movement within a larger musical work.
///
/// Classical works often consist of multiple movements with different
/// tempos, keys, and characters (e.g., a symphony's four movements).
#[derive(Debug, Clone, PartialEq)]
pub struct Movement {
    pub name: Option<String>,                // Movement name or number
    pub timeline: Timeline,                  // The musical content
    pub context_changes: Vec<ContextChange>, // Movement-level context changes
}

impl Movement {
    pub fn new(name: Option<String>) -> Self {
        Self {
            name,
            timeline: Timeline::default(),
            context_changes: Vec::new(),
        }
    }

    pub fn set_timeline(&mut self, timeline: Timeline) {
        self.timeline = timeline;
    }

    pub fn with_timeline(mut self, timeline: Timeline) -> Self {
        self.set_timeline(timeline);
        self
    }

    pub fn add_context_change(&mut self, change: ContextChange) {
        self.context_changes.push(change);
    }

    pub fn with_context_change(mut self, change: ContextChange) -> Self {
        self.add_context_change(change);
        self
    }

    pub fn total_duration(&self) -> Fraction {
        self.timeline.total_duration()
    }
}

/// The top-level container for a complete musical work.
///
/// A score can either be:
/// - Multi-movement (symphony, sonata) with separate movements
/// - Single-timeline (song, short piece) with direct timeline
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Score {
    pub name: Option<String>,                // Title of the work
    pub movements: Vec<Movement>,            // Multiple movements (if multi-movement)
    pub timeline: Option<Timeline>,          // Direct timeline (if single-movement)
    pub context_changes: Vec<ContextChange>, // Score-level context changes
}

impl Score {
    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }
    pub fn with_name(mut self, name: String) -> Self {
        self.set_name(name);
        self
    }

    pub fn add_movement(&mut self, movement: Movement) {
        self.movements.push(movement);
    }

    pub fn with_movement(mut self, movement: Movement) -> Self {
        self.add_movement(movement);
        self
    }

    pub fn set_timeline(&mut self, timeline: Timeline) {
        self.timeline = Some(timeline);
    }

    pub fn with_timeline(mut self, timeline: Timeline) -> Self {
        self.set_timeline(timeline);
        self
    }

    pub fn add_context_change(&mut self, change: ContextChange) {
        self.context_changes.push(change);
    }

    pub fn with_context_change(mut self, change: ContextChange) -> Self {
        self.add_context_change(change);
        self
    }

    pub fn total_duration(&self) -> Fraction {
        if let Some(timeline) = &self.timeline {
            timeline.total_duration()
        } else {
            self.movements.iter().map(|m| m.total_duration()).sum()
        }
    }

    /// Returns true if this score contains multiple movements.
    pub fn is_multi_movement(&self) -> bool {
        !self.movements.is_empty()
    }
}

/// Calculates the interval between two pitches in semitones.
///
/// # Arguments
/// * `ordered` - If true, returns signed interval (p2 - p1); if false, returns absolute value
pub fn pitch_interval(p1: &Pitch, p2: &Pitch, ordered: bool) -> i32 {
    let interval = p1.interval_to(p2);
    if ordered {
        interval
    } else {
        interval.abs()
    }
}

/// Calculates the interval between two pitch classes.
///
/// # Arguments
/// * `ordered` - If true, returns directed interval (0-11); if false, returns interval class (0-6)
pub fn pitch_class_interval(pc1: &PitchClass, pc2: &PitchClass, ordered: bool) -> u8 {
    if ordered {
        pc1.interval_to(pc2)
    } else {
        pc1.interval_class_to(pc2)
    }
}

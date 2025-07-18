//! Core traits for musical transformations and relationships.
//!
//! These traits provide fundamental operations used throughout music theory:
//! - Transposition (moving pitches up or down by semitones)
//! - Inversion (reflecting pitches around an axis)
//! - Interval calculation (measuring distances between musical objects)

use super::PitchClass;

/// Trait for musical objects that can be transposed (shifted by semitones).
///
/// Transposition is a fundamental operation in music theory that moves
/// pitches, chords, or entire musical passages up or down by a fixed
/// number of semitones while preserving their relative relationships.
///
/// # Examples
/// - Transposing a C major chord up 2 semitones yields a D major chord
/// - Transposing a melody up an octave (12 semitones) raises its pitch
///   while keeping the same note sequence
pub trait Transpose {
    /// Transposes the musical object by the specified number of semitones.
    ///
    /// # Arguments
    /// * `semitones` - Number of semitones to transpose (positive = up, negative = down)
    ///
    /// # Returns
    /// A new instance of the same type, transposed by the given interval
    fn transpose(&self, semitones: i32) -> Self;
}

/// Trait for musical objects that can be inverted around an axis.
///
/// Musical inversion reflects pitches around a central axis, creating
/// a mirror image of the original musical material. This is a key operation
/// in twelve-tone and atonal music analysis, where inversions are used
/// to generate related musical material.
///
/// The trait is generic over the axis type to support different kinds of
/// musical inversion:
/// - Inversion around a `PitchClass`: Chromatic inversion ignoring octave information
/// - Inversion around a `Pitch`: Inversion preserving octave relationships
///
/// # Examples
/// - Inverting a C major triad (C-E-G) around C pitch class yields C-Ab-F
/// - Inverting around a specific pitch preserves octave structure
/// - Inversion is often combined with transposition in serial music
pub trait Invert<Axis = PitchClass> {
    /// Inverts the musical object around the specified axis.
    ///
    /// # Arguments
    /// * `axis` - The axis to use as the center of inversion
    ///
    /// # Returns
    /// A new instance of the same type, inverted around the given axis
    fn invert(&self, axis: &Axis) -> Self;
}

/// Trait for calculating intervals between musical objects.
///
/// An interval represents the distance between two musical objects,
/// such as the number of semitones between two pitches or the
/// relationship between two pitch class sets.
///
/// The output type varies depending on what kind of musical objects
/// are being compared (e.g., semitones for pitches, interval classes
/// for pitch classes).
pub trait Interval {
    /// The type returned when calculating intervals between objects of this type.
    type Output;

    /// Calculates the interval from this object to another object of the same type.
    ///
    /// # Arguments
    /// * `other` - The target object to calculate the interval to
    ///
    /// # Returns
    /// The interval between the two objects, in a format appropriate for the type
    fn interval_to(&self, other: &Self) -> Self::Output;
}

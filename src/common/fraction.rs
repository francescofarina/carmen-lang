//! Musical fraction utilities for handling note durations, tuplets, and rhythmic calculations.
//!
//! This module provides a robust `Fraction` type for representing musical durations
//! as rational numbers, along with utilities for working with tuplets and dotted notes.

use super::utils::gcd;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::iter::Sum;
use std::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};
use std::str::FromStr;

/// A struct representing a musical tuplet's properties.
///
/// Tuplets are rhythmic groupings where a certain number of notes are played
/// in the time normally occupied by a different number of notes. For example,
/// a triplet plays 3 notes in the time of 2 normal notes.
///
/// # Examples
///
/// ```
/// use carmen_lang::common::fraction::{TupletInfo, Fraction};
///
/// // A triplet: 3 eighth notes in the time of 2 eighth notes
/// let triplet = TupletInfo {
///     tuplet_number: 3,
///     normal_number: 2,
///     base_duration: 8,
/// };
///
/// // Each note in the triplet has duration 2/(8*3) = 1/12
/// let duration = triplet.tuplet_duration();
/// assert_eq!(duration, Fraction::new(1, 12));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TupletInfo {
    /// The number of notes in the tuplet (e.g., 3 for a triplet).
    pub tuplet_number: u32,
    /// The number of normal notes that the tuplet fits into (e.g., 2 for a triplet).
    pub normal_number: u32,
    /// The denominator of the base note duration (e.g., 8 for an eighth-note-based tuplet).
    pub base_duration: u32,
}

impl TupletInfo {
    /// Calculates the duration of each individual note within the tuplet.
    ///
    /// The duration is calculated as: `normal_number / (base_duration * tuplet_number)`
    ///
    /// For a triplet (3 notes in time of 2), each note gets 2/3 of the base duration.
    /// If the base duration is an eighth note (1/8), each triplet note is 2/(8*3) = 1/12.
    pub fn tuplet_duration(&self) -> Fraction {
        Fraction::new(self.normal_number, self.base_duration * self.tuplet_number)
    }
}

/// Represents a rational number, always stored in its simplest form.
///
/// This type is specifically designed for musical duration calculations where
/// precision is critical. All fractions are automatically reduced to their
/// simplest form upon creation.
///
/// # Examples
///
/// ```
/// use carmen_lang::common::fraction::Fraction;
///
/// // Create a half note (1/2 duration)
/// let half_note = Fraction::new(1, 2);
///
/// // Create a quarter note (1/4 duration) - equivalent to 2/8
/// let quarter_note = Fraction::new(2, 8); // Automatically reduced to 1/4
/// assert_eq!(quarter_note, Fraction::new(1, 4));
///
/// // Add two quarter notes to get a half note
/// assert_eq!(quarter_note + quarter_note, half_note);
/// ```
#[derive(Debug, Clone, Copy, Eq)]
pub struct Fraction {
    pub numerator: u32,
    pub denominator: u32,
}

impl Fraction {
    /// Creates a new fraction in its simplest form.
    ///
    /// The fraction is automatically reduced using the greatest common divisor.
    ///
    /// # Panics
    ///
    /// Panics if the denominator is zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::Fraction;
    ///
    /// let f = Fraction::new(6, 8); // Automatically reduced to 3/4
    /// assert_eq!(f.numerator, 3);
    /// assert_eq!(f.denominator, 4);
    /// ```
    pub fn new(numerator: u32, denominator: u32) -> Self {
        if denominator == 0 {
            panic!("Denominator cannot be zero");
        }
        let common = gcd(numerator, denominator);
        Self {
            numerator: numerator / common,
            denominator: denominator / common,
        }
    }

    /// Converts the fraction to a floating-point number.
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::Fraction;
    ///
    /// let quarter_note = Fraction::new(1, 4);
    /// assert_eq!(quarter_note.to_f64(), 0.25);
    /// ```
    pub fn to_f64(&self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }

    /// Checks if this fraction represents a standard binary musical duration.
    ///
    /// Standard binary durations are those that can be written with standard note values
    /// and dots in traditional musical notation. These have:
    /// - A denominator that is a power of 2 (1, 2, 4, 8, 16, etc.)
    /// - A numerator of the form 2^k - 1 (which allows for dotted notes)
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::Fraction;
    ///
    /// // Standard durations
    /// assert!(Fraction::new(1, 4).is_standard_binary()); // Quarter note
    /// assert!(Fraction::new(3, 8).is_standard_binary()); // Dotted quarter note
    /// assert!(Fraction::new(1, 8).is_standard_binary()); // Eighth note
    ///
    /// // Non-standard durations (require tuplets)
    /// assert!(!Fraction::new(1, 6).is_standard_binary()); // Triplet eighth
    /// assert!(!Fraction::new(2, 3).is_standard_binary()); // Complex duration
    /// ```
    pub fn is_standard_binary(&self) -> bool {
        // Denominator must be a power of two.
        if self.denominator == 0 || (self.denominator & (self.denominator - 1)) != 0 {
            return false;
        }
        // Numerator must be of the form 2^k - 1, which means numerator + 1 is a power of two.
        if self.numerator == 0 {
            return false;
        }
        let n_plus_1 = self.numerator + 1;
        (n_plus_1 & (n_plus_1 - 1)) == 0
    }

    /// Returns a new fraction with the specified number of dots applied.
    ///
    /// In musical notation, dots extend a note's duration. Each dot adds half
    /// of the note's current duration. The mathematical formula for n dots is:
    /// `duration * (2^(n+1) - 1) / 2^n`
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::Fraction;
    ///
    /// let quarter = Fraction::new(1, 4);
    ///
    /// // Single dot: quarter + eighth = 3/8
    /// let dotted_quarter = quarter.with_dots(1);
    /// assert_eq!(dotted_quarter, Fraction::new(3, 8));
    ///
    /// // Double dot: quarter + eighth + sixteenth = 7/16
    /// let double_dotted_quarter = quarter.with_dots(2);
    /// assert_eq!(double_dotted_quarter, Fraction::new(7, 16));
    /// ```
    pub fn with_dots(&self, dots: u32) -> Self {
        if dots == 0 {
            return *self;
        }
        // For n dots, the duration is multiplied by (2^(n+1) - 1) / 2^n.
        let dot_factor_num = (1u32 << (dots + 1)) - 1;
        let dot_factor_denom = 1u32 << dots;

        Self::new(
            self.numerator * dot_factor_num,
            self.denominator * dot_factor_denom,
        )
    }

    /// Converts this fraction to tuplet information if it represents a non-standard duration.
    ///
    /// Returns `None` if the fraction represents a standard binary duration that can be
    /// notated without tuplets. Otherwise, returns the tuplet information needed to
    /// represent this duration.
    ///
    /// The algorithm finds the closest longer standard duration as the tuplet base,
    /// then calculates how many tuplet notes fit in the time of how many normal notes.
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::{Fraction, TupletInfo};
    ///
    /// // Standard duration - no tuplet needed
    /// let quarter = Fraction::new(1, 4);
    /// assert!(quarter.to_tuplet_info().is_none());
    ///
    /// // Triplet eighth note: 1/12 = 2/(8*3), so 3 notes in time of 2
    /// let triplet_eighth = Fraction::new(1, 12);
    /// let tuplet_info = triplet_eighth.to_tuplet_info().unwrap();
    /// assert_eq!(tuplet_info.tuplet_number, 3);
    /// assert_eq!(tuplet_info.normal_number, 2);
    /// assert_eq!(tuplet_info.base_duration, 8);
    /// ```
    pub fn to_tuplet_info(&self) -> Option<TupletInfo> {
        if self.is_standard_binary() {
            return None;
        }

        // Find the closest *longer* standard duration to serve as the tuplet's base.
        let mut base_duration_denom: u32 = 1;
        while (1.0 / base_duration_denom as f64) > self.to_f64() {
            base_duration_denom *= 2;
        }
        base_duration_denom = base_duration_denom.saturating_div(2).max(1);

        // Calculate the scaling factor: (self) / (base_duration)
        // (num/den) / (1/base_denom) = (num * base_denom) / den
        let factor_num = self.numerator * base_duration_denom;
        let factor_den = self.denominator;

        // Simplify the factor to get the tuplet numbers. "3 notes in the time of 2" -> 2/3.
        let common = gcd(factor_num, factor_den);
        let normal_number = factor_num / common;
        let tuplet_number = factor_den / common;

        Some(TupletInfo {
            tuplet_number,
            normal_number,
            base_duration: base_duration_denom,
        })
    }

    /// Returns the reciprocal of this fraction (denominator/numerator).
    ///
    /// # Panics
    ///
    /// Panics if the numerator is zero (division by zero).
    ///
    /// # Examples
    ///
    /// ```
    /// use carmen_lang::common::fraction::Fraction;
    ///
    /// let quarter = Fraction::new(1, 4);
    /// let reciprocal = quarter.reciprocal();
    /// assert_eq!(reciprocal, Fraction::new(4, 1));
    /// ```
    pub fn reciprocal(&self) -> Self {
        if self.numerator == 0 {
            panic!("Cannot calculate the reciprocal of zero");
        }
        Self {
            numerator: self.denominator,
            denominator: self.numerator,
        }
    }
}

impl PartialEq for Fraction {
    fn eq(&self, other: &Self) -> bool {
        self.numerator == other.numerator && self.denominator == other.denominator
    }
}

impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fraction {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.numerator as u64 * other.denominator as u64)
            .cmp(&(other.numerator as u64 * self.denominator as u64))
    }
}

impl Display for Fraction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl FromStr for Fraction {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('/').collect();
        match *parts.as_slice() {
            [num_str] => {
                let numerator = num_str.parse::<u32>().map_err(|_| "Invalid numerator")?;
                Ok(Fraction::new(numerator, 1))
            }
            [num_str, den_str] => {
                let numerator = num_str.parse::<u32>().map_err(|_| "Invalid numerator")?;
                let denominator = den_str.parse::<u32>().map_err(|_| "Invalid denominator")?;
                if denominator == 0 {
                    Err("Denominator cannot be zero")
                } else {
                    Ok(Fraction::new(numerator, denominator))
                }
            }
            _ => Err("Invalid fraction format"),
        }
    }
}

impl Add for Fraction {
    type Output = Self;

    /// Adds two fractions using cross-multiplication.
    ///
    /// Uses u64 arithmetic internally to prevent overflow during intermediate calculations.
    fn add(self, rhs: Self) -> Self::Output {
        let numerator = (self.numerator as u64 * rhs.denominator as u64)
            + (rhs.numerator as u64 * self.denominator as u64);
        let denominator = self.denominator as u64 * rhs.denominator as u64;

        Self::new(numerator as u32, denominator as u32)
    }
}

impl AddAssign for Fraction {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl AddAssign<u32> for Fraction {
    fn add_assign(&mut self, rhs: u32) {
        *self = *self + Self::new(rhs, 1);
    }
}

impl Add<u32> for Fraction {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        self + Self::new(rhs, 1)
    }
}

impl Sub for Fraction {
    type Output = Self;

    /// Subtracts two fractions using cross-multiplication.
    ///
    /// Returns 0/1 if the result would be negative (underflow protection).
    fn sub(self, rhs: Self) -> Self::Output {
        let new_num = (self.numerator as u64 * rhs.denominator as u64)
            .checked_sub(rhs.numerator as u64 * self.denominator as u64);

        if new_num.is_none() {
            return Self::new(0, 1);
        }

        let new_den = self.denominator as u64 * rhs.denominator as u64;
        Self::new(new_num.unwrap() as u32, new_den as u32)
    }
}

impl SubAssign for Fraction {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Sum<Fraction> for Fraction {
    fn sum<I: Iterator<Item = Fraction>>(iter: I) -> Self {
        iter.fold(Fraction::new(0, 1), |a, b| a + b)
    }
}

impl Mul for Fraction {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(
            self.numerator * rhs.numerator,
            self.denominator * rhs.denominator,
        )
    }
}

impl Div for Fraction {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(
            self.numerator * rhs.denominator,
            self.denominator * rhs.numerator,
        )
    }
}

impl Mul<u32> for Fraction {
    type Output = Self;

    fn mul(self, rhs: u32) -> Self::Output {
        Self::new(self.numerator * rhs, self.denominator)
    }
}

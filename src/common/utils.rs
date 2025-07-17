//! Mathematical utility functions for common calculations.
//!
//! This module provides basic mathematical operations that are commonly
//! needed throughout the application, particularly for fraction arithmetic.

/// Calculates the Greatest Common Divisor (GCD) of two numbers using Euclid's algorithm.
///
/// The GCD is the largest positive integer that divides both numbers without remainder.
/// This function uses the recursive Euclidean algorithm for efficiency.
///
/// # Examples
///
/// ```
/// use carmen_lang::common::utils::gcd;
///
/// assert_eq!(gcd(12, 8), 4);
/// assert_eq!(gcd(17, 13), 1); // coprime numbers
/// assert_eq!(gcd(0, 5), 5);
/// ```
pub fn gcd(a: u32, b: u32) -> u32 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

/// Calculates the Least Common Multiple (LCM) of two numbers.
///
/// The LCM is the smallest positive integer that is divisible by both numbers.
/// This function uses the relationship: LCM(a,b) = (a * b) / GCD(a,b).
///
/// # Examples
///
/// ```
/// use carmen_lang::common::utils::lcm;
///
/// assert_eq!(lcm(4, 6), 12);
/// assert_eq!(lcm(3, 5), 15); // coprime numbers
/// assert_eq!(lcm(8, 12), 24);
/// ```
pub fn lcm(a: u32, b: u32) -> u32 {
    a * b / gcd(a, b)
}

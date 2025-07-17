//! Argument extraction utilities for builtin functions.
//!
//! This module provides a flexible system for extracting and validating arguments
//! passed to builtin functions. It handles type conversion, arity checking, and
//! special cases like tuple unpacking and chord decomposition.

use crate::{
    errors::{AddSpan, CarmenError, ErrorSource, Result},
    Value,
};

/// Trait for extracting typed values from `Value` instances.
///
/// This trait enables generic argument extraction across different types,
/// with automatic type conversion and validation.
pub trait ValueExtractor<T> {
    /// Attempts to extract a value of type `T` from this `Value`.
    ///
    /// Returns `Some(T)` if the extraction succeeds, `None` if the value
    /// cannot be converted to the target type.
    fn extract(&self) -> Option<T>;

    /// Returns the static type name for error reporting.
    fn type_name_static() -> &'static str;
}

impl<T> ValueExtractor<T> for Value
where
    T: Clone + 'static,
    Option<T>: From<Value>,
{
    fn extract(&self) -> Option<T> {
        self.clone().into()
    }

    fn type_name_static() -> &'static str {
        std::any::type_name::<T>()
    }
}

/// Generic argument extractor that can extract exactly `N` arguments of type `T`.
///
/// This struct uses const generics to specify the number of arguments to extract
/// at compile time, enabling type-safe argument validation.
pub struct ArgExtractor<const N: usize>;

impl<const N: usize> ArgExtractor<N> {
    /// Extracts exactly `N` arguments of type `T` from the provided argument list.
    ///
    /// This function handles several argument patterns:
    /// 1. Multiple arguments: `func(arg1, arg2, arg3)`
    /// 2. Tuple unpacking: `func((arg1, arg2, arg3))`
    /// 3. Chord decomposition: `func(chord)` where chord contains multiple pitches
    ///
    /// # Arguments
    /// * `args` - The argument list to extract from
    /// * `span` - Source location for error reporting
    ///
    /// # Returns
    /// A vector containing exactly `N` extracted values of type `T`
    pub fn extract<T>(args: &[Value], span: &crate::errors::Span) -> Result<Vec<T>>
    where
        Value: ValueExtractor<T>,
        T: Clone,
    {
        match args.len() {
            1 => {
                // Try to extract from tuple first
                if let Value::Tuple(vals) = &args[0] {
                    if vals.len() >= N {
                        let extracted: Option<Vec<T>> =
                            vals[..N].iter().map(|v| v.extract()).collect();

                        if let Some(values) = extracted {
                            return Ok(values);
                        }
                    }
                }

                // Try to extract from chord by converting pitches to Values
                if let Value::Chord(chord) = &args[0] {
                    if chord.pitches.len() >= N {
                        let pitch_values: Vec<Value> =
                            chord.pitches.iter().map(|p| Value::Pitch(*p)).collect();
                        let extracted: Option<Vec<T>> =
                            pitch_values[..N].iter().map(|v| v.extract()).collect();

                        if let Some(values) = extracted {
                            return Ok(values);
                        }
                    }
                }
                Err(CarmenError::new(
                    ErrorSource::Runtime(format!(
                        "Function requires {} {}s",
                        N,
                        <Value as ValueExtractor<T>>::type_name_static()
                    )),
                    *span,
                ))
            }
            len if len >= N => {
                let extracted: Option<Vec<T>> = args[..N].iter().map(|v| v.extract()).collect();

                if let Some(values) = extracted {
                    Ok(values)
                } else {
                    Err(ErrorSource::Type(format!(
                        "Invalid argument types, expected {}s",
                        <Value as ValueExtractor<T>>::type_name_static()
                    ))
                    .with_span(*span))
                }
            }
            _ => Err(CarmenError::new(
                ErrorSource::Argument(format!("Function requires at least {N} arguments")),
                *span,
            )),
        }
    }
}

/// Extracts arguments that may be provided as individual values, tuples, or chords.
///
/// This function provides flexible argument handling for functions that can accept
/// either individual arguments or packed collections.
///
/// # Behavior
/// * Single tuple argument → Unpacks tuple elements
/// * Single chord argument → Converts pitches to individual `Value::Pitch` instances
/// * Single other argument → Returns as single-element vector
/// * Multiple arguments → Returns all arguments as-is
///
/// # Arguments
/// * `args` - The argument list to process
/// * `_span` - Source location (currently unused)
///
/// # Returns
/// A vector of individual `Value` instances
pub fn extract_mixed_args(args: &[Value], _span: &crate::errors::Span) -> Result<Vec<Value>> {
    match args.len() {
        1 => {
            if let Value::Tuple(vals) = &args[0] {
                Ok(vals.clone())
            } else if let Value::Chord(chord) = &args[0] {
                Ok(chord.pitches.iter().map(|p| Value::Pitch(*p)).collect())
            } else {
                Ok(vec![args[0].clone()])
            }
        }
        _ => Ok(args.to_vec()),
    }
}

/// Extracts a named argument (keyword argument) with a default fallback value.
///
/// This helper function searches through keyword arguments for a specific parameter
/// and attempts to extract it as the target type. If the argument is not found or
/// cannot be converted, the default value is returned.
///
/// # Arguments
/// * `named_args` - List of (name, value) pairs representing keyword arguments
/// * `name` - The parameter name to search for
/// * `default` - Default value to return if extraction fails
///
/// # Returns
/// The extracted value of type `T`, or the default if extraction fails
pub fn extract_kwarg<T>(named_args: &[(String, Value)], name: &str, default: T) -> T
where
    Value: ValueExtractor<T>,
    T: Clone,
{
    named_args
        .iter()
        .find(|(arg_name, _)| arg_name == name)
        .and_then(|(_, value)| value.extract())
        .unwrap_or(default)
}

// Convenience macros for common argument extraction patterns.
// These macros provide a more ergonomic interface for the most common
// argument extraction scenarios in builtin functions.

/// Extracts exactly `$count` arguments of type `$type` from an argument list.
///
/// # Usage
/// ```ignore
/// let values = extract_args!(args, span, 2, f64)?;
/// ```
#[macro_export]
macro_rules! extract_args {
    ($args:expr, $span:expr, $count:literal, $type:ty) => {
        $crate::interpreter::arg_extraction::ArgExtractor::<$count>::extract::<$type>($args, $span)
    };
}

/// Extracts exactly two arguments and returns them as a tuple.
/// This is a common pattern for binary operations.
///
/// # Usage
/// ```ignore
/// let (first, second) = extract_two!(args, span, Pitch)?;
/// ```
#[macro_export]
macro_rules! extract_two {
    ($args:expr, $span:expr, $type:ty) => {{
        let values = extract_args!($args, $span, 2, $type)?;
        (values[0].clone(), values[1].clone())
    }};
}

/// Extracts exactly three arguments and returns them as a tuple.
/// This is useful for ternary operations.
///
/// # Usage
/// ```ignore
/// let (first, second, third) = extract_three!(args, span, f64)?;
/// ```
#[macro_export]
macro_rules! extract_three {
    ($args:expr, $span:expr, $type:ty) => {{
        let values = extract_args!($args, $span, 3, $type)?;
        (values[0].clone(), values[1].clone(), values[2].clone())
    }};
}

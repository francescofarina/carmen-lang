//! Builtin functions for the Carmen interpreter.
//!
//! This module provides the core library of musical analysis and transformation
//! functions available to Carmen programs. Functions include pitch operations,
//! set theory analysis, transposition, inversion, and other music-theoretical
//! computations.

use super::Value;
use crate::core::traits::{Invert as CoreInvert, Transpose as CoreTranspose};
use crate::core::*;
use crate::errors::{AddSpan, ErrorSource, Result, Span};
use crate::interpreter::arg_extraction::*;
use crate::{extract_args, extract_two};

/// Trait defining the interface for builtin functions.
///
/// All builtin functions must implement this trait to be callable from Carmen code.
/// The trait provides a uniform interface for function dispatch and error handling.
pub trait BuiltinFunction {
    /// Returns the function's name.
    fn name(&self) -> &'static str;

    /// Executes the function with the given arguments.
    ///
    /// # Arguments
    /// * `args` - Positional arguments passed to the function
    /// * `kwargs` - Named arguments (keyword arguments) passed to the function
    /// * `span` - Source location for error reporting
    ///
    /// # Returns
    /// The result of the function execution as a `Value`
    ///
    /// # Errors
    /// Returns an error if argument validation fails or the operation cannot be completed
    fn call(&self, args: &[Value], kwargs: &[(String, Value)], span: &Span) -> Result<Value>;
}

/// Calculates the interval in semitones between two pitches.
///
/// # Arguments
/// * `pitch1` - First pitch
/// * `pitch2` - Second pitch
/// * `ordered` (optional) - If `true`, returns signed interval; if `false`, returns absolute interval
pub struct PitchInterval;
impl BuiltinFunction for PitchInterval {
    fn name(&self) -> &'static str {
        "pitch_interval"
    }

    fn call(&self, args: &[Value], kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        let (p1, p2) = extract_two!(args, span, Pitch);
        let ordered = extract_kwarg(kwargs, "ordered", false);
        let interval = crate::core::pitch_interval(&p1, &p2, ordered);
        Ok(Value::Number(interval as f64))
    }
}

/// Calculates the interval in semitones between two pitch classes.
///
/// # Arguments
/// * `pc1` - First pitch class (0-11)
/// * `pc2` - Second pitch class (0-11)
/// * `ordered` (optional) - If `true`, returns signed interval; if `false`, returns absolute interval
pub struct PitchClassInterval;
impl BuiltinFunction for PitchClassInterval {
    fn name(&self) -> &'static str {
        "pitch_class_interval"
    }

    fn call(&self, args: &[Value], kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        let (pc1, pc2) = extract_two!(args, span, PitchClass);
        let ordered = extract_kwarg(kwargs, "ordered", false);
        let interval = crate::core::pitch_class_interval(&pc1, &pc2, ordered);
        Ok(Value::Number(interval as f64))
    }
}

/// Transposes musical objects by a specified number of semitones.
///
/// This function can transpose pitches, pitch classes, pitch class sets, chords,
/// musical events, and lists of musical objects.
///
/// # Arguments
/// * `object` - The musical object to transpose
/// * `semitones` - Number of semitones to transpose (positive = up, negative = down)
pub struct Transpose;
impl BuiltinFunction for Transpose {
    fn name(&self) -> &'static str {
        "transpose"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        let values = extract_mixed_args(args, span)?;

        if values.len() < 2 {
            return Err(
                ErrorSource::Argument("transpose requires 2 arguments".to_string())
                    .with_span(*span),
            );
        }

        let semitones = match <Value as ValueExtractor<f64>>::extract(&values[1]) {
            Some(n) => n as i32,
            None => {
                return Err(
                    ErrorSource::Argument("Second argument must be a number".to_string())
                        .with_span(*span),
                )
            }
        };

        match &values[0] {
            Value::Pitch(p) => Ok(Value::Pitch(p.transpose(semitones))),
            Value::PitchClass(pc) => Ok(Value::PitchClass(pc.transpose(semitones))),
            Value::PitchClassSet(pcs) => Ok(Value::PitchClassSet(pcs.transpose(semitones))),
            Value::Chord(chord) => Ok(Value::Chord(chord.transpose(semitones))),
            Value::MusicalEvent(event) => Ok(Value::MusicalEvent(event.transpose(semitones))),
            Value::Rest => Ok(Value::Rest),
            Value::List(events) => {
                let transposed: Result<Vec<Value>> = events
                    .iter()
                    .map(|event| self.call(&[event.clone(), values[1].clone()], &[], span))
                    .collect();
                Ok(Value::List(transposed?))
            }
            _ => Err(ErrorSource::Type("Cannot transpose this type".to_string()).with_span(*span)),
        }
    }
}

/// Inverts musical objects around a specified axis pitch class.
///
/// Inversion reflects pitches or pitch classes around an axis, preserving
/// interval relationships but reversing their direction.
///
/// # Arguments
/// * `object` - The musical object to invert
/// * `axis` - The pitch class to use as the inversion axis
pub struct Invert;
impl BuiltinFunction for Invert {
    fn name(&self) -> &'static str {
        "invert"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        let values = extract_mixed_args(args, span)?;

        if values.len() < 2 {
            return Err(
                ErrorSource::Argument("invert requires 2 arguments".to_string()).with_span(*span),
            );
        }

        // Try to extract axis as PitchClass first, then as Pitch
        if let Some(axis_pc) = <Value as ValueExtractor<PitchClass>>::extract(&values[1]) {
            // Chromatic inversion around pitch class axis
            match &values[0] {
                Value::Number(n) => {
                    if *n >= 0.0 && *n <= 11.0 && n.fract() == 0.0 {
                        let pc = PitchClass::new(*n as u8);
                        let inverted = pc.invert(&axis_pc);
                        Ok(Value::Number(inverted.0 as f64))
                    } else {
                        Err(ErrorSource::Type(
                            "Number must be integer 0-11 for pitch class inversion".to_string(),
                        )
                        .with_span(*span))
                    }
                }
                Value::PitchClass(pc) => Ok(Value::PitchClass(pc.invert(&axis_pc))),
                Value::PitchClassSet(pcs) => Ok(Value::PitchClassSet(pcs.invert(&axis_pc))),
                Value::Pitch(p) => {
                    let inverted_pc = p.pitch_class.invert(&axis_pc);
                    let inverted_pitch = Pitch {
                        pitch_class: inverted_pc,
                        octave: p.octave,
                    };
                    Ok(Value::Pitch(inverted_pitch))
                }
                _ => Err(ErrorSource::Type("Cannot invert this type".to_string()).with_span(*span)),
            }
        } else if let Some(axis_pitch) = <Value as ValueExtractor<Pitch>>::extract(&values[1]) {
            // Pitch inversion around specific pitch axis
            match &values[0] {
                Value::Pitch(p) => Ok(Value::Pitch(p.invert(&axis_pitch))),
                _ => Err(ErrorSource::Type(
                    "When using a pitch as axis, can only invert pitches".to_string(),
                )
                .with_span(*span)),
            }
        } else {
            Err(
                ErrorSource::Argument("Second argument must be a pitch class or pitch".to_string())
                    .with_span(*span),
            )
        }
    }
}

/// Computes the normal form of a pitch class set.
///
/// Normal form is the most compact representation of a pitch class set,
/// starting from the pitch class that produces the smallest intervals.
///
/// # Arguments
/// * `set` - A pitch class set or collection of pitch classes
pub struct NormalForm;
impl BuiltinFunction for NormalForm {
    fn name(&self) -> &'static str {
        "normal_form"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(
                ErrorSource::Argument("normal_form requires 1 argument".to_string())
                    .with_span(*span),
            );
        }

        match &args[0] {
            Value::PitchClassSet(pcs) => {
                let normal = pcs.normal_form();
                Ok(Value::PitchClassSet(normal))
            }
            Value::Set(values) => {
                // Convert set of numbers to pitch class set
                let mut classes = Vec::new();
                for val in values {
                    if let Value::PitchClass(n) = val {
                        classes.push(*n);
                    }
                }
                let pcs = PitchClassSet::new(classes);
                let normal = pcs.normal_form();
                Ok(Value::PitchClassSet(normal))
            }
            _ => Err(
                ErrorSource::Type("normal_form requires a pitch class set".to_string())
                    .with_span(*span),
            ),
        }
    }
}

/// Computes the prime form of a pitch class set.
///
/// Prime form is the most compact representation of a set class,
/// considering both the set and its inversion, transposed to start at 0.
///
/// # Arguments
/// * `set` - A pitch class set or collection of pitch classes/numbers
pub struct PrimeForm;
impl BuiltinFunction for PrimeForm {
    fn name(&self) -> &'static str {
        "prime_form"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(
                ErrorSource::Argument("prime_form requires 1 argument".to_string())
                    .with_span(*span),
            );
        }

        match &args[0] {
            Value::PitchClassSet(pcs) => {
                let prime = pcs.prime_form();
                Ok(Value::PitchClassSet(prime))
            }
            Value::Set(values) => {
                let mut classes = Vec::new();
                for val in values {
                    match val {
                        Value::PitchClass(pc) => classes.push(*pc),
                        Value::Number(n) if *n >= 0.0 && *n <= 11.0 && n.fract() == 0.0 => {
                            classes.push(PitchClass::new(*n as u8));
                        }
                        _ => {}
                    }
                }
                let pcs = PitchClassSet::new(classes);
                let prime = pcs.prime_form();
                Ok(Value::PitchClassSet(prime))
            }
            _ => Err(
                ErrorSource::Type("prime_form requires a pitch class set".to_string())
                    .with_span(*span),
            ),
        }
    }
}

/// Computes the interval class vector of a pitch class set.
///
/// The interval class vector shows the frequency of each interval class (1-6)
/// within the set, providing a compact representation of its intervallic content.
///
/// # Arguments
/// * `set` - A pitch class set
///
/// # Returns
/// A list of 6 numbers representing the count of each interval class
pub struct IntervalClassVector;
impl BuiltinFunction for IntervalClassVector {
    fn name(&self) -> &'static str {
        "interval_class_vector"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(ErrorSource::Argument(
                "interval_class_vector requires 1 argument".to_string(),
            )
            .with_span(*span));
        }

        match &args[0] {
            Value::PitchClassSet(pcs) => {
                let icv = pcs.interval_class_vector();
                let values: Vec<Value> = icv.into_iter().map(|n| Value::Number(n as f64)).collect();
                Ok(Value::List(values))
            }
            _ => Err(ErrorSource::Type(
                "interval_class_vector requires a pitch class set".to_string(),
            )
            .with_span(*span)),
        }
    }
}

/// Computes the set class of a pitch class set.
///
/// A set class contains all transpositions and inversions of a given pitch class set.
/// This function returns all members of the set class.
///
/// # Arguments
/// * `set` - A pitch class set or collection of pitch classes/numbers
///
/// # Returns
/// A list of pitch class sets representing all members of the set class
pub struct SetClass;
impl BuiltinFunction for SetClass {
    fn name(&self) -> &'static str {
        "set_class"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(
                ErrorSource::Argument("set_class requires 1 argument".to_string()).with_span(*span),
            );
        }

        match &args[0] {
            Value::PitchClassSet(pcs) => {
                let set_class = pcs.set_class();
                let values: Vec<Value> = set_class.into_iter().map(Value::PitchClassSet).collect();
                Ok(Value::List(values))
            }
            Value::Set(values) => {
                let mut classes = Vec::new();
                for val in values {
                    match val {
                        Value::PitchClass(pc) => classes.push(*pc),
                        Value::Number(n) if *n >= 0.0 && *n <= 11.0 && n.fract() == 0.0 => {
                            classes.push(PitchClass::new(*n as u8));
                        }
                        _ => {}
                    }
                }
                let pcs = PitchClassSet::new(classes);
                let set_class = pcs.set_class();
                let values: Vec<Value> = set_class.into_iter().map(Value::PitchClassSet).collect();
                Ok(Value::List(values))
            }
            _ => Err(
                ErrorSource::Type("set_class requires a pitch class set".to_string())
                    .with_span(*span),
            ),
        }
    }
}

/// Calculates the interval class between two pitch classes.
///
/// Interval class reduces all intervals to their smallest form (1-6 semitones),
/// treating intervals and their inversions as equivalent.
///
/// # Arguments
/// * `pc1` - First pitch class
/// * `pc2` - Second pitch class
///
/// # Returns
/// An interval class number from 1 to 6
pub struct IntervalClass;
impl BuiltinFunction for IntervalClass {
    fn name(&self) -> &'static str {
        "interval_class"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        let (pc1, pc2) = extract_two!(args, span, PitchClass);
        let ic = pc1.interval_class_to(&pc2);
        Ok(Value::Number(ic as f64))
    }
}

/// Computes the interval class content of a pitch class set.
///
/// This is similar to interval class vector but manually calculates the count
/// of each interval class by examining all pairs within the set.
///
/// # Arguments
/// * `set` - A pitch class set
///
/// # Returns
/// A list of 6 numbers representing the count of each interval class (1-6)
pub struct IntervalClassContent;
impl BuiltinFunction for IntervalClassContent {
    fn name(&self) -> &'static str {
        "interval_class_content"
    }

    fn call(&self, args: &[Value], _kwargs: &[(String, Value)], span: &Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(ErrorSource::Argument(
                "interval_class_content requires 1 argument".to_string(),
            )
            .with_span(*span));
        }

        match &args[0] {
            Value::PitchClassSet(pcs) => {
                let mut content = std::collections::HashMap::new();
                let classes: Vec<PitchClass> = pcs.classes.iter().cloned().collect();

                for i in 0..classes.len() {
                    for j in (i + 1)..classes.len() {
                        let ic = classes[i].interval_class_to(&classes[j]);
                        *content.entry(ic).or_insert(0) += 1;
                    }
                }

                let result: Vec<Value> = (1..=6)
                    .map(|i| Value::Number(*content.get(&i).unwrap_or(&0) as f64))
                    .collect();

                Ok(Value::List(result))
            }
            _ => Err(ErrorSource::Type(
                "interval_class_content requires a pitch class set".to_string(),
            )
            .with_span(*span)),
        }
    }
}

/// Registry for all builtin functions available in the Carmen interpreter.
///
/// The registry manages function registration, lookup, and dispatch. It provides
/// a centralized way to access all builtin functions and their aliases.
pub struct BuiltinRegistry {
    functions: std::collections::HashMap<String, Box<dyn BuiltinFunction>>,
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinRegistry {
    /// Creates a new registry with all builtin functions registered.
    pub fn new() -> Self {
        let mut registry = Self {
            functions: std::collections::HashMap::new(),
        };

        registry.register(Box::new(PitchInterval));
        registry.register(Box::new(Transpose));
        registry.register(Box::new(Invert));
        registry.register(Box::new(PitchClassInterval));
        registry.register(Box::new(NormalForm));
        registry.register(Box::new(PrimeForm));
        registry.register(Box::new(IntervalClassVector));
        registry.register(Box::new(SetClass));
        registry.register(Box::new(IntervalClass));
        registry.register(Box::new(IntervalClassContent));

        registry
    }

    /// Registers a builtin function in the registry.
    ///
    /// # Arguments
    /// * `func` - The function implementation to register
    fn register(&mut self, func: Box<dyn BuiltinFunction>) {
        let name = func.name().to_string();
        self.functions.insert(name, func);
    }

    /// Calls a builtin function by name with the provided arguments.
    ///
    /// # Arguments
    /// * `name` - The function name to call
    /// * `args` - Positional arguments to pass to the function
    /// * `kwargs` - Named arguments to pass to the function
    /// * `span` - Source location for error reporting
    ///
    /// # Returns
    /// The result of the function call
    ///
    /// # Errors
    /// Returns an error if the function is not found or the call fails
    pub fn call(
        &self,
        name: &str,
        args: &[Value],
        kwargs: &[(String, Value)],
        span: &Span,
    ) -> Result<Value> {
        self.functions
            .get(name)
            .ok_or_else(|| {
                ErrorSource::Runtime(format!("Unknown builtin function: {name}")).with_span(*span)
            })?
            .call(args, kwargs, span)
    }

    /// Returns a list of all registered function names.
    ///
    /// This is useful for auto-completion and help systems.
    pub fn get_function_names(&self) -> Vec<&str> {
        self.functions.keys().map(|s| s.as_str()).collect()
    }
}

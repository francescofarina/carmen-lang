//! Carmen language interpreter.
//!
//! This module contains the core interpreter for the Carmen music programming language.
//! It provides runtime evaluation of Carmen AST nodes, value representation, environment
//! management, and execution of musical programs.
//!
//! # Key Components
//!
//! * [`Value`] - Runtime representation of all Carmen values
//! * [`Interpreter`] - Main interpreter engine that evaluates Carmen programs
//! * [`Environment`] - Variable scope management with builtin function registration
//! * [`BuiltinRegistry`] - Registry of builtin functions available to programs
//!
//! # Value System
//!
//! The interpreter uses a dynamic type system where all runtime values are represented
//! by the [`Value`] enum. This includes:
//! - Basic types: numbers, strings, booleans
//! - Musical types: pitches, chords, durations, dynamics
//! - Collection types: lists, tuples, sets
//! - Structural types: musical events, parts, scores
//! - Function types: user-defined and builtin functions
//!
//! # Execution Model
//!
//! Programs are executed statement by statement, with expressions evaluated recursively.
//! The interpreter maintains a stack of scopes for variable binding and supports
//! control flow constructs like conditionals, loops, and function calls.

pub mod arg_extraction;
pub mod builtins;

use crate::ast::*;
use crate::common::fraction::Fraction;
use crate::core::*;
use crate::errors::{AddSpan, CarmenError, CoreResult, ErrorSource, Result};
use builtins::BuiltinRegistry;
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

/// Runtime representation of all Carmen values.
///
/// This enum encompasses all possible values that can exist during Carmen program execution,
/// from basic types like numbers and strings to complex musical structures like scores.
/// The type system is dynamic, allowing values to be converted and combined at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Floating-point number (covers integers as well)
    Number(f64),
    /// Text string
    String(String),
    /// Boolean true/false value
    Boolean(bool),
    /// Musical pitch with octave information
    Pitch(Pitch),
    /// Pitch class without octave (0-11)
    PitchClass(PitchClass),
    /// Set of pitch classes for set theory analysis
    PitchClassSet(PitchClassSet),
    /// Musical duration (note length)
    Duration(Duration),
    /// Dynamic marking (pp, mf, ff, etc.)
    Dynamic(Dynamic),
    /// Performance attribute (staccato, legato, etc.)
    Attribute(Attribute),
    /// Musical chord (multiple simultaneous pitches)
    Chord(Chord),
    /// Musical rest (silence)
    Rest,
    /// Ordered collection of values
    List(Vec<Value>),
    /// Fixed-size ordered collection, often used for chords
    Tuple(Vec<Value>),
    /// Unordered collection of unique values
    Set(Vec<Value>),
    /// Musical event with timing and content
    MusicalEvent(MusicalEvent),
    /// Musical part containing staves and events
    Part(Part),
    /// Musical staff with multiple voices
    Staff(Staff),
    /// Timeline containing multiple parts
    Timeline(Timeline),
    /// Movement containing a timeline and metadata
    Movement(Movement),
    /// Complete musical score with movements or timeline
    Score(Score),
    /// Metadata key for score/part/staff annotation
    MetadataKey(MetadataKey),
    /// Metadata value for score/part/staff annotation
    MetadataValue(MetadataValue),
    /// Context change (tempo, key signature, etc.)
    ContextChange(ContextChange),
    /// User-defined function with parameters and body
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<SpannedStatement>,
    },
    /// Reference to a builtin function
    BuiltinFunction(String),
    /// Null/empty value
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Pitch(p) => write!(f, "{}", p.to_note_name()),
            Value::PitchClass(pc) => write!(f, "{}", pc.to_note_name()),
            Value::PitchClassSet(pcs) => {
                let mut classes: Vec<_> = pcs.classes.iter().collect();
                classes.sort_by_key(|pc| pc.0); // sort by pitch class number

                let names: Vec<String> = classes.iter().map(|&pc| format!("{}", pc.0)).collect();
                write!(f, "{{{}}}", names.join(", "))
            }
            Value::Duration(d) => write!(f, "{}", d.to_fractional_string()),
            Value::Dynamic(d) => write!(f, "{d:?}"),
            Value::Attribute(attr) => write!(f, "{attr:?}"),
            Value::Rest => write!(f, "~"),
            Value::Chord(chord) => {
                let notes: Vec<String> = chord
                    .pitches
                    .iter()
                    .map(|pitch| pitch.to_note_name())
                    .collect();
                write!(f, "({})", notes.join(", "))
            }
            Value::List(items) => {
                let formatted: Vec<String> = items.iter().map(|v| format!("{v}")).collect();
                write!(f, "[{}]", formatted.join(", "))
            }
            Value::Tuple(items) => {
                let formatted: Vec<String> = items.iter().map(|v| format!("{v}")).collect();
                write!(f, "({})", formatted.join(", "))
            }
            Value::Set(items) => {
                let formatted: Vec<String> = items.iter().map(|v| format!("{v}")).collect();
                write!(f, "{{{}}}", formatted.join(", "))
            }
            Value::MusicalEvent(event) => {
                let duration_str = event.duration.to_fractional_string();
                let content_str = match &event.content {
                    EventContent::Note(pitch) => pitch.to_note_name(),
                    EventContent::Chord(chord) => {
                        let notes: Vec<String> = chord
                            .pitches
                            .iter()
                            .map(|pitch| pitch.to_note_name())
                            .collect();
                        format!("({})", notes.join(", "))
                    }
                    EventContent::Rest => "~".to_string(),
                    EventContent::Sequence(events) => {
                        format!("[{} events]", events.len())
                    }
                };

                let mut parts = vec![duration_str, content_str];

                if let Some(dynamic) = &event.dynamic {
                    parts.push(format!("{dynamic:?}").to_lowercase());
                }

                if !event.attributes.is_empty() {
                    let attrs: Vec<String> = event
                        .attributes
                        .iter()
                        .map(|a| format!("{a:?}").to_lowercase())
                        .collect();
                    parts.push(attrs.join(" "));
                }

                write!(f, "{}", parts.join(" "))
            }
            Value::Part(part) => {
                let event_count = part.total_event_count();
                let mut result = if let Some(name) = &part.name {
                    format!("Part \"{name}\"")
                } else {
                    "Part".to_string()
                };

                if !part.staves.is_empty() {
                    result.push_str(&format!(
                        " ({} staves, {} events, {:.1}s)",
                        part.staves.len(),
                        event_count,
                        part.total_duration()
                    ));

                    // Add staff breakdown
                    let staff_info: Vec<String> = part
                        .staves
                        .iter()
                        .map(|s| {
                            let voices_count = s.voices.len();
                            let events_count: usize = s.voices.iter().map(|v| v.events.len()).sum();
                            let voice_str = if voices_count == 1 { "voice" } else { "voices" };
                            format!(
                                "S{}: {} {}, {} events",
                                s.number, voices_count, voice_str, events_count
                            )
                        })
                        .collect();

                    if staff_info.len() <= 4 {
                        result.push_str(&format!(" [{}]", staff_info.join(", ")));
                    } else {
                        result.push_str(&format!(
                            " [{}... +{}]",
                            staff_info[..3].join(", "),
                            staff_info.len() - 3
                        ));
                    }
                } else {
                    result.push_str(&format!(
                        " ({} events, {:.1}s)",
                        event_count,
                        part.total_duration()
                    ));
                }

                write!(f, "{result}")
            }

            Value::Staff(staff) => {
                let voices_count = staff.voices.len();
                let events_count: usize = staff.voices.iter().map(|v| v.events.len()).sum();
                let voice_str = if voices_count == 1 { "voice" } else { "voices" };
                write!(
                    f,
                    "Staff {} ({} {}, {} events, {:.1}s)",
                    staff.number,
                    voices_count,
                    voice_str,
                    events_count,
                    staff.total_duration()
                )
            }
            Value::Timeline(timeline) => {
                let total_events: usize =
                    timeline.parts.iter().map(|p| p.total_event_count()).sum();
                let total_staves: usize =
                    timeline.parts.iter().map(|p| p.staves.len().max(1)).sum();

                let mut result = format!(
                    "Timeline ({} parts, {} staves, {} events, {:.1}s)",
                    timeline.parts.len(),
                    total_staves,
                    total_events,
                    timeline.total_duration()
                );

                // Add part breakdown
                let part_info: Vec<String> = timeline
                    .parts
                    .iter()
                    .map(|p| {
                        let name = if let Some(name) = &p.name {
                            format!("\"{name}\"")
                        } else {
                            "Unnamed".to_string()
                        };

                        if p.staves.is_empty() {
                            format!("{}: {}", name, p.events.len())
                        } else {
                            format!("{}: {}s+{}", name, p.staves.len(), p.events.len())
                        }
                    })
                    .collect();

                if !part_info.is_empty() && part_info.len() <= 3 {
                    result.push_str(&format!(" [{}]", part_info.join(", ")));
                } else if !part_info.is_empty() {
                    result.push_str(&format!(
                        " [{}... +{}]",
                        part_info[..2].join(", "),
                        part_info.len() - 2
                    ));
                }

                write!(f, "{result}")
            }
            Value::Movement(movement) => {
                let mut result = if let Some(name) = &movement.name {
                    format!("Movement \"{name}\"")
                } else {
                    "Movement".to_string()
                };

                let total_events: usize = movement
                    .timeline
                    .parts
                    .iter()
                    .map(|p| p.total_event_count())
                    .sum();
                result.push_str(&format!(
                    " ({} parts, {} events, {:.1}s)",
                    movement.timeline.parts.len(),
                    total_events,
                    movement.total_duration()
                ));

                // Add part summary
                let part_info: Vec<String> = movement
                    .timeline
                    .parts
                    .iter()
                    .map(|p| {
                        if let Some(name) = &p.name {
                            format!("\"{name}\"")
                        } else {
                            "Unnamed".to_string()
                        }
                    })
                    .collect();

                if !part_info.is_empty() {
                    if part_info.len() <= 3 {
                        result.push_str(&format!(" [{}]", part_info.join(", ")));
                    } else {
                        result.push_str(&format!(
                            " [{}... +{} more]",
                            part_info[..2].join(", "),
                            part_info.len() - 2
                        ));
                    }
                }

                write!(f, "{result}")
            }
            Value::Score(score) => {
                let mut result = if let Some(name) = &score.name {
                    format!("Score \"{name}\"")
                } else {
                    "Score".to_string()
                };

                if score.is_multi_movement() {
                    let total_parts: usize =
                        score.movements.iter().map(|m| m.timeline.parts.len()).sum();
                    let total_events: usize = score
                        .movements
                        .iter()
                        .map(|m| {
                            m.timeline
                                .parts
                                .iter()
                                .map(|p| p.total_event_count())
                                .sum::<usize>()
                        })
                        .sum();

                    result.push_str(&format!(
                        " ({} movements, {} parts total, {} events, {:.1}s)",
                        score.movements.len(),
                        total_parts,
                        total_events,
                        score.total_duration()
                    ));

                    // Add movement summary
                    let movement_names: Vec<String> = score
                        .movements
                        .iter()
                        .enumerate()
                        .map(|(i, m)| {
                            if let Some(name) = &m.name {
                                format!("\"{name}\"")
                            } else {
                                format!("Movement {}", i + 1)
                            }
                        })
                        .collect();

                    if movement_names.len() <= 3 {
                        result.push_str(&format!(" [{}]", movement_names.join(", ")));
                    } else {
                        result.push_str(&format!(
                            " [{}... +{} more]",
                            movement_names[..2].join(", "),
                            movement_names.len() - 2
                        ));
                    }
                } else if let Some(timeline) = &score.timeline {
                    let total_events: usize =
                        timeline.parts.iter().map(|p| p.total_event_count()).sum();
                    result.push_str(&format!(
                        " ({} parts, {} events, {:.1}s)",
                        timeline.parts.len(),
                        total_events,
                        score.total_duration()
                    ));
                }

                write!(f, "{result}")
            }
            Value::Function { name, params, .. } => {
                write!(f, "Function {}({})", name, params.join(", "))
            }
            Value::BuiltinFunction(name) => {
                write!(f, "BuiltinFunction {name}")
            }
            Value::MetadataKey(key) => write!(f, "MetadataKey({key:?})"),
            Value::MetadataValue(value) => write!(f, "MetadataValue({value:?})"),
            Value::ContextChange(change) => write!(f, "ContextChange({change:?})"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    /// Returns the type name of this value as a string.
    ///
    /// Used for error messages and debugging output.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
            Value::Pitch(_) => "pitch",
            Value::PitchClass(_) => "pitch_class",
            Value::PitchClassSet(_) => "pitch_class_set",
            Value::Duration(_) => "duration",
            Value::Dynamic(_) => "dynamic",
            Value::Attribute(_) => "attribute",
            Value::Chord(_) => "chord",
            Value::Rest => "rest",
            Value::List(_) => "list",
            Value::Tuple(_) => "tuple",
            Value::Set(_) => "set",
            Value::MusicalEvent(_) => "musical_event",
            Value::Part(_) => "part",
            Value::Staff(_) => "staff",
            Value::Timeline(_) => "timeline",
            Value::Movement(_) => "movement",
            Value::Score(_) => "score",
            Value::MetadataKey(_) => "metadata_key",
            Value::MetadataValue(_) => "metadata_value",
            Value::ContextChange(_) => "context_change",
            Value::Function { .. } => "function",
            Value::BuiltinFunction(_) => "builtin_function",
            Value::Nil => "nil",
        }
    }

    /// Determines if this value is considered "truthy" in boolean contexts.
    ///
    /// # Truthy values
    /// - All values except: `false`, `nil`, `0.0`, and empty strings/lists
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            _ => true,
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(value)
    }
}

impl From<Value> for Option<f64> {
    fn from(value: Value) -> Self {
        match value {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }
}

impl From<Value> for Option<bool> {
    fn from(value: Value) -> Self {
        Some(value.is_truthy())
    }
}

impl From<PitchClass> for Value {
    fn from(pc: PitchClass) -> Self {
        Value::PitchClass(pc)
    }
}

impl From<Value> for Option<PitchClass> {
    fn from(value: Value) -> Self {
        match value {
            Value::PitchClass(pc) => Some(pc),
            Value::Number(n) if (0.0..=11.0).contains(&n) && n.fract() == 0.0 => {
                Some(PitchClass::new(n as u8))
            }
            _ => None,
        }
    }
}

impl From<Pitch> for Value {
    fn from(pitch: Pitch) -> Self {
        Value::Pitch(pitch)
    }
}

impl From<Value> for Option<Pitch> {
    fn from(value: Value) -> Self {
        match value {
            Value::Pitch(p) => Some(p),
            _ => None,
        }
    }
}

impl TryFrom<SpannedLiteral> for Value {
    type Error = CarmenError;

    fn try_from(literal: SpannedLiteral) -> Result<Self> {
        match literal.node {
            Literal::Number(value) => Ok(Value::Number(value)),
            Literal::String(value) => Ok(Value::String(value)),
            Literal::Boolean(value) => Ok(Value::Boolean(value)),
            Literal::Pitch {
                note,
                accidentals,
                octave,
            } => {
                if let Some(octave) = octave {
                    Pitch::from_note_name(&note, accidentals, octave as i8)
                        .map(Value::Pitch)
                        .map_err(|e| e.with_span(literal.span))
                } else {
                    // Pitch class
                    PitchClass::from_note_name(&note, accidentals)
                        .map(Value::PitchClass)
                        .map_err(|e| e.with_span(literal.span))
                }
            }
            Literal::Duration {
                numerator,
                denominator,
                dots,
            } => {
                let base_fraction = Fraction::new(numerator, denominator);
                let final_fraction = base_fraction.with_dots(dots);
                Ok(Value::Duration(Duration {
                    fraction: final_fraction,
                }))
            }
            Literal::Dynamic(value) => Dynamic::from_str(&value)
                .map(Value::Dynamic)
                .map_err(|e| e.with_span(literal.span)),
            Literal::Attribute(value) => Attribute::from_str(&value)
                .map(Value::Attribute)
                .map_err(|e| e.with_span(literal.span)),
            Literal::Rest { .. } => Ok(Value::Rest),
        }
    }
}

impl TryFrom<Value> for MetadataKey {
    type Error = ErrorSource;

    fn try_from(value: Value) -> CoreResult<Self> {
        if let Value::String(s) = value {
            match s.as_str() {
                "title" => Ok(MetadataKey::Title),
                "composer" => Ok(MetadataKey::Composer),
                "copyright" => Ok(MetadataKey::Copyright),
                "tempo" => Ok(MetadataKey::Tempo),
                "time_signature" => Ok(MetadataKey::TimeSignature),
                "key_signature" => Ok(MetadataKey::KeySignature),
                "clef" => Ok(MetadataKey::Clef),
                "instrument" => Ok(MetadataKey::Instrument),
                "channel" => Ok(MetadataKey::Channel),
                "offset" => Ok(MetadataKey::Offset),
                _ => Err(ErrorSource::Argument(format!("Invalid metadata key: {s}"))),
            }
        } else {
            Err(ErrorSource::Type(
                "Expected string for metadata key".to_string(),
            ))
        }
    }
}

impl TryFrom<Value> for MetadataValue {
    type Error = ErrorSource;

    fn try_from(value: Value) -> CoreResult<Self> {
        match value {
            Value::String(s) => Ok(MetadataValue::String(s)),
            Value::Number(n) => Ok(MetadataValue::Number(n)),
            Value::List(l) => {
                if l.len() == 2 {
                    if let (Value::Number(num), Value::Number(den)) = (&l[0], &l[1]) {
                        if num.fract() == 0.0 && den.fract() == 0.0 && *den != 0.0 {
                            return Ok(MetadataValue::TimeSignature(*num as u32, *den as u32));
                        }
                    }
                }
                Err(ErrorSource::Type(
                    "Expected [numerator, denominator] for TimeSignature".to_string(),
                ))
            }

            _ => Err(ErrorSource::Type(format!(
                "Unsupported value type for metadata: {}",
                value.type_name()
            ))),
        }
    }
}

/// Variable binding environment with scope management.
///
/// The environment maintains a stack of scopes for variable lookup and binding.
/// It includes a registry of builtin functions that are automatically available
/// to all Carmen programs.
pub struct Environment {
    /// Stack of variable scopes (innermost scope is last)
    scopes: Vec<HashMap<String, Value>>,
    /// Registry of builtin functions
    builtins: BuiltinRegistry,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    /// Creates a new environment with a global scope and registered builtins.
    pub fn new() -> Self {
        let mut env = Self {
            scopes: vec![HashMap::new()],
            builtins: BuiltinRegistry::new(),
        };
        env.define_builtins();
        env
    }

    /// Pushes a new variable scope onto the stack.
    ///
    /// Used when entering function calls or block expressions.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pops the most recent variable scope from the stack.
    ///
    /// The global scope is never popped, ensuring at least one scope remains.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Defines a variable in the current (innermost) scope.
    ///
    /// If the variable already exists in the current scope, it is overwritten.
    pub fn define(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value);
        }
    }

    /// Looks up a variable by name, searching from innermost to outermost scope.
    ///
    /// Returns the first matching variable found, or `None` if not found.
    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Registers all builtin functions and their aliases in the global scope.
    fn define_builtins(&mut self) {
        // Register builtin functions
        let function_names: Vec<String> = self
            .builtins
            .get_function_names()
            .into_iter()
            .map(|name| name.to_string())
            .collect();
        for name in &function_names {
            self.define(name, Value::BuiltinFunction(name.clone()));
        }

        // Register aliases
        self.define("pi", Value::BuiltinFunction("pitch_interval".to_string()));
        self.define("T", Value::BuiltinFunction("transpose".to_string()));
        self.define(
            "pci",
            Value::BuiltinFunction("pitch_class_interval".to_string()),
        );
        self.define("I", Value::BuiltinFunction("invert".to_string()));
        self.define("ic", Value::BuiltinFunction("interval_class".to_string()));
        self.define(
            "ic_vector",
            Value::BuiltinFunction("interval_class_vector".to_string()),
        )
    }
}

/// Control flow states during program execution.
///
/// Used to handle early returns from functions and control structures.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    /// Normal execution continues
    None,
    /// Return from current function with a value
    Return(Value),
}

/// Result of executing a statement or expression.
///
/// Distinguishes between normal value results and control flow changes.
#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionResult {
    /// Normal execution produced a value
    Value(Value),
    /// Control flow change (return, etc.)
    Control(ControlFlow),
}

impl ExecutionResult {
    /// Converts this execution result into a value.
    ///
    /// Control flow results are converted to their contained values,
    /// with `ControlFlow::None` becoming `Value::Nil`.
    pub fn into_value(self) -> Value {
        match self {
            ExecutionResult::Value(val) => val,
            ExecutionResult::Control(ControlFlow::Return(val)) => val,
            ExecutionResult::Control(ControlFlow::None) => Value::Nil,
        }
    }

    /// Returns `true` if this result represents a return statement.
    pub fn is_return(&self) -> bool {
        matches!(self, ExecutionResult::Control(ControlFlow::Return(_)))
    }
}

/// The main Carmen interpreter.
///
/// Evaluates Carmen AST nodes and executes programs. Maintains program state
/// including variable bindings and timing information for musical constructs.
pub struct Interpreter {
    /// Variable binding environment
    environment: Environment,
    /// Current time offset for musical event placement
    current_time_offset: Fraction,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    /// Creates a new interpreter with a fresh environment.
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            current_time_offset: Fraction::new(0, 1),
        }
    }

    /// Returns all available symbols (variables and functions) in the environment.
    ///
    /// Useful for auto-completion and introspection in REPL environments.
    pub fn get_all_symbols(&self) -> Vec<String> {
        let mut symbols = Vec::new();
        for scope in &self.environment.scopes {
            symbols.extend(scope.keys().cloned());
        }
        symbols.extend(
            self.environment
                .builtins
                .get_function_names()
                .into_iter()
                .map(String::from),
        );
        symbols.sort();
        symbols.dedup();
        symbols
    }

    /// Gets the value of a variable by name.
    ///
    /// Returns `None` if the variable is not defined.
    pub fn get_variable(&self, name: &str) -> Option<Value> {
        self.environment.get(name).cloned()
    }

    /// Interprets a complete Carmen program.
    ///
    /// Executes all statements in sequence and returns the value of the last
    /// expression statement, or the returned value if a return statement is encountered.
    pub fn interpret(&mut self, program: &Program) -> Result<Value> {
        let mut last_value = Value::Nil;

        for statement in &program.statements {
            match self.execute_statement(statement)? {
                ExecutionResult::Value(val) => last_value = val,
                ExecutionResult::Control(ControlFlow::Return(val)) => return Ok(val),
                ExecutionResult::Control(ControlFlow::None) => {}
            }
        }

        Ok(last_value)
    }

    /// Creates a properly typed metadata value from a generic value.
    ///
    /// Validates that the value is appropriate for the given metadata key
    /// and converts it to the correct `MetadataValue` variant.
    fn create_metadata_value(
        &self,
        key: &MetadataKey,
        value: Value,
        span: &crate::errors::Span,
    ) -> Result<MetadataValue> {
        match key {
            MetadataKey::Title
            | MetadataKey::Composer
            | MetadataKey::Copyright
            | MetadataKey::Instrument => match value {
                Value::String(s) => Ok(MetadataValue::String(s)),
                _ => Err(
                    ErrorSource::Type(format!("Expected string for metadata key '{key}'"))
                        .with_span(*span),
                ),
            },
            MetadataKey::Tempo | MetadataKey::Channel | MetadataKey::Offset => match value {
                Value::Number(n) => Ok(MetadataValue::Number(n)),
                _ => Err(
                    ErrorSource::Type(format!("Expected number for metadata key '{key}'"))
                        .with_span(*span),
                ),
            },
            MetadataKey::TimeSignature => match value {
                Value::Tuple(l) if l.len() == 2 => {
                    if let (Value::Number(num), Value::Number(den)) = (&l[0], &l[1]) {
                        if num.fract() == 0.0 && den.fract() == 0.0 && *den > 0.0 {
                            return Ok(MetadataValue::TimeSignature(*num as u32, *den as u32));
                        }
                    }
                    Err(ErrorSource::Type(
                        "Expected tuple of two numbers for time signature".to_string(),
                    )
                    .with_span(*span))
                }
                _ => Err(ErrorSource::Type(
                    "Expected tuple of (num, den) for time signature".to_string(),
                )
                .with_span(*span)),
            },
            MetadataKey::KeySignature => match value {
                Value::String(s) => {
                    let parts: Vec<&str> = s.split_whitespace().collect();
                    let key_name = parts[0];
                    let mode = if parts.len() > 1 {
                        parts[1].to_lowercase()
                    } else {
                        "major".to_string()
                    };

                    let major_keys = ["C", "G", "D", "A", "E", "B", "F#", "C#"];
                    let minor_keys = ["A", "E", "B", "F#", "C#", "G#", "D#", "A#"];

                    match mode.as_str() {
                        "major" => {
                            if let Some(pos) = major_keys
                                .iter()
                                .position(|&k| k.eq_ignore_ascii_case(key_name))
                            {
                                Ok(MetadataValue::KeySignature(KeySignature::Major(pos as u8)))
                            } else {
                                Err(ErrorSource::Argument(format!(
                                    "Unsupported major key signature '{s}'"
                                ))
                                .with_span(*span))
                            }
                        }
                        "minor" => {
                            if let Some(pos) = minor_keys
                                .iter()
                                .position(|&k| k.eq_ignore_ascii_case(key_name))
                            {
                                Ok(MetadataValue::KeySignature(KeySignature::Minor(pos as u8)))
                            } else {
                                Err(ErrorSource::Argument(format!(
                                    "Unsupported minor key signature '{s}'"
                                ))
                                .with_span(*span))
                            }
                        }
                        _ => Err(ErrorSource::Argument(format!(
                            "Unsupported key signature mode '{mode}'"
                        ))
                        .with_span(*span)),
                    }
                }
                _ => Err(
                    ErrorSource::Type("Expected string for key signature".to_string())
                        .with_span(*span),
                ),
            },
            MetadataKey::Clef => match value {
                Value::String(s) => Clef::from_str(&s)
                    .map(MetadataValue::Clef)
                    .map_err(|e| e.with_span(*span)),
                _ => {
                    Err(ErrorSource::Type("Expected string for clef".to_string()).with_span(*span))
                }
            },
        }
    }

    /// Propagates octave information through a sequence of values.
    ///
    /// When a pitch is encountered, its octave becomes the default for subsequent
    /// pitch classes. This allows writing `[c4, d, e, f]` instead of `[c4, d4, e4, f4]`.
    fn propagate_implicit_octaves(&self, values: &mut Vec<Value>) {
        let mut last_octave: Option<i8> = None;
        for value in values {
            process_value_octaves(value, &mut last_octave);
        }
    }

    /// Executes a single statement and returns the result.
    ///
    /// Handles variable declarations, function definitions, return statements,
    /// metadata declarations, and expression statements.
    fn execute_statement(&mut self, statement: &SpannedStatement) -> Result<ExecutionResult> {
        match &statement.node {
            Statement::Expression(expr) => {
                // For REPL: expression statements should return their value
                // The distinction between printing/not printing is handled at REPL level
                let val = self.evaluate_expression(expr)?;
                Ok(ExecutionResult::Value(val))
            }
            Statement::VariableDeclaration { name, value, .. } => {
                let val = self.evaluate_expression(value)?;
                self.environment.define(name, val.clone());
                Ok(ExecutionResult::Value(val))
            }
            Statement::FunctionDeclaration {
                name, params, body, ..
            } => {
                let function = Value::Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                };
                self.environment.define(name, function.clone());
                Ok(ExecutionResult::Value(function))
            }
            Statement::Return { value, .. } => {
                let val = if let Some(expr) = value {
                    self.evaluate_expression(expr)?
                } else {
                    Value::Nil
                };
                Ok(ExecutionResult::Control(ControlFlow::Return(val)))
            }
            Statement::Metadata { key, value, .. } => {
                let evaluated_value = self.evaluate_expression(value)?;
                let metadata_key = MetadataKey::try_from(Value::String(key.clone()))
                    .map_err(|e| e.with_span(value.span))?;
                let metadata_value =
                    self.create_metadata_value(&metadata_key, evaluated_value, &value.span)?;

                let change = ContextChange {
                    key: metadata_key,
                    value: metadata_value,
                    time_offset: self.current_time_offset,
                };
                Ok(ExecutionResult::Value(Value::ContextChange(change)))
            }
        }
    }

    /// Builds a musical voice from a sequence of musical events.
    ///
    /// Calculates timing offsets for each event based on their durations.
    fn build_voice(&mut self, events: Vec<Value>) -> Result<Voice> {
        let mut voice = Voice::new();
        let mut voice_time_offset = Fraction::new(0, 1);
        for event_val in events {
            if let Value::MusicalEvent(event) = event_val {
                let mut event_with_offset = event.clone();
                event_with_offset.offset = voice_time_offset;
                voice.add_event(event_with_offset);
                voice_time_offset += event.total_duration();
            }
        }
        Ok(voice)
    }

    /// Builds a musical staff from tuple elements representing voices.
    ///
    /// Each element in the tuple becomes a separate voice on the staff.
    fn build_staff_from_tuple(
        &mut self,
        elements: Vec<Value>,
        num_staves: usize,
        span: &crate::errors::Span,
    ) -> Result<Staff> {
        let mut staff = Staff::new((num_staves + 1) as u32);
        for voice_val in elements {
            let events_for_voice = match voice_val {
                Value::List(events) => events,
                Value::MusicalEvent(event) => match event.content {
                    EventContent::Sequence(events) => {
                        events.into_iter().map(Value::MusicalEvent).collect()
                    }
                    _ => vec![Value::MusicalEvent(event)],
                },
                _ => {
                    return Err(ErrorSource::Runtime(
                        "Tuples in a part must contain lists or musical events.".to_string(),
                    )
                    .with_span(*span));
                }
            };
            let voice = self.build_voice(events_for_voice)?;
            staff = staff.add_voice(voice);
        }
        Ok(staff)
    }

    /// Evaluates an expression and returns its value.
    ///
    /// This is the core evaluation function that handles all expression types
    /// including literals, variables, function calls, control structures, and
    /// musical constructs.
    fn evaluate_expression(&mut self, expression: &SpannedExpression) -> Result<Value> {
        match &expression.node {
            Expression::Literal(literal) => SpannedLiteral {
                node: literal.clone(),
                span: expression.span,
            }
            .try_into(),
            Expression::Identifier(name) => self.environment.get(name).cloned().ok_or_else(|| {
                ErrorSource::Runtime(format!("Undefined variable '{name}'"))
                    .with_span(expression.span)
            }),
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.evaluate_binary_op(&left_val, operator, &right_val, &expression.span)
            }
            Expression::Unary { operator, operand } => {
                let val = self.evaluate_expression(operand)?;
                self.evaluate_unary_op(operator, &val, &expression.span)
            }
            Expression::Call {
                callee,
                args,
                kwargs,
            } => {
                let function = self.evaluate_expression(callee)?;
                let arg_values: Result<Vec<Value>> = args
                    .iter()
                    .map(|arg| self.evaluate_expression(arg))
                    .collect();
                let arg_values = arg_values?;

                let kwarg_values: Result<Vec<(String, Value)>> = kwargs
                    .iter()
                    .map(|(name, expr)| Ok((name.clone(), self.evaluate_expression(expr)?)))
                    .collect();
                let kwarg_values = kwarg_values?;

                self.call_function(&function, &arg_values, &kwarg_values, &expression.span)
            }
            Expression::Pipe { left, right } => {
                let left_val = self.evaluate_expression(left)?;
                match right.as_ref() {
                    Spanned {
                        node:
                            Expression::Call {
                                callee,
                                args,
                                kwargs,
                            },
                        ..
                    } => {
                        let function = self.evaluate_expression(callee)?;
                        let mut arg_values = vec![left_val];
                        for arg in args {
                            arg_values.push(self.evaluate_expression(arg)?);
                        }

                        let kwarg_values: Result<Vec<(String, Value)>> = kwargs
                            .iter()
                            .map(|(name, expr)| Ok((name.clone(), self.evaluate_expression(expr)?)))
                            .collect();
                        let kwarg_values = kwarg_values?;
                        self.call_function(&function, &arg_values, &kwarg_values, &expression.span)
                    }
                    _ => Err(ErrorSource::Runtime(
                        "Pipe right side must be a function call".to_string(),
                    )
                    .with_span(expression.span)),
                }
            }
            Expression::List { elements, .. } => {
                let values: Result<Vec<Value>> = elements
                    .iter()
                    .map(|elem| self.evaluate_expression(elem))
                    .collect();
                let mut values = values?;
                self.propagate_implicit_octaves(&mut values);
                Ok(Value::List(values))
            }
            Expression::Tuple { elements, .. } => {
                let values: Result<Vec<Value>> = elements
                    .iter()
                    .map(|elem| self.evaluate_expression(elem))
                    .collect();
                let mut values = values?;
                self.propagate_implicit_octaves(&mut values);

                // If all elements are pitches, create a chord
                if values.iter().all(|v| matches!(v, Value::Pitch(_))) {
                    let pitches: Vec<Pitch> = values
                        .into_iter()
                        .map(|v| match v {
                            Value::Pitch(p) => p,
                            _ => unreachable!(),
                        })
                        .collect();
                    Ok(Value::Chord(Chord { pitches }))
                } else {
                    Ok(Value::Tuple(values))
                }
            }
            Expression::Set { elements, .. } => {
                let values: Result<Vec<Value>> = elements
                    .iter()
                    .map(|elem| self.evaluate_expression(elem))
                    .collect();
                Ok(Value::Set(values?))
            }
            Expression::Block { statements, .. } => {
                self.environment.push_scope();
                let mut last_value = Value::Nil;

                for stmt in statements {
                    match self.execute_statement(stmt)? {
                        ExecutionResult::Value(val) => last_value = val,
                        ExecutionResult::Control(flow) => {
                            self.environment.pop_scope();
                            return Ok(ExecutionResult::Control(flow).into_value());
                        }
                    }
                }

                self.environment.pop_scope();
                Ok(last_value)
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let condition_val = self.evaluate_expression(condition)?;

                if condition_val.is_truthy() {
                    self.evaluate_expression(then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_expression(else_expr)
                } else {
                    Ok(Value::Nil)
                }
            }
            Expression::For {
                variable,
                iterable,
                body,
            } => {
                let iterable_val = self.evaluate_expression(iterable)?;

                match iterable_val {
                    Value::List(items) => {
                        self.environment.push_scope();
                        let mut results = Vec::new();

                        for item in items {
                            self.environment.define(variable, item);
                            results.push(self.evaluate_expression(body)?);
                        }

                        self.environment.pop_scope();
                        Ok(Value::List(results))
                    }
                    _ => Err(
                        ErrorSource::Runtime("For loop requires iterable".to_string())
                            .with_span(expression.span),
                    ),
                }
            }
            Expression::While { condition, body } => {
                let mut results = Vec::new();
                let mut iterations = 0;
                const MAX_ITERATIONS: usize = 10000; // TODO: Prevent infinite loops in a better way

                while iterations < MAX_ITERATIONS {
                    let condition_val = self.evaluate_expression(condition)?;
                    if !condition_val.is_truthy() {
                        break;
                    }

                    results.push(self.evaluate_expression(body)?);
                    iterations += 1;
                }

                if iterations >= MAX_ITERATIONS {
                    return Err(ErrorSource::Runtime(
                        "While loop exceeded maximum iterations".to_string(),
                    )
                    .with_span(expression.span));
                }

                Ok(Value::List(results))
            }
            Expression::PitchClassSet { classes, .. } => {
                let pitch_classes: Vec<PitchClass> =
                    classes.iter().map(|&n| PitchClass::new(n)).collect();
                Ok(Value::PitchClassSet(PitchClassSet::new(pitch_classes)))
            }
            Expression::MusicalEvent {
                duration,
                pitches,
                dynamic,
                attributes,
                ..
            } => {
                let duration_val = self.evaluate_expression(duration)?;
                let pitches_val = self.evaluate_expression(pitches)?;

                let duration = match duration_val {
                    Value::Duration(d) => d,
                    _ => {
                        return Err(ErrorSource::Runtime("Expected duration".to_string())
                            .with_span(expression.span))
                    }
                };

                let content =
                    match pitches_val {
                        Value::Pitch(p) => EventContent::Note(p),
                        Value::Chord(c) => EventContent::Chord(c),
                        Value::Rest => EventContent::Rest,
                        Value::List(elements) => {
                            let mut events = Vec::new();
                            let mut current_offset = Fraction::new(0, 1);

                            for element in elements {
                                match element {
                                    Value::MusicalEvent(mut event) => {
                                        event.offset = current_offset;
                                        current_offset += event.duration.fraction;
                                        events.push(event);
                                    }
                                    Value::Pitch(pitch) => {
                                        let mut event = MusicalEvent::note(pitch, duration);
                                        event.offset = current_offset;
                                        events.push(event);
                                        current_offset += duration.fraction;
                                    }
                                    Value::Rest => {
                                        let mut rest_event = MusicalEvent::rest(duration);
                                        rest_event.offset = current_offset;
                                        events.push(rest_event);
                                        current_offset += duration.fraction;
                                    }
                                    Value::Chord(chord) => {
                                        let mut chord_event = MusicalEvent::chord(chord, duration);
                                        chord_event.offset = current_offset;
                                        events.push(chord_event);
                                        current_offset += duration.fraction;
                                    }
                                    _ => return Err(ErrorSource::Runtime(
                                        "Expected musical event, pitch, chord or rest in sequence"
                                            .to_string(),
                                    )
                                    .with_span(expression.span)),
                                }
                            }

                            EventContent::Sequence(events)
                        }
                        Value::Tuple(pitches) => {
                            let pitch_vec: Result<Vec<Pitch>> = pitches
                                .into_iter()
                                .map(|p| match p {
                                    Value::Pitch(pitch) => Ok(pitch),
                                    _ => Err(ErrorSource::Runtime(
                                        "Expected pitch in tuple chord".to_string(),
                                    )
                                    .with_span(expression.span)),
                                })
                                .collect();
                            EventContent::Chord(Chord {
                                pitches: pitch_vec?,
                            })
                        }
                        _ => {
                            return Err(ErrorSource::Runtime(
                                "Expected pitch, chord, or list of pitches".to_string(),
                            )
                            .with_span(expression.span))
                        }
                    };

                let mut event = MusicalEvent {
                    duration,
                    content,
                    dynamic: None,
                    attributes: Vec::new(),
                    offset: Fraction::new(0, 1),
                };

                if let Some(dyn_expr) = dynamic {
                    let dyn_val = self.evaluate_expression(dyn_expr)?;
                    if let Value::Dynamic(d) = dyn_val {
                        event.dynamic = Some(d);
                    }
                }

                for attr_expr in attributes {
                    let attr_val = self.evaluate_expression(attr_expr)?;
                    if let Value::Attribute(a) = attr_val {
                        event.attributes.push(a);
                    }
                }

                Ok(Value::MusicalEvent(event))
            }
            Expression::Part { name, body, .. } => {
                let part_name = if let Some(name_expr) = name {
                    let name_val = self.evaluate_expression(name_expr)?;
                    match name_val {
                        Value::String(s) => Some(s),
                        _ => None,
                    }
                } else {
                    None
                };

                let mut part = Part::new(part_name);
                let initial_time_offset = self.current_time_offset;
                self.current_time_offset = Fraction::new(0, 1); // Reset time for new part scope

                // Handle block bodies specially to collect all statements
                match body.node {
                    Expression::Block { ref statements, .. } => {
                        self.environment.push_scope();
                        let mut collected_changes_in_scope = Vec::new();

                        for stmt in statements {
                            let result = self.execute_statement(stmt)?;
                            match result {
                                ExecutionResult::Value(val) => match val {
                                    Value::MusicalEvent(event) => {
                                        let mut event_with_offset = event.clone();
                                        event_with_offset.offset = self.current_time_offset;
                                        part.add_event(event_with_offset);
                                        self.current_time_offset += event.total_duration();
                                    }
                                    Value::Staff(staff) => {
                                        part.add_staff(staff);
                                    }
                                    Value::ContextChange(change) => {
                                        collected_changes_in_scope.push(change);
                                    }
                                    Value::List(items) => {
                                        for item in items {
                                            if let Value::MusicalEvent(event) = item {
                                                let mut event_with_offset = event.clone();
                                                event_with_offset.offset = self.current_time_offset;
                                                part.add_event(event_with_offset);
                                                self.current_time_offset += event.total_duration();
                                            }
                                        }
                                    }
                                    Value::Tuple(elements) => {
                                        let staff = self.build_staff_from_tuple(
                                            elements,
                                            part.staves.len(),
                                            &stmt.span,
                                        )?;
                                        part.add_staff(staff);
                                    }
                                    _ => {}
                                },
                                ExecutionResult::Control(_) => break,
                            }
                        }

                        self.environment.pop_scope();
                        part.context_changes.extend(collected_changes_in_scope);
                    }
                    _ => {
                        // Handle non-block bodies
                        let body_val = self.evaluate_expression(body)?;
                        match body_val {
                            Value::MusicalEvent(event) => {
                                let mut event_with_offset = event.clone();
                                event_with_offset.offset = self.current_time_offset;
                                part.add_event(event_with_offset);
                                self.current_time_offset += event.total_duration();
                            }
                            Value::Staff(staff) => {
                                part.add_staff(staff);
                            }
                            Value::ContextChange(change) => {
                                part.add_context_change(change);
                            }
                            Value::List(items) => {
                                for item in items {
                                    if let Value::MusicalEvent(event) = item {
                                        let mut event_with_offset = event.clone();
                                        event_with_offset.offset = self.current_time_offset;
                                        part.add_event(event_with_offset);
                                        self.current_time_offset += event.total_duration();
                                    }
                                }
                            }
                            Value::Tuple(elements) => {
                                let staff = self.build_staff_from_tuple(
                                    elements,
                                    part.staves.len(),
                                    &expression.span,
                                )?;
                                part.add_staff(staff);
                            }
                            _ => {}
                        }
                    }
                }
                self.current_time_offset = initial_time_offset; // Restore time
                Ok(Value::Part(part))
            }
            Expression::Staff { number, body, .. } => {
                let number_val = self.evaluate_expression(number)?;
                let number = match number_val {
                    Value::Number(n) => n as u32,
                    _ => {
                        return Err(
                            ErrorSource::Runtime("Expected number for staff".to_string())
                                .with_span(expression.span),
                        )
                    }
                };

                let mut staff = Staff::new(number);
                let initial_time_offset = self.current_time_offset;
                self.current_time_offset = Fraction::new(0, 1); // Reset time for new staff scope

                let body_val = self.evaluate_expression(body)?;
                match body_val {
                    Value::MusicalEvent(event) => {
                        let voice = self.build_voice(vec![Value::MusicalEvent(event)])?;
                        staff = staff.add_voice(voice);
                    }
                    Value::List(events) => {
                        let voice = self.build_voice(events)?;
                        staff = staff.add_voice(voice);
                    }
                    Value::Tuple(elements) => {
                        for voice_val in elements {
                            let events_for_voice = match voice_val {
                                Value::List(events) => events,
                                Value::MusicalEvent(event) => match event.content {
                                    EventContent::Sequence(events) => {
                                        events.into_iter().map(Value::MusicalEvent).collect()
                                    }
                                    _ => vec![Value::MusicalEvent(event)],
                                },
                                _ => {
                                    return Err(ErrorSource::Runtime(
                                        "Tuples in a staff must contain lists or musical events."
                                            .to_string(),
                                    )
                                    .with_span(expression.span));
                                }
                            };
                            let voice = self.build_voice(events_for_voice)?;
                            staff = staff.add_voice(voice);
                        }
                    }
                    Value::ContextChange(change) => {
                        staff.add_context_change(change);
                    }
                    _ => {}
                }

                // After evaluating the main content, check for any context changes within the block
                if let Expression::Block { statements, .. } = &body.node {
                    for stmt in statements {
                        if let Statement::Metadata { key, value, .. } = &stmt.node {
                            let evaluated_value = self.evaluate_expression(value)?;
                            let metadata_key = MetadataKey::try_from(Value::String(key.clone()))
                                .map_err(|e| e.with_span(value.span))?;
                            let metadata_value = self.create_metadata_value(
                                &metadata_key,
                                evaluated_value,
                                &value.span,
                            )?;
                            let change = ContextChange {
                                key: metadata_key,
                                value: metadata_value,
                                time_offset: self.current_time_offset,
                            };
                            staff.add_context_change(change);
                        }
                    }
                }

                self.current_time_offset = initial_time_offset; // Restore time
                Ok(Value::Staff(staff))
            }
            Expression::Timeline { body, .. } => {
                let mut timeline = Timeline::default();
                let initial_time_offset = self.current_time_offset;
                self.current_time_offset = Fraction::new(0, 1); // Reset time for new timeline scope

                // Handle block bodies specially to collect all statements
                match body.node {
                    Expression::Block { ref statements, .. } => {
                        self.environment.push_scope();
                        let mut collected_changes_in_scope = Vec::new();

                        for stmt in statements {
                            let result = self.execute_statement(stmt)?;
                            match result {
                                ExecutionResult::Value(val) => match val {
                                    Value::Part(part) => {
                                        timeline.add_part(part);
                                    }
                                    Value::List(parts) => {
                                        for part_val in parts {
                                            if let Value::Part(part) = part_val {
                                                timeline.add_part(part);
                                            }
                                        }
                                    }
                                    Value::ContextChange(change) => {
                                        collected_changes_in_scope.push(change);
                                    }
                                    _ => {}
                                },
                                ExecutionResult::Control(_) => break,
                            }
                        }

                        self.environment.pop_scope();
                        timeline.context_changes.extend(collected_changes_in_scope);
                    }
                    _ => {
                        let body_val = self.evaluate_expression(body)?;
                        match body_val {
                            Value::Part(part) => {
                                timeline.add_part(part);
                            }
                            Value::List(parts) => {
                                for part_val in parts {
                                    if let Value::Part(part) = part_val {
                                        timeline.add_part(part);
                                    }
                                }
                            }
                            Value::ContextChange(change) => {
                                timeline.add_context_change(change);
                            }
                            _ => {}
                        }
                    }
                }
                self.current_time_offset = initial_time_offset; // Restore time
                Ok(Value::Timeline(timeline))
            }
            Expression::Movement { name, body, .. } => {
                let movement_name = if let Some(name_expr) = name {
                    let name_val = self.evaluate_expression(name_expr)?;
                    match name_val {
                        Value::String(s) => Some(s),
                        _ => None,
                    }
                } else {
                    None
                };

                let mut movement = Movement::new(movement_name);
                let initial_time_offset = self.current_time_offset;
                self.current_time_offset = Fraction::new(0, 1); // Reset time for new movement scope

                // Handle block bodies specially to collect all statements
                match body.node {
                    Expression::Block { ref statements, .. } => {
                        self.environment.push_scope();
                        let mut collected_changes_in_scope = Vec::new();

                        for stmt in statements {
                            let result = self.execute_statement(stmt)?;
                            match result {
                                ExecutionResult::Value(val) => match val {
                                    Value::Timeline(timeline) => {
                                        movement = movement.with_timeline(timeline);
                                    }
                                    Value::ContextChange(change) => {
                                        collected_changes_in_scope.push(change);
                                    }
                                    _ => {}
                                },
                                ExecutionResult::Control(_) => break,
                            }
                        }

                        self.environment.pop_scope();
                        movement.context_changes.extend(collected_changes_in_scope);
                    }
                    _ => {
                        let body_val = self.evaluate_expression(body)?;
                        match body_val {
                            Value::Timeline(timeline) => {
                                movement = movement.with_timeline(timeline);
                            }
                            Value::ContextChange(change) => {
                                movement.add_context_change(change);
                            }
                            _ => {
                                return Err(ErrorSource::Runtime(
                                    "Movement body must contain a timeline or context changes"
                                        .to_string(),
                                )
                                .with_span(expression.span))
                            }
                        }
                    }
                }
                self.current_time_offset = initial_time_offset; // Restore time
                Ok(Value::Movement(movement))
            }
            Expression::Score { name, body, .. } => {
                let score_name = if let Some(name_expr) = name {
                    let name_val = self.evaluate_expression(name_expr)?;
                    match name_val {
                        Value::String(s) => Some(s),
                        _ => None,
                    }
                } else {
                    None
                };

                let mut score = Score::default();
                if let Some(name) = score_name {
                    score = score.with_name(name);
                }
                let initial_time_offset = self.current_time_offset;
                self.current_time_offset = Fraction::new(0, 1); // Reset time for new score scope

                // Handle block bodies specially to collect all statements
                match body.node {
                    Expression::Block { ref statements, .. } => {
                        self.environment.push_scope();
                        let mut collected_changes_in_scope = Vec::new();

                        for stmt in statements {
                            let result = self.execute_statement(stmt)?;
                            match result {
                                ExecutionResult::Value(val) => match val {
                                    Value::Timeline(timeline) => {
                                        score.set_timeline(timeline);
                                    }
                                    Value::Movement(movement) => {
                                        score.add_movement(movement);
                                    }
                                    Value::ContextChange(change) => {
                                        collected_changes_in_scope.push(change);
                                    }
                                    Value::List(items) => {
                                        for item in items {
                                            match item {
                                                Value::Movement(movement) => {
                                                    score.add_movement(movement);
                                                }
                                                Value::Timeline(timeline) => {
                                                    score.set_timeline(timeline);
                                                }
                                                Value::ContextChange(change) => {
                                                    collected_changes_in_scope.push(change);
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                    _ => {}
                                },
                                ExecutionResult::Control(_) => break,
                            }
                        }

                        self.environment.pop_scope();
                        score.context_changes.extend(collected_changes_in_scope);
                    }
                    _ => {
                        let body_val = self.evaluate_expression(body)?;
                        match body_val {
                            Value::Timeline(timeline) => {
                                score.set_timeline(timeline);
                            }
                            Value::Movement(movement) => {
                                score.add_movement(movement);
                            }
                            Value::ContextChange(change) => {
                                score.add_context_change(change);
                            }
                            Value::List(items) => {
                                for item in items {
                                    match item {
                                        Value::Movement(movement) => {
                                            score.add_movement(movement);
                                        }
                                        Value::Timeline(timeline) => {
                                            score.set_timeline(timeline);
                                        }
                                        Value::ContextChange(change) => {
                                            score.add_context_change(change);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                self.current_time_offset = initial_time_offset; // Restore time
                Ok(Value::Score(score))
            }
        }
    }

    /// Evaluates a binary operation between two values.
    ///
    /// Handles arithmetic, comparison, logical, and musical operations
    /// like transposition and duration arithmetic.
    fn evaluate_binary_op(
        &self,
        left: &Value,
        operator: &BinaryOp,
        right: &Value,
        span: &crate::errors::Span,
    ) -> Result<Value> {
        match (left, operator, right) {
            // Arithmetic operations
            (Value::Number(a), BinaryOp::Add, Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Number(a), BinaryOp::Subtract, Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::Number(a), BinaryOp::Multiply, Value::Number(b)) => Ok(Value::Number(a * b)),
            (Value::Number(a), BinaryOp::Divide, Value::Number(b)) => {
                if *b == 0.0 {
                    Err(ErrorSource::Runtime("Division by zero".to_string()).with_span(*span))
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (Value::Number(a), BinaryOp::Modulo, Value::Number(b)) => Ok(Value::Number(a % b)),

            // String concatenation
            (Value::String(a), BinaryOp::Add, Value::String(b)) => {
                Ok(Value::String(format!("{a}{b}")))
            }

            // List operations
            (Value::List(a), BinaryOp::Add, Value::List(b)) => {
                let mut result = a.clone();
                result.extend(b.iter().cloned());
                Ok(Value::List(result))
            }
            (Value::List(a), BinaryOp::Multiply, Value::Number(n)) => {
                let n = *n as usize;
                let mut result = Vec::new();
                for _ in 0..n {
                    result.extend(a.clone());
                }
                Ok(Value::List(result))
            }

            // Duration operations
            (Value::Duration(a), BinaryOp::Add, Value::Duration(b)) => Ok(Value::Duration(a + b)),
            (Value::Duration(a), BinaryOp::Multiply, Value::Number(b)) => {
                Ok(Value::Duration(a * (*b as u32)))
            }

            // Pitch transposition
            (Value::Pitch(p), BinaryOp::Add, Value::Number(n)) => {
                Ok(Value::Pitch(p.transpose(*n as i32)))
            }
            (Value::Pitch(p), BinaryOp::Subtract, Value::Number(n)) => {
                Ok(Value::Pitch(p.transpose(-(*n as i32))))
            }

            // Pitch class transposition
            (Value::PitchClass(pc), BinaryOp::Add, Value::Number(n)) => {
                Ok(Value::PitchClass(pc.transpose(*n as i32)))
            }
            (Value::PitchClass(pc), BinaryOp::Subtract, Value::Number(n)) => {
                Ok(Value::PitchClass(pc.transpose(-(*n as i32))))
            }

            // Pitch class set transposition
            (Value::PitchClassSet(pcs), BinaryOp::Add, Value::Number(n)) => {
                Ok(Value::PitchClassSet(pcs.transpose(*n as i32)))
            }
            (Value::PitchClassSet(pcs), BinaryOp::Subtract, Value::Number(n)) => {
                Ok(Value::PitchClassSet(pcs.transpose(-(*n as i32))))
            }

            // Comparison operations
            (Value::Number(a), BinaryOp::Equal, Value::Number(b)) => Ok(Value::Boolean(a == b)),
            (Value::Number(a), BinaryOp::NotEqual, Value::Number(b)) => Ok(Value::Boolean(a != b)),
            (Value::Number(a), BinaryOp::Less, Value::Number(b)) => Ok(Value::Boolean(a < b)),
            (Value::Number(a), BinaryOp::Greater, Value::Number(b)) => Ok(Value::Boolean(a > b)),
            (Value::Number(a), BinaryOp::LessEqual, Value::Number(b)) => Ok(Value::Boolean(a <= b)),
            (Value::Number(a), BinaryOp::GreaterEqual, Value::Number(b)) => {
                Ok(Value::Boolean(a >= b))
            }

            (Value::String(a), BinaryOp::Equal, Value::String(b)) => Ok(Value::Boolean(a == b)),
            (Value::String(a), BinaryOp::NotEqual, Value::String(b)) => Ok(Value::Boolean(a != b)),

            (Value::Boolean(a), BinaryOp::Equal, Value::Boolean(b)) => Ok(Value::Boolean(a == b)),
            (Value::Boolean(a), BinaryOp::NotEqual, Value::Boolean(b)) => {
                Ok(Value::Boolean(a != b))
            }

            // Logical operations
            (Value::Boolean(a), BinaryOp::And, Value::Boolean(b)) => Ok(Value::Boolean(*a && *b)),
            (Value::Boolean(a), BinaryOp::Or, Value::Boolean(b)) => Ok(Value::Boolean(*a || *b)),

            _ => Err(ErrorSource::Type(format!(
                "Unsupported operation: {} {:?} {}",
                left.type_name(),
                operator,
                right.type_name()
            ))
            .with_span(*span)),
        }
    }

    /// Evaluates a unary operation on a value.
    ///
    /// Supports negation for numbers and logical NOT for booleans.
    fn evaluate_unary_op(
        &self,
        operator: &UnaryOp,
        operand: &Value,
        span: &crate::errors::Span,
    ) -> Result<Value> {
        match (operator, operand) {
            (UnaryOp::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOp::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            (UnaryOp::Not, val) => Ok(Value::Boolean(!val.is_truthy())),
            _ => Err(ErrorSource::Type(format!(
                "Unsupported unary operation: {:?} {}",
                operator,
                operand.type_name()
            ))
            .with_span(*span)),
        }
    }

    /// Calls a function (user-defined or builtin) with the given arguments.
    ///
    /// For user-defined functions, creates a new scope, binds parameters,
    /// and executes the function body. For builtin functions, delegates
    /// to the builtin registry.
    fn call_function(
        &mut self,
        function: &Value,
        args: &[Value],
        kwargs: &[(String, Value)],
        span: &crate::errors::Span,
    ) -> Result<Value> {
        match function {
            Value::Function { params, body, .. } => {
                if args.len() != params.len() {
                    return Err(ErrorSource::Runtime(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ))
                    .with_span(*span));
                }

                self.environment.push_scope();

                // Bind parameters
                for (param, arg) in params.iter().zip(args.iter()) {
                    self.environment.define(param, arg.clone());
                }

                // Execute function body
                let mut result = Value::Nil;
                for (i, stmt) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1;

                    match self.execute_statement(stmt)? {
                        ExecutionResult::Value(val) => {
                            if is_last {
                                result = val;
                            }
                        }
                        ExecutionResult::Control(ControlFlow::Return(val)) => {
                            self.environment.pop_scope();
                            return Ok(val);
                        }
                        ExecutionResult::Control(ControlFlow::None) => {}
                    }
                }

                self.environment.pop_scope();
                Ok(result)
            }
            Value::BuiltinFunction(name) => {
                call_builtin_function(name, args, kwargs, span, &self.environment.builtins)
            }
            _ => Err(ErrorSource::Runtime("Not a function".to_string()).with_span(*span)),
        }
    }
}

/// Recursively processes values to propagate octave information.
///
/// Updates the `last_octave` tracker based on pitches encountered,
/// and converts pitch classes to pitches when an octave is available.
fn process_value_octaves(value: &mut Value, last_octave: &mut Option<i8>) {
    match value {
        Value::Pitch(p) => {
            *last_octave = Some(p.octave);
        }
        Value::PitchClass(pc) => {
            if let Some(oct) = *last_octave {
                *value = Value::Pitch(Pitch {
                    pitch_class: *pc,
                    octave: oct,
                });
            }
        }
        Value::List(values) | Value::Tuple(values) => {
            for val in values {
                process_value_octaves(val, last_octave);
            }
        }
        Value::MusicalEvent(event) => {
            // Update current_time_offset based on the event's duration
            *last_octave = match &event.content {
                EventContent::Note(p) => Some(p.octave),
                EventContent::Chord(c) => c.pitches.last().map(|p| p.octave),
                EventContent::Sequence(events) => events.last().and_then(|e| match &e.content {
                    EventContent::Note(p) => Some(p.octave),
                    EventContent::Chord(c) => c.pitches.last().map(|p| p.octave),
                    _ => None,
                }),
                _ => None,
            };
        }
        Value::ContextChange(_) => {
            // Context changes don't affect octave propagation
        }
        _ => {}
    }
}

/// Calls a builtin function through the registry.
///
/// This is a convenience wrapper around the registry's call method.
fn call_builtin_function(
    name: &str,
    args: &[Value],
    kwargs: &[(String, Value)],
    span: &crate::errors::Span,
    registry: &BuiltinRegistry,
) -> Result<Value> {
    registry.call(name, args, kwargs, span)
}

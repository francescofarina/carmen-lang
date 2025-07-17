//! Abstract Syntax Tree (AST) definitions.
//!
//! This module defines the core AST nodes that represent parsed Carmen programs.
//! The AST is designed to capture both musical and programming constructs while
//! preserving source location information for error reporting and tooling.
//!
//! # Structure
//!
//! The AST is built around three main concepts:
//! - **Literals**: Primitive values like numbers, strings, pitches, durations, dynamics
//! - **Expressions**: Complex expressions including function calls, musical events, control flow
//! - **Statements**: Top-level constructs like variable declarations, function definitions
//!
//! All AST nodes are wrapped in [`Spanned`] to preserve source location information.

pub mod inspector;
pub mod spanned;

use crate::errors::Span;
pub use spanned::Spanned;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::str::FromStr;

/// Literal values in the Carmen language.
///
/// Literals represent primitive values that can be directly written in source code.
/// Carmen supports both traditional programming literals (numbers, strings, booleans)
/// and musical literals (pitches, durations, dynamics, rests).
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Pitch {
        note: String,
        accidentals: i32,
        octave: Option<i32>,
    },
    Duration {
        numerator: u32,
        denominator: u32,
        dots: u32,
    },
    Dynamic(String),
    Attribute(String),
    Rest {
        duration: Option<Box<Literal>>,
    },
}

impl Literal {
    /// Parse a pitch literal from text.
    ///
    /// Accepts formats like:
    /// - `c`, `d`, `e` (pitch classes)
    /// - `c4`, `f#5`, `bb3` (specific pitches)
    /// - Accidentals can be `#`/`s` for sharps, `b` for flats
    pub fn parse_pitch(text: &str) -> Option<Self> {
        let chars: Vec<char> = text.chars().collect();
        if chars.is_empty() {
            return None;
        }

        let note = chars[0].to_ascii_lowercase().to_string();
        if !matches!(note.as_str(), "a" | "b" | "c" | "d" | "e" | "f" | "g") {
            return None;
        }

        let mut i = 1;
        let mut accidentals = 0;

        // Count accidentals
        while i < chars.len() {
            match chars[i] {
                's' | '#' => accidentals += 1,
                'b' => accidentals -= 1,
                _ => break,
            }
            i += 1;
        }

        // Parse octave if present
        let octave = if i < chars.len() {
            let remaining: String = chars[i..].iter().collect();
            // Only accept if the entire remaining string is a valid octave number
            if remaining.chars().all(|c| c.is_ascii_digit()) {
                remaining.parse().ok()
            } else {
                return None; // Invalid characters after note and accidentals
            }
        } else {
            None
        };

        Some(Literal::Pitch {
            note,
            accidentals,
            octave,
        })
    }

    /// Check if the given text represents a valid dynamic marking.
    ///
    /// Recognizes standard dynamic markings from `pppp` to `ffff`.
    pub fn is_dynamic(text: &str) -> bool {
        matches!(
            text,
            "pppp" | "ppp" | "pp" | "p" | "mp" | "mf" | "f" | "ff" | "fff" | "ffff"
        )
    }

    /// Check if the given text represents a valid performance attribute.
    ///
    /// Recognizes common articulation and expression markings.
    pub fn is_attribute(text: &str) -> bool {
        matches!(
            text,
            "staccato" | "legato" | "accent" | "tenuto" | "marcato" | "sforzando"
        )
    }

    /// Parse a duration literal from text.
    ///
    /// Accepts formats like:
    /// - `1/4` (quarter note)
    /// - `1/2.` (dotted half note)
    /// - `3/8` (three eighth notes)
    /// - `2` (two whole notes)
    ///
    /// Dots are represented by trailing periods.
    pub fn parse_duration(text: &str) -> Option<Self> {
        let chars: Vec<char> = text.chars().collect();
        if chars.is_empty() {
            return None;
        }

        let mut dots = 0;
        let mut base_end = chars.len();
        while base_end > 0 && chars[base_end - 1] == '.' {
            dots += 1;
            base_end -= 1;
        }
        let base_str: String = chars[..base_end].iter().collect();

        let (numerator, denominator) = if let Some(slash_pos) = base_str.find('/') {
            let num_str = &base_str[..slash_pos];
            let den_str = &base_str[slash_pos + 1..];
            match (num_str.parse::<u32>(), den_str.parse::<u32>()) {
                (Ok(num), Ok(den)) if den > 0 => (num, den),
                _ => return None,
            }
        } else if let Ok(num) = base_str.parse::<u32>() {
            (num, 1) // Handle whole numbers as x/1
        } else {
            return None;
        };

        Some(Literal::Duration {
            numerator,
            denominator,
            dots,
        })
    }

    /// Parse a rest literal from text.
    ///
    /// Accepts formats like:
    /// - `~` (contextual rest)
    /// - `~1/4` (quarter rest)
    /// - `~1/2.` (dotted half rest)
    pub fn parse_rest(text: &str) -> Option<Self> {
        if !text.starts_with('~') {
            return None;
        }

        if text == "~" {
            // Inline rest
            return Some(Literal::Rest { duration: None });
        }

        // Standalone rest with duration like ~1/4
        let duration_str = &text[1..];
        Self::parse_duration(duration_str)
    }
}

/// Binary operators in Carmen expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Subtract,
    /// Multiplication (`*`)
    Multiply,
    /// Division (`/`)
    Divide,
    /// Modulo (`%`)
    Modulo,
    /// Equality (`==`)
    Equal,
    /// Inequality (`!=`)
    NotEqual,
    /// Less than (`<`)
    Less,
    /// Greater than (`>`)
    Greater,
    /// Less than or equal (`<=`)
    LessEqual,
    /// Greater than or equal (`>=`)
    GreaterEqual,
    /// Logical AND (`and`)
    And,
    /// Logical OR (`or`)
    Or,
}

/// Unary operators in Carmen expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    /// Unary minus (`-`)
    Minus,
    /// Logical NOT (`not`)
    Not,
}

/// Expressions in the Carmen language.
///
/// Expressions represent computations that produce values. This includes
/// traditional programming constructs (arithmetic, function calls, control flow)
/// as well as musical constructs (events, parts, scores).
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    /// Variable reference
    Identifier(String),
    /// Binary operation (e.g., `a + b`, `x == y`)
    Binary {
        left: Box<SpannedExpression>,
        operator: BinaryOp,
        right: Box<SpannedExpression>,
    },
    /// Unary operation (e.g., `-x`, `not flag`)
    Unary {
        operator: UnaryOp,
        operand: Box<SpannedExpression>,
    },
    /// Function call with positional and named arguments
    Call {
        callee: Box<SpannedExpression>,
        args: Vec<SpannedExpression>,
        kwargs: Vec<(String, SpannedExpression)>,
    },
    /// Pipe operation for function composition (e.g., `data |> process`)
    Pipe {
        left: Box<SpannedExpression>,
        right: Box<SpannedExpression>,
    },
    /// List literal (e.g., `[1, 2, 3]`)
    List { elements: Vec<SpannedExpression> },
    /// Tuple literal (e.g., `(1, 2, 3)`)
    Tuple { elements: Vec<SpannedExpression> },
    /// Set literal (e.g., `{1, 2, 3}`)
    Set { elements: Vec<SpannedExpression> },
    /// Block expression containing multiple statements
    Block { statements: Vec<SpannedStatement> },
    /// Conditional expression
    If {
        condition: Box<SpannedExpression>,
        then_branch: Box<SpannedExpression>,
        else_branch: Option<Box<SpannedExpression>>,
    },
    /// For loop expression
    For {
        variable: String,
        iterable: Box<SpannedExpression>,
        body: Box<SpannedExpression>,
    },
    /// While loop expression
    While {
        condition: Box<SpannedExpression>,
        body: Box<SpannedExpression>,
    },
    /// Set of pitch classes for harmonic analysis
    PitchClassSet { classes: Vec<u8> },
    /// Musical event (note, chord, or rest with timing and expression)
    MusicalEvent {
        duration: Box<SpannedExpression>,
        pitches: Box<SpannedExpression>,
        dynamic: Option<Box<SpannedExpression>>,
        attributes: Vec<SpannedExpression>,
    },
    /// Musical part containing a sequence of events
    Part {
        name: Option<Box<SpannedExpression>>,
        body: Box<SpannedExpression>,
    },
    /// Staff within a part (for multi-staff instruments)
    Staff {
        number: Box<SpannedExpression>,
        body: Box<SpannedExpression>,
    },
    /// Timeline for organizing musical events in time
    Timeline { body: Box<SpannedExpression> },
    /// Complete musical score
    Score {
        name: Option<Box<SpannedExpression>>,
        body: Box<SpannedExpression>,
    },
    /// Movement within a larger work
    Movement {
        name: Option<Box<SpannedExpression>>,
        body: Box<SpannedExpression>,
    },
}

/// Statements in the Carmen language.
///
/// Statements represent actions or declarations that don't necessarily
/// produce values. They form the top-level structure of Carmen programs.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Expression used as a statement
    Expression(SpannedExpression),
    /// Variable declaration (`let name = value`)
    VariableDeclaration {
        name: String,
        value: SpannedExpression,
    },
    /// Function declaration (`fn name(params) { body }`)
    FunctionDeclaration {
        name: String,
        params: Vec<String>,
        body: Vec<SpannedStatement>,
    },
    /// Return statement (`return value`)
    Return { value: Option<SpannedExpression> },
    /// Metadata declaration for score information (`@key value`)
    Metadata {
        key: String,
        value: SpannedExpression,
    },
}

/// Expression with source location information
pub type SpannedExpression = Spanned<Expression>;
/// Statement with source location information
pub type SpannedStatement = Spanned<Statement>;
/// Literal with source location information
pub type SpannedLiteral = Spanned<Literal>;

/// Position of a comment relative to AST nodes.
///
/// Comments can appear in various positions relative to the code they
/// document, and this enum captures those relationships for proper
/// formatting and preservation during transformations.
#[derive(Debug, Clone, PartialEq)]
pub enum CommentPosition {
    /// Comment appears before the associated AST node
    Leading,
    /// Comment appears after the associated AST node on the same line
    Trailing,
    /// Comment appears between elements in a container (index of preceding element)
    Between(usize),
    /// Standalone comment not associated with any specific AST node
    Standalone,
}

/// Information about a comment in the source code.
///
/// Comments are preserved during parsing to enable formatting tools
/// and documentation generation. This struct tracks both the comment
/// content and its relationship to nearby AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub struct CommentInfo {
    /// Text content of the comment (without comment markers)
    pub content: String,
    /// Source location of the comment
    pub span: Span,
    /// Position relative to associated AST nodes
    pub position: CommentPosition,
    /// Span of the AST node this comment is associated with
    pub associated_span: Option<Span>,
}

impl CommentInfo {
    /// Create a new standalone comment.
    pub fn new(content: String, span: Span) -> Self {
        Self {
            content,
            span,
            position: CommentPosition::Standalone,
            associated_span: None,
        }
    }

    /// Associate this comment with an AST node at the given position.
    pub fn with_position(mut self, position: CommentPosition, associated_span: Span) -> Self {
        self.position = position;
        self.associated_span = Some(associated_span);
        self
    }

    /// Check if this comment appears on the same line as the given span.
    pub fn is_same_line(&self, other_span: &Span) -> bool {
        self.span.start.line == other_span.end.line
    }

    /// Check if this comment appears before the given span.
    pub fn is_before(&self, other_span: &Span) -> bool {
        self.span.end.offset < other_span.end.offset
    }
}

/// Root AST node representing a complete Carmen program.
///
/// A program consists of a sequence of statements and any comments
/// found in the source. Comments are preserved to enable formatting
/// and documentation tools.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// Top-level statements in the program
    pub statements: Vec<SpannedStatement>,
    /// All comments found in the source
    pub comments: Vec<CommentInfo>,
    /// Span covering the entire program
    pub span: Span,
}

impl Program {
    /// Get all comments that appear before the given span
    pub fn leading_comments(&self, span: &Span) -> Vec<&CommentInfo> {
        self.comments
            .iter()
            .filter(|comment| {
                matches!(comment.position, CommentPosition::Leading)
                    && (comment.associated_span.as_ref() == Some(span))
            })
            .collect()
    }

    /// Get all comments that appear after the given span on the same line
    pub fn trailing_comments(&self, span: &Span) -> Vec<&CommentInfo> {
        self.comments
            .iter()
            .filter(|comment| {
                matches!(comment.position, CommentPosition::Trailing)
                    && (comment.associated_span.as_ref() == Some(span))
            })
            .collect()
    }

    /// Get comments between elements in a list/tuple/etc
    pub fn between_comments(
        &self,
        container_span: &Span,
        element_index: usize,
    ) -> Vec<&CommentInfo> {
        self.comments
            .iter()
            .filter(|comment| {
                matches!(comment.position, CommentPosition::Between(idx) if idx == element_index)
                    && comment.associated_span.as_ref().is_some_and(|s| {
                        s.start.offset >= container_span.start.offset
                            && s.end.offset <= container_span.end.offset
                    })
            })
            .collect()
    }

    /// Get all standalone comments (not associated with specific code)
    pub fn standalone_comments(&self) -> Vec<&CommentInfo> {
        self.comments
            .iter()
            .filter(|comment| matches!(comment.position, CommentPosition::Standalone))
            .collect()
    }

    /// Get all comments within a given span range
    pub fn comments_in_range(&self, start_offset: usize, end_offset: usize) -> Vec<&CommentInfo> {
        self.comments
            .iter()
            .filter(|comment| {
                comment.span.start.offset >= start_offset && comment.span.end.offset <= end_offset
            })
            .collect()
    }

    /// Check if there are any comments between two spans
    pub fn has_comments_between(&self, span1: &Span, span2: &Span) -> bool {
        self.comments.iter().any(|comment| {
            comment.span.start.offset > span1.end.offset
                && comment.span.end.offset < span2.start.offset
        })
    }
}
/// Display implementation for literals that produces valid Carmen syntax.
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Literal::Number(value) => write!(f, "{value}"),
            Literal::String(value) => write!(f, "\"{value}\""),
            Literal::Boolean(value) => write!(f, "{value}"),
            Literal::Pitch {
                note,
                accidentals,
                octave,
                ..
            } => {
                write!(f, "{}", note.to_uppercase())?;
                for _ in 0..*accidentals {
                    write!(f, "#")?;
                }
                for _ in 0..accidentals.abs() {
                    if *accidentals < 0 {
                        write!(f, "b")?;
                    }
                }
                if let Some(oct) = octave {
                    write!(f, "{oct}")?;
                }
                Ok(())
            }
            Literal::Duration {
                numerator,
                denominator,
                dots,
            } => {
                write!(f, "{numerator}/{denominator}")?;
                for _ in 0..*dots {
                    write!(f, ".")?;
                }
                Ok(())
            }
            Literal::Dynamic(value) => write!(f, "{value}"),
            Literal::Attribute(value) => write!(f, "{value}"),
            Literal::Rest { duration, .. } => {
                write!(f, "~")?;
                if let Some(dur) = duration {
                    write!(f, "{dur}")?;
                }
                Ok(())
            }
        }
    }
}

/// Display implementation for expressions that produces human-readable output.
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Expression::Literal(lit) => write!(f, "{lit}"),
            Expression::Identifier(name) => write!(f, "{name}"),
            Expression::List { elements, .. } => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "]")
            }
            Expression::Tuple { elements, .. } => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Expression::Set { elements, .. } => {
                write!(f, "{{")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "}}")
            }
            Expression::PitchClassSet { classes, .. } => {
                let names: Vec<String> = classes.iter().map(|&n| format!("{n}")).collect();
                write!(f, "{{{}}}", names.join(", "))
            }
            Expression::MusicalEvent {
                duration,
                pitches,
                dynamic,
                attributes,
                ..
            } => {
                write!(f, "{duration} {pitches}")?;
                if let Some(dyn_val) = dynamic {
                    write!(f, " {dyn_val}")?;
                }
                for attr in attributes {
                    write!(f, " {attr}")?;
                }
                Ok(())
            }
            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => {
                write!(f, "({} {} {})", left, format_binary_op(operator), right)
            }
            Expression::Call {
                callee,
                args,
                kwargs,
                ..
            } => {
                write!(f, "{callee}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                for (name, value) in kwargs {
                    if !args.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}={value}")?;
                }
                write!(f, ")")
            }
            Expression::Pipe { left, right, .. } => {
                write!(f, "{left} |> {right}")
            }
            Expression::Block { statements, .. } => {
                write!(f, "{{ {} statements }}", statements.len())
            }
            _ => write!(
                f,
                "[{}]",
                std::any::type_name::<Self>()
                    .split("::")
                    .last()
                    .unwrap_or("Expression")
            ),
        }
    }
}

/// Convert a binary operator to its string representation.
fn format_binary_op(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Modulo => "%",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::Greater => ">",
        BinaryOp::LessEqual => "<=",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
    }
}

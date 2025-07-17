//! AST inspection and debugging utilities.
//!
//! This module provides tools for examining and debugging Carmen AST structures.
//! The primary purpose is to generate human-readable representations of parsed
//! programs for debugging, testing, and development purposes.
//!
//! # Key Features
//!
//! - Hierarchical tree-like display of AST nodes
//! - Musical literal interpretation (showing MIDI values, note names, etc.)
//! - Comment preservation and positioning information
//! - Span information for source location tracking

use crate::ast::*;
use crate::common::fraction::Fraction;
use crate::core::*;
use std::fmt::Write;

/// AST inspector for generating detailed, human-readable representations of Carmen programs.
///
/// The inspector traverses the AST and produces a hierarchical text representation
/// that shows the structure, types, and values of all nodes. This is particularly
/// useful for debugging parser issues, understanding AST structure, and testing.
///
/// The output includes:
/// - Node types and hierarchical structure
/// - Literal values with musical interpretations
/// - Source location information
/// - Comment positions and content
/// - Musical semantics (MIDI values, note names, etc.)
pub struct AstInspector {
    /// Current indentation level for hierarchical output
    indent_level: usize,
    /// Accumulated output string
    output: String,
}

impl Default for AstInspector {
    fn default() -> Self {
        Self::new()
    }
}

impl AstInspector {
    /// Create a new AST inspector.
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            output: String::new(),
        }
    }

    /// Generate a complete dump of a Carmen program.
    ///
    /// This is the main entry point for AST inspection. It produces a comprehensive
    /// representation showing all statements, comments, and their relationships.
    ///
    /// # Arguments
    ///
    /// * `program` - The parsed Carmen program to inspect
    ///
    /// # Returns
    ///
    /// A formatted string containing the complete AST dump
    pub fn dump_program(&mut self, program: &Program) -> String {
        self.output.clear();
        self.indent_level = 0;

        self.writeln("=== CARMEN AST DUMP ===");
        self.writeln("");

        if !program.comments.is_empty() {
            self.writeln(&format!("Comments: {} total", program.comments.len()));
            for comment in &program.comments {
                self.indent();
                self.writeln(&format!(
                    "// {} ({})",
                    comment.content.trim(),
                    self.format_position(&comment.position)
                ));
            }
            self.writeln("");
        }

        self.writeln(&format!("Statements: {} total", program.statements.len()));
        self.writeln("");

        for (i, statement) in program.statements.iter().enumerate() {
            self.writeln(&format!(
                "Statement {} ({}:{}):",
                i + 1,
                statement.span.start.line,
                statement.span.start.column
            ));
            self.indent_level += 1;
            self.dump_statement(statement);
            self.indent_level -= 1;
            self.writeln("");
        }

        self.output.clone()
    }

    /// Generate output for a single statement.
    ///
    /// Recursively processes the statement and any nested expressions or statements,
    /// producing indented output that shows the hierarchical structure.
    fn dump_statement(&mut self, statement: &SpannedStatement) {
        match &statement.node {
            Statement::Expression(expr) => {
                self.indent();
                self.writeln("Type: Expression Statement");
                self.indent();
                self.writeln("Expression:");
                self.indent_level += 1;
                self.dump_expression(expr);
                self.indent_level -= 1;
            }
            Statement::VariableDeclaration { name, value, .. } => {
                self.indent();
                self.writeln("Type: Variable Declaration");
                self.indent();
                self.writeln(&format!("Name: {name}"));
                self.indent();
                self.writeln("Value:");
                self.indent_level += 1;
                self.dump_expression(value);
                self.indent_level -= 1;
            }
            Statement::FunctionDeclaration {
                name, params, body, ..
            } => {
                self.indent();
                self.writeln("Type: Function Declaration");
                self.indent();
                self.writeln(&format!("Name: {name}"));
                self.indent();
                self.writeln(&format!("Parameters: [{}]", params.join(", ")));
                self.indent();
                self.writeln(&format!("Body: {} statements", body.len()));

                self.indent_level += 1;
                for (i, stmt) in body.iter().enumerate() {
                    self.indent();
                    self.writeln(&format!("Body Statement {}:", i + 1));
                    self.indent_level += 1;
                    self.dump_statement(stmt);
                    self.indent_level -= 1;
                }
                self.indent_level -= 1;
            }
            Statement::Return { value, .. } => {
                self.indent();
                self.writeln("Type: Return Statement");
                if let Some(val) = value {
                    self.indent();
                    self.writeln("Value:");
                    self.indent_level += 1;
                    self.dump_expression(val);
                    self.indent_level -= 1;
                } else {
                    self.indent();
                    self.writeln("Value: None");
                }
            }
            Statement::Metadata { key, value, .. } => {
                self.indent();
                self.writeln("Type: Metadata");
                self.indent();
                self.writeln(&format!("Key: {key}"));
                self.indent();
                self.writeln("Value:");
                self.indent_level += 1;
                self.dump_expression(value);
                self.indent_level -= 1;
            }
        }
    }

    /// Generate output for a single expression.
    ///
    /// Handles all expression types, including musical constructs like events,
    /// parts, and scores. Shows both the logical structure and any musical
    /// semantics (e.g., MIDI values for pitches).
    fn dump_expression(&mut self, expression: &SpannedExpression) {
        match &expression.node {
            Expression::Literal(literal) => self.dump_literal(literal),
            Expression::Identifier(name) => {
                self.indent();
                self.writeln("Type: Identifier");
                self.indent();
                self.writeln(&format!("Name: {name}"));
            }
            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => {
                self.indent();
                self.writeln("Type: Binary Operation");
                self.indent();
                self.writeln(&format!("Operator: {operator:?}"));
                self.indent();
                self.writeln("Left:");
                self.indent_level += 1;
                self.dump_expression(left);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Right:");
                self.indent_level += 1;
                self.dump_expression(right);
                self.indent_level -= 1;
            }
            Expression::Unary {
                operator, operand, ..
            } => {
                self.indent();
                self.writeln("Type: Unary Operation");
                self.indent();
                self.writeln(&format!("Operator: {operator:?}"));
                self.indent();
                self.writeln("Operand:");
                self.indent_level += 1;
                self.dump_expression(operand);
                self.indent_level -= 1;
            }
            Expression::Call {
                callee,
                args,
                kwargs,
                ..
            } => {
                self.indent();
                self.writeln("Type: Function Call");
                self.indent();
                self.writeln("Function:");
                self.indent_level += 1;
                self.dump_expression(callee);
                self.indent_level -= 1;

                if !args.is_empty() {
                    self.indent();
                    self.writeln(&format!("Arguments: {} total", args.len()));
                    self.indent_level += 1;
                    for (i, arg) in args.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Arg {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_expression(arg);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }

                if !kwargs.is_empty() {
                    self.indent();
                    self.writeln(&format!("Named Arguments: {} total", kwargs.len()));
                    self.indent_level += 1;
                    for (name, value) in kwargs {
                        self.indent();
                        self.writeln(&format!("{name}:"));
                        self.indent_level += 1;
                        self.dump_expression(value);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::Pipe { left, right, .. } => {
                self.indent();
                self.writeln("Type: Pipe Operation");
                self.indent();
                self.writeln("Input:");
                self.indent_level += 1;
                self.dump_expression(left);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Function:");
                self.indent_level += 1;
                self.dump_expression(right);
                self.indent_level -= 1;
            }
            Expression::List { elements, .. } => {
                self.indent();
                self.writeln(&format!("Type: List ({} elements)", elements.len()));
                if !elements.is_empty() {
                    self.indent_level += 1;
                    for (i, element) in elements.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Element {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_expression(element);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::Tuple { elements, .. } => {
                self.indent();
                self.writeln(&format!("Type: Tuple ({} elements)", elements.len()));
                if !elements.is_empty() {
                    self.indent_level += 1;
                    for (i, element) in elements.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Element {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_expression(element);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::Set { elements, .. } => {
                self.indent();
                self.writeln(&format!("Type: Set ({} elements)", elements.len()));
                if !elements.is_empty() {
                    self.indent_level += 1;
                    for (i, element) in elements.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Element {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_expression(element);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::Block { statements, .. } => {
                self.indent();
                self.writeln(&format!("Type: Block ({} statements)", statements.len()));
                if !statements.is_empty() {
                    self.indent_level += 1;
                    for (i, stmt) in statements.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Statement {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_statement(stmt);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.indent();
                self.writeln("Type: If Expression");
                self.indent();
                self.writeln("Condition:");
                self.indent_level += 1;
                self.dump_expression(condition);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Then:");
                self.indent_level += 1;
                self.dump_expression(then_branch);
                self.indent_level -= 1;
                if let Some(else_expr) = else_branch {
                    self.indent();
                    self.writeln("Else:");
                    self.indent_level += 1;
                    self.dump_expression(else_expr);
                    self.indent_level -= 1;
                }
            }
            Expression::For {
                variable,
                iterable,
                body,
                ..
            } => {
                self.indent();
                self.writeln("Type: For Loop");
                self.indent();
                self.writeln(&format!("Variable: {variable}"));
                self.indent();
                self.writeln("Iterable:");
                self.indent_level += 1;
                self.dump_expression(iterable);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::While {
                condition, body, ..
            } => {
                self.indent();
                self.writeln("Type: While Loop");
                self.indent();
                self.writeln("Condition:");
                self.indent_level += 1;
                self.dump_expression(condition);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::PitchClassSet { classes, .. } => {
                self.indent();
                self.writeln("Type: Pitch Class Set");
                self.indent();
                self.writeln(&format!("Classes: {classes:?}"));
                self.indent();
                self.writeln(&format!(
                    "Note Names: [{}]",
                    classes
                        .iter()
                        .map(|&pc| { PitchClass::new(pc).to_note_name() })
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
            Expression::MusicalEvent {
                duration,
                pitches,
                dynamic,
                attributes,
                ..
            } => {
                self.indent();
                self.writeln("Type: Musical Event");
                self.indent();
                self.writeln("Duration:");
                self.indent_level += 1;
                self.dump_expression(duration);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Pitches:");
                self.indent_level += 1;
                self.dump_expression(pitches);
                self.indent_level -= 1;

                if let Some(dyn_expr) = dynamic {
                    self.indent();
                    self.writeln("Dynamic:");
                    self.indent_level += 1;
                    self.dump_expression(dyn_expr);
                    self.indent_level -= 1;
                }

                if !attributes.is_empty() {
                    self.indent();
                    self.writeln(&format!("Attributes: {} total", attributes.len()));
                    self.indent_level += 1;
                    for (i, attr) in attributes.iter().enumerate() {
                        self.indent();
                        self.writeln(&format!("Attribute {}:", i + 1));
                        self.indent_level += 1;
                        self.dump_expression(attr);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }
            }
            Expression::Part { name, body, .. } => {
                self.indent();
                self.writeln("Type: Part");
                if let Some(name_expr) = name {
                    self.indent();
                    self.writeln("Name:");
                    self.indent_level += 1;
                    self.dump_expression(name_expr);
                    self.indent_level -= 1;
                }
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::Staff { number, body, .. } => {
                self.indent();
                self.writeln("Type: Staff");
                self.indent();
                self.writeln("Number:");
                self.indent_level += 1;
                self.dump_expression(number);
                self.indent_level -= 1;
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::Timeline { body, .. } => {
                self.indent();
                self.writeln("Type: Timeline");
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::Movement { name, body, .. } => {
                self.indent();
                self.writeln("Type: Movement");
                if let Some(name_expr) = name {
                    self.indent();
                    self.writeln("Name:");
                    self.indent_level += 1;
                    self.dump_expression(name_expr);
                    self.indent_level -= 1;
                }
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
            Expression::Score { name, body, .. } => {
                self.indent();
                self.writeln("Type: Score");
                if let Some(name_expr) = name {
                    self.indent();
                    self.writeln("Name:");
                    self.indent_level += 1;
                    self.dump_expression(name_expr);
                    self.indent_level -= 1;
                }
                self.indent();
                self.writeln("Body:");
                self.indent_level += 1;
                self.dump_expression(body);
                self.indent_level -= 1;
            }
        }
    }

    /// Generate output for a literal value.
    ///
    /// For musical literals (pitches, durations, dynamics), this includes
    /// additional semantic information like MIDI values, note names, and
    /// calculated durations.
    fn dump_literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Number(value) => {
                self.indent();
                self.writeln("Type: Number");
                self.indent();
                self.writeln(&format!("Value: {value}"));
            }
            Literal::String(value) => {
                self.indent();
                self.writeln("Type: String");
                self.indent();
                self.writeln(&format!("Value: \"{value}\""));
            }
            Literal::Boolean(value) => {
                self.indent();
                self.writeln("Type: Boolean");
                self.indent();
                self.writeln(&format!("Value: {value}"));
            }
            Literal::Pitch {
                note,
                accidentals,
                octave,
                ..
            } => {
                self.indent();
                self.writeln("Type: Pitch");
                self.indent();
                self.writeln(&format!("Note: {}", note.to_uppercase()));
                self.indent();
                self.writeln(&format!("Accidentals: {accidentals}"));
                if let Some(oct) = octave {
                    self.indent();
                    self.writeln(&format!("Octave: {oct}"));
                } else {
                    self.indent();
                    self.writeln("Octave: None (Pitch Class)");
                }

                // Show MIDI equivalent if possible
                if let Some(oct) = octave {
                    if let Ok(pitch) = Pitch::from_note_name(note, *accidentals, *oct as i8) {
                        self.indent();
                        self.writeln(&format!("MIDI Note: {}", pitch.to_midi()));
                        self.indent();
                        self.writeln(&format!("Display: {}", pitch.to_note_name()));
                    }
                }
            }
            Literal::Duration {
                numerator,
                denominator,
                dots,
                ..
            } => {
                self.indent();
                self.writeln("Type: Duration");
                self.indent();
                self.writeln(&format!("Base: {numerator}/{denominator}"));
                self.indent();
                self.writeln(&format!("Dots: {dots}"));

                let duration = Duration {
                    fraction: Fraction::new(*numerator, *denominator),
                }
                .with_dots(*dots);
                self.indent();
                self.writeln(&format!("Total: {}", duration.fraction.to_f64()));
                self.indent();
                self.writeln(&format!("Display: {}", duration.to_fractional_string()));
            }
            Literal::Dynamic(value) => {
                self.indent();
                self.writeln("Type: Dynamic");
                self.indent();
                self.writeln(&format!("Value: {value}"));

                if let Ok(dynamic) = Dynamic::from_str(value) {
                    self.indent();
                    self.writeln(&format!("MIDI Velocity: {}", dynamic.to_velocity()));
                }
            }
            Literal::Attribute(value) => {
                self.indent();
                self.writeln("Type: Attribute");
                self.indent();
                self.writeln(&format!("Value: {value}"));
            }
            Literal::Rest { duration, .. } => {
                self.indent();
                self.writeln("Type: Rest");
                if let Some(dur) = duration {
                    self.indent();
                    self.writeln("Duration:");
                    self.indent_level += 1;
                    self.dump_literal(dur);
                    self.indent_level -= 1;
                } else {
                    self.indent();
                    self.writeln("Duration: Inherited from context");
                }
            }
        }
    }

    /// Format a comment position for display.
    ///
    /// Converts the [`CommentPosition`] enum into a human-readable string
    /// that describes where the comment appears relative to AST nodes.
    fn format_position(&self, position: &CommentPosition) -> String {
        match position {
            CommentPosition::Leading => "leading".to_string(),
            CommentPosition::Trailing => "trailing".to_string(),
            CommentPosition::Between(idx) => format!("between elements at {idx}"),
            CommentPosition::Standalone => "standalone".to_string(),
        }
    }

    /// Add indentation to the output based on the current nesting level.
    fn indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str("  ");
        }
    }

    /// Write a line of text to the output with a newline.
    fn writeln(&mut self, text: &str) {
        writeln!(self.output, "{text}").unwrap();
    }
}

/// Convenient function to inspect a Carmen program AST.
///
/// This is the primary public interface for AST inspection. It creates
/// an inspector instance and generates a complete dump of the program.
///
/// # Arguments
///
/// * `program` - The parsed Carmen program to inspect
///
/// # Returns
///
/// A formatted string containing the complete AST structure
pub fn inspect_ast(program: &Program) -> String {
    let mut dumper = AstInspector::new();
    dumper.dump_program(program)
}

/// Write AST inspection output directly to a file.
///
/// This convenience function combines AST inspection with file output,
/// useful for generating debug dumps during development or testing.
///
/// # Arguments
///
/// * `program` - The parsed Carmen program to inspect
/// * `output_path` - Path where the output file should be written
///
/// # Returns
///
/// `Ok(())` on success, or an I/O error if file writing fails
pub fn inspect_ast_to_file(program: &Program, output_path: &str) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::Write;

    let content = inspect_ast(program);
    let mut file = File::create(output_path)?;
    file.write_all(content.as_bytes())?;
    Ok(())
}

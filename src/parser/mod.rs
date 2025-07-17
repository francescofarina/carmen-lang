//! Parser for the Carmen language.
//!
//! This module provides a recursive descent parser that transforms a stream of tokens
//! from the lexer into an Abstract Syntax Tree (AST). The parser handles:
//!
//! - Musical expressions (notes, chords, rhythms)
//! - Control flow (if/else, for loops, while loops)
//! - Function and variable declarations
//! - Music-specific constructs (parts, staves, timelines, scores)
//! - Comments and their positioning context
//!
//! The parser uses precedence climbing for binary operators and includes special
//! handling for musical notation where context determines token interpretation.

use crate::ast::*;
use crate::errors::{AddSpan, CarmenError, ErrorSource, Result, Span};
use crate::lexer::{Token, TokenType};

/// Categorizes tokens for identifier validation during parsing.
///
/// Used to provide better error messages when reserved words are used
/// as identifiers in contexts where they shouldn't be.
#[derive(Debug, PartialEq)]
enum TokenCategory {
    /// Token can be used as an identifier
    ValidIdentifier,
    /// Token is reserved (pitch, duration, keyword, etc.)
    Reserved(&'static str), // contains type of reserved symbol
    /// Token cannot be used as an identifier
    Invalid,
}

fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Creates plural form of symbol type names for error messages.
fn pluralize_symbol_type(symbol_type: &str) -> String {
    match symbol_type {
        "pitch" => "Pitches".to_string(),
        "duration" => "Durations".to_string(),
        "dynamic" => "Dynamics".to_string(),
        "attribute" => "Attributes".to_string(),
        "keyword" => "Keywords".to_string(),
        _ => format!("{}s", capitalize_first(symbol_type)),
    }
}

/// Context for comment collection to determine appropriate positioning.
///
/// Comments in Carmen can be positioned relative to code elements for better
/// formatting and semantic meaning. This enum tracks the context to determine
/// where comments should be associated.
#[derive(Debug, Clone)]
enum CommentContext {
    /// Comments that appear before the given span
    Leading(Span),
    /// Comments that appear after the given span
    Trailing(Span),
    /// Comments between list/sequence elements at the given index
    Between(usize),
    /// Comments not associated with any specific code element
    Standalone,
}

/// Recursive descent parser for the Carmen language.
///
/// The parser consumes a stream of tokens and produces an Abstract Syntax Tree (AST).
/// It handles operator precedence, musical notation context, and comment positioning.
pub struct Parser {
    /// The complete list of tokens to parse
    tokens: Vec<Token>,
    /// Current position in the token stream
    current: usize,
    /// Collected comments with positioning information
    comments: Vec<CommentInfo>,
}

impl Parser {
    /// Creates a new parser with the given token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            comments: Vec::new(),
        }
    }

    /// Collects comments from the current position based on the given context.
    ///
    /// This method determines how comments should be positioned relative to code
    /// elements and stores them for later use in formatting or semantic analysis.
    fn collect_comments(&mut self, context: CommentContext) -> Vec<CommentInfo> {
        let mut collected = Vec::new();

        while !self.is_at_end() {
            if let TokenType::Comment(content) = &self.current_token().token_type {
                let comment_span = self.current_token().span;
                let mut comment = CommentInfo::new(content.clone(), comment_span);

                comment.position = match context {
                    CommentContext::Leading(ref target_span) => {
                        if comment.is_same_line(target_span) {
                            CommentPosition::Trailing
                        } else {
                            CommentPosition::Leading
                        }
                    }
                    CommentContext::Trailing(ref target_span) => {
                        comment.associated_span = Some(*target_span);
                        if comment.is_same_line(target_span) {
                            CommentPosition::Trailing
                        } else {
                            CommentPosition::Standalone
                        }
                    }
                    CommentContext::Between(index) => CommentPosition::Between(index),
                    CommentContext::Standalone => CommentPosition::Standalone,
                };

                collected.push(comment.clone());
                self.comments.push(comment);
                self.advance();
            } else {
                break;
            }
        }
        collected
    }

    /// Skips comments while parsing and collects them for later use.
    ///
    /// Comments are preserved for formatting tools and semantic analysis.
    fn skip_comments(&mut self) {
        self.collect_comments(CommentContext::Standalone);
    }

    fn collect_leading_comments(&mut self, target_span: &Span) -> Vec<CommentInfo> {
        self.collect_comments(CommentContext::Leading(*target_span))
    }

    fn collect_trailing_comments(&mut self, target_span: &Span) -> Vec<CommentInfo> {
        self.collect_comments(CommentContext::Trailing(*target_span))
    }

    fn collect_between_comments(&mut self, element_index: usize) -> Vec<CommentInfo> {
        self.collect_comments(CommentContext::Between(element_index))
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current_token().token_type, TokenType::Eof)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous_token()
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.current_token().token_type)
                == std::mem::discriminant(token_type)
        }
    }

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Categorizes a token to determine if it can be used as an identifier.
    ///
    /// This is used, e.g., to provide helpful error messages when reserved words
    /// are used in identifier contexts.
    fn categorize_token(&self, token_type: &TokenType) -> TokenCategory {
        match token_type {
            TokenType::Identifier(_) => TokenCategory::ValidIdentifier,
            TokenType::Pitch(_) => TokenCategory::Reserved("pitch"),
            TokenType::Duration(_) => TokenCategory::Reserved("duration"),
            TokenType::Dynamic(_) => TokenCategory::Reserved("dynamic"),
            TokenType::Attribute(_) => TokenCategory::Reserved("attribute"),
            TokenType::Let
            | TokenType::Def
            | TokenType::If
            | TokenType::Else
            | TokenType::For
            | TokenType::While
            | TokenType::In
            | TokenType::Return
            | TokenType::Part
            | TokenType::Staff
            | TokenType::Timeline
            | TokenType::And
            | TokenType::Or
            | TokenType::Not
            | TokenType::True
            | TokenType::False => TokenCategory::Reserved("keyword"),
            _ => TokenCategory::Invalid,
        }
    }

    /// Extracts the string representation of a token for error messages.
    fn extract_token_value(&self, token_type: &TokenType) -> String {
        match token_type {
            TokenType::Pitch(s)
            | TokenType::Duration(s)
            | TokenType::Dynamic(s)
            | TokenType::Attribute(s)
            | TokenType::Identifier(s) => s.clone(),
            TokenType::Let => "let".to_string(),
            TokenType::Def => "def".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::For => "for".to_string(),
            TokenType::While => "while".to_string(),
            TokenType::In => "in".to_string(),
            TokenType::Return => "return".to_string(),
            TokenType::Part => "part".to_string(),
            TokenType::Staff => "staff".to_string(),
            TokenType::Timeline => "timeline".to_string(),
            TokenType::And => "and".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Not => "not".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::False => "false".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Expects an identifier token and provides context-aware error messages.
    ///
    /// This method validates that the current token can be used as an identifier
    /// in the given context and provides helpful error messages when reserved
    /// words are used inappropriately.
    fn expect_identifier(&self, context: &str) -> Result<String> {
        let token = self.current_token();

        match self.categorize_token(&token.token_type) {
            TokenCategory::ValidIdentifier => {
                if let TokenType::Identifier(name) = &token.token_type {
                    Ok(name.clone())
                } else {
                    unreachable!("ValidIdentifier should only be returned for Identifier tokens")
                }
            }
            TokenCategory::Reserved(symbol_type) => {
                let symbol_value = self.extract_token_value(&token.token_type);
                Err(ErrorSource::Parsing(format!(
                    "Cannot use {} '{}' as {}. {} are reserved.",
                    symbol_type,
                    symbol_value,
                    context,
                    pluralize_symbol_type(symbol_type)
                ))
                .with_span(token.span))
            }
            TokenCategory::Invalid => Err(self.error(&format!("Expected {context}"))),
        }
    }

    fn error(&self, message: &str) -> CarmenError {
        ErrorSource::Parsing(message.to_string()).with_span(self.current_token().span)
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<()> {
        if self.check(&token_type) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(message))
        }
    }

    /// Parses an identifier with context-aware interpretation for musical notation.
    ///
    /// In musical contexts, some identifiers may be interpreted as musical elements
    /// (pitches, dynamics, attributes) rather than variable names. This method
    /// handles that context-sensitive parsing.
    fn parse_identifier_in_musical_context(&mut self, context: &str) -> Result<SpannedExpression> {
        let token = self.current_token().clone();

        if let TokenType::Identifier(name) = &token.token_type {
            // In musical context, single letters that could be dynamics should be treated as dynamics
            if context == "dynamic_or_attribute" && Literal::is_dynamic(name) {
                self.advance();
                return Ok(Spanned {
                    node: Expression::Literal(Literal::Dynamic(name.clone())),
                    span: token.span,
                });
            }

            // Check if it could be an attribute
            if context == "dynamic_or_attribute" && Literal::is_attribute(name) {
                self.advance();
                return Ok(Spanned {
                    node: Expression::Literal(Literal::Attribute(name.clone())),
                    span: token.span,
                });
            }

            // Check if it could be a pitch (for musical contexts)
            if context == "pitch" && self.could_be_pitch_identifier(name) {
                self.advance();
                if let Some(pitch) = Literal::parse_pitch(name) {
                    return Ok(Spanned {
                        node: Expression::Literal(pitch),
                        span: token.span,
                    });
                }
            }
        }

        // If we reach here and context is "dynamic_or_attribute", this identifier
        // is not a dynamic or attribute, so we should not consume it
        if context == "dynamic_or_attribute" {
            return Err(
                ErrorSource::Parsing("Expected dynamic or attribute".to_string())
                    .with_span(token.span),
            );
        }

        // Fall back to regular identifier
        let name = match &token.token_type {
            TokenType::Identifier(name) => name.clone(),
            _ => return Err(self.error(&format!("Expected identifier in {context} context"))),
        };

        self.advance();
        Ok(Spanned {
            node: Expression::Identifier(name),
            span: token.span,
        })
    }

    /// Determines if an identifier could represent a pitch in musical notation.
    ///
    /// Only considers it a potential pitch if it would actually parse as a valid
    /// pitch, preventing identifiers like "ad", "add", etc. from being misinterpreted.
    fn could_be_pitch_identifier(&self, name: &str) -> bool {
        Literal::parse_pitch(name).is_some()
    }

    /// Determines if the current token sequence could start a musical event.
    ///
    /// Musical events follow the pattern: duration + pitch(es) + optional modifiers.
    /// This method uses lookahead to distinguish musical events from other expressions.
    fn could_be_musical_event(&self) -> bool {
        match &self.current_token().token_type {
            TokenType::Duration(_) => {
                // Check if next token could be a pitch/chord/rest
                if self.current + 1 < self.tokens.len() {
                    match &self.tokens[self.current + 1].token_type {
                        TokenType::Pitch(_) |
                        TokenType::Identifier(_) | // identifier resolving to pitch
                        TokenType::LeftParen | // chord tuple
                        TokenType::LeftBracket | // list of pitches
                        TokenType::Tilde => true, // rest
                        _ => false,
                    }
                } else {
                    false
                }
            }
            TokenType::Number(n) if *n == 1.0 => {
                // Special case: "1" could be duration "1/1" if followed by musical content
                if self.current + 1 < self.tokens.len() {
                    match &self.tokens[self.current + 1].token_type {
                        TokenType::Pitch(_)
                        | TokenType::Tilde
                        | TokenType::LeftParen
                        | TokenType::LeftBracket => true,
                        TokenType::Identifier(name) => {
                            // Check if identifier could be a pitch
                            self.could_be_pitch_identifier(name)
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn grouping_or_tuple(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::LeftParen, "Expected '('")?;

        self.skip_comments();

        if self.check(&TokenType::RightParen) {
            self.advance();
            return Ok(Spanned {
                node: Expression::Tuple {
                    elements: Vec::new(),
                },
                span: Span {
                    start,
                    end: self.previous_token().span.end,
                },
            });
        }

        let mut elements = vec![self.expression()?];
        self.collect_leading_comments(&elements[0].span);

        if self.match_token(&TokenType::Comma) {
            self.collect_between_comments(0);
            // This is a tuple
            while !self.check(&TokenType::RightParen) && !self.is_at_end() {
                let element = self.expression()?;
                self.collect_trailing_comments(&element.span);
                elements.push(element);

                if !self.match_token(&TokenType::Comma) {
                    break;
                }

                self.collect_between_comments(elements.len() - 1);

                // Allow trailing comma
                if self.check(&TokenType::RightParen) {
                    break;
                }
            }

            self.skip_comments();
            self.consume(TokenType::RightParen, "Expected ')' after tuple elements")?;
            Ok(Spanned {
                node: Expression::Tuple { elements },
                span: Span {
                    start,
                    end: self.previous_token().span.end,
                },
            })
        } else {
            self.skip_comments();
            // This is a grouping expression
            self.consume(TokenType::RightParen, "Expected ')' after expression")?;
            Ok(elements.into_iter().next().unwrap())
        }
    }

    fn list(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::LeftBracket, "Expected '['")?;

        let mut elements = Vec::new();

        self.skip_comments(); // collect standalone comments after opening bracket

        if !self.check(&TokenType::RightBracket) {
            loop {
                let element = self.expression()?;
                self.collect_trailing_comments(&element.span);
                elements.push(element);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                // Collect comments between elements
                self.collect_between_comments(elements.len() - 1);

                // Allow trailing comma
                if self.check(&TokenType::RightBracket) {
                    break;
                }
            }
        }

        self.skip_comments(); // Collect comments before closing bracket
        self.consume(TokenType::RightBracket, "Expected ']' after list elements")?;

        Ok(Spanned {
            node: Expression::List { elements },
            span: Span {
                start,
                end: self.previous_token().span.end,
            },
        })
    }

    /// Checks if an expression represents a pitch class number (0-11).
    ///
    /// Used to determine if a set of numbers should be interpreted as a pitch class set.
    fn is_pitch_class_number(expr: &SpannedExpression) -> bool {
        match expr.node {
            Expression::Literal(Literal::Number(value)) => {
                (0.0..=11.0).contains(&value) && value.fract() == 0.0
            }
            _ => false,
        }
    }

    fn extract_pitch_class_number(expr: &SpannedExpression) -> Option<u8> {
        match expr.node {
            Expression::Literal(Literal::Number(value)) => {
                if (0.0..=11.0).contains(&value) && value.fract() == 0.0 {
                    Some(value as u8)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Checks if all elements in a collection are pitch class numbers.
    ///
    /// Used to determine if a set should be interpreted as a pitch class set.
    fn is_all_pitch_class_numbers(elements: &[SpannedExpression]) -> bool {
        !elements.is_empty() && elements.iter().all(Self::is_pitch_class_number)
    }

    /// Parses a brace-delimited construct that could be a set, pitch class set, or block.
    ///
    /// Uses lookahead to determine whether the content represents statements (block)
    /// or expressions (set). If all expressions are pitch class numbers (0-11),
    /// creates a specialized pitch class set.
    fn set_or_block(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::LeftBrace, "Expected '{'")?;

        self.skip_comments();

        if self.check(&TokenType::RightBrace) {
            self.advance();
            return Ok(Spanned {
                node: Expression::Set {
                    elements: Vec::new(),
                },
                span: Span {
                    start,
                    end: self.previous_token().span.end,
                },
            });
        }

        // Try to parse as statements (block)
        let checkpoint = self.current;

        // If we can parse a statement, it's a block
        if self.declaration().is_ok() {
            self.current = checkpoint; // Reset position
            let mut statements = Vec::new();

            while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                self.skip_comments(); // Skip leading comments
                if self.check(&TokenType::RightBrace) {
                    break;
                }

                let stmt = self.declaration()?;
                self.collect_trailing_comments(&stmt.span); // Collect trailing comments
                statements.push(stmt);
            }

            self.skip_comments(); // skip any remaining comment before closing brace
            self.consume(TokenType::RightBrace, "Expected '}' after block")?;

            Ok(Spanned {
                node: Expression::Block { statements },
                span: Span {
                    start,
                    end: self.previous_token().span.end,
                },
            })
        } else {
            // Parse as set
            self.current = checkpoint;
            let mut elements = Vec::new();

            self.skip_comments(); // Skip leading comments
            if !self.check(&TokenType::RightBrace) {
                loop {
                    let expr = self.expression()?;
                    self.collect_trailing_comments(&expr.span);
                    elements.push(expr);

                    if !self.match_token(&TokenType::Comma) {
                        break;
                    }

                    self.collect_between_comments(elements.len() - 1);

                    // Allow trailing comma
                    if self.check(&TokenType::RightBrace) {
                        break;
                    }
                }
            }

            self.skip_comments();
            self.consume(TokenType::RightBrace, "Expected '}' after set elements")?;
            if Self::is_all_pitch_class_numbers(&elements) {
                let classes: Vec<u8> = elements
                    .iter()
                    .filter_map(Self::extract_pitch_class_number)
                    .collect();

                Ok(Spanned {
                    node: Expression::PitchClassSet { classes },
                    span: Span {
                        start,
                        end: self.previous_token().span.end,
                    },
                })
            } else {
                Ok(Spanned {
                    node: Expression::Set { elements },
                    span: Span {
                        start,
                        end: self.previous_token().span.end,
                    },
                })
            }
        }
    }

    fn if_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::If, "Expected 'if'")?;

        self.consume(TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition")?;

        let then_branch = self.expression()?;

        let else_branch = if self.match_token(&TokenType::Else) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        let end = if let Some(ref else_expr) = else_branch {
            else_expr.span.end
        } else {
            then_branch.span.end
        };

        Ok(Spanned {
            node: Expression::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            },
            span: Span { start, end },
        })
    }

    fn for_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::For, "Expected 'for'")?;

        self.consume(TokenType::LeftParen, "Expected '(' after 'for'")?;

        let variable = self.expect_identifier("a loop variable")?;
        self.advance();

        self.consume(TokenType::In, "Expected 'in' after for variable")?;
        let iterable = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after for clause")?;

        let body = self.expression()?;

        let end = body.span.end;

        Ok(Spanned {
            node: Expression::For {
                variable,
                iterable: Box::new(iterable),
                body: Box::new(body),
            },
            span: Span { start, end },
        })
    }

    fn while_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::While, "Expected 'while'")?;

        self.consume(TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition")?;

        let body = self.expression()?;

        Ok(Spanned {
            node: Expression::While {
                condition: Box::new(condition),
                body: Box::new(body.clone()),
            },
            span: Span {
                start,
                end: body.span.end,
            },
        })
    }

    fn part_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Part, "Expected 'part'")?;

        let name = if matches!(self.current_token().token_type, TokenType::String(_)) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        let body = self.expression()?;

        Ok(Spanned {
            node: Expression::Part {
                name,
                body: Box::new(body.clone()),
            },
            span: Span {
                start,
                end: body.span.end,
            },
        })
    }

    fn staff_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Staff, "Expected 'staff'")?;

        let number = self.expression()?;
        let body = self.expression()?;

        Ok(Spanned {
            node: Expression::Staff {
                number: Box::new(number),
                body: Box::new(body.clone()),
            },
            span: Span {
                start,
                end: body.span.end,
            },
        })
    }

    fn timeline_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Timeline, "Expected 'timeline'")?;

        let body = self.expression()?;

        Ok(Spanned {
            node: Expression::Timeline {
                body: Box::new(body.clone()),
            },
            span: Span {
                start,
                end: body.span.end,
            },
        })
    }

    fn movement_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Movement, "Expected 'movement'")?;

        let name = if matches!(self.current_token().token_type, TokenType::String(_)) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        let body = self.expression()?;

        Ok(Spanned {
            node: Expression::Movement {
                name,
                body: Box::new(body.clone()),
            },
            span: Span {
                start,
                end: body.span.end,
            },
        })
    }

    fn score_expression(&mut self) -> Result<SpannedExpression> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Score, "Expected 'score'")?;

        let name = if matches!(self.current_token().token_type, TokenType::String(_)) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        let body = self.expression()?;

        let end = body.span.end;

        Ok(Spanned {
            node: Expression::Score {
                name,
                body: Box::new(body),
            },
            span: Span { start, end },
        })
    }

    /// Parses a musical event: duration + pitch(es) + optional dynamics/attributes.
    fn musical_event(&mut self) -> Result<SpannedExpression> {
        let start_pos = self.current_token().span.start;

        // Parse duration
        let duration = match &self.current_token().token_type {
            TokenType::Duration(d) => {
                let d = d.clone();
                let span = self.current_token().span;
                self.advance();
                if let Some(duration) = Literal::parse_duration(&d) {
                    Spanned {
                        node: Expression::Literal(duration),
                        span,
                    }
                } else {
                    return Err(
                        ErrorSource::Parsing(format!("Invalid duration: {d}")).with_span(span)
                    );
                }
            }
            TokenType::Number(n) if *n == 1.0 => {
                // Treat "1" as whole note duration in musical context
                let span = self.current_token().span;
                self.advance();

                Spanned {
                    node: Expression::Literal(Literal::Duration {
                        numerator: 1,
                        denominator: 1,
                        dots: 0,
                    }),
                    span,
                }
            }
            _ => return Err(self.error("Expected duration at start of musical event")),
        };

        let pitches = self.expression()?; // parse pitche(s)

        let mut dynamic = None;
        let mut attributes = Vec::new();

        while !self.check(&TokenType::Semicolon) && !self.is_at_end() {
            // Check what kind of token we have and handle accordingly
            match &self.current_token().token_type {
                TokenType::Dynamic(_) => {
                    if dynamic.is_some() {
                        return Err(self.error("Multiple dynamics not allowed"));
                    }
                    dynamic = Some(Box::new(self.expression()?));
                }
                TokenType::Attribute(_) => {
                    attributes.push(self.expression()?);
                }
                TokenType::Pitch(text) => {
                    let text = text.clone();

                    // In musical event context, prioritize dynamic/attribute interpretation
                    if Literal::is_dynamic(&text) {
                        if dynamic.is_some() {
                            return Err(self.error("Multiple dynamics not allowed"));
                        }
                        let span = self.current_token().span;
                        self.advance();
                        dynamic = Some(Box::new(Spanned {
                            node: Expression::Literal(Literal::Dynamic(text)),
                            span,
                        }));
                    } else if Literal::is_attribute(&text) {
                        let span = self.current_token().span;
                        self.advance();
                        attributes.push(Spanned {
                            node: Expression::Literal(Literal::Attribute(text)),
                            span,
                        });
                    } else {
                        break; // Not a dynamic or attribute, stop parsing musical modifiers
                    }
                }
                TokenType::Identifier(_) => {
                    // Use the existing method to handle context-aware parsing
                    match self.parse_identifier_in_musical_context("dynamic_or_attribute") {
                        Ok(expr) => {
                            match expr.node {
                                Expression::Literal(Literal::Dynamic { .. }) => {
                                    if dynamic.is_some() {
                                        return Err(self.error("Multiple dynamics not allowed"));
                                    }
                                    dynamic = Some(Box::new(expr));
                                }
                                Expression::Literal(Literal::Attribute { .. }) => {
                                    attributes.push(expr);
                                }
                                _ => unreachable!(), // parse_identifier_in_musical_context should only return dynamics/attributes in this context
                            }
                        }
                        Err(_) => {
                            // Not a dynamic or attribute, stop parsing musical modifiers
                            break;
                        }
                    }
                }
                _ => break,
            }
        }

        let end = if !attributes.is_empty() {
            attributes.last().unwrap().span.end
        } else if let Some(ref dyn_expr) = dynamic {
            dyn_expr.span.end
        } else {
            pitches.span.end
        };

        Ok(Spanned {
            node: Expression::MusicalEvent {
                duration: Box::new(duration),
                pitches: Box::new(pitches),
                dynamic,
                attributes,
            },
            span: Span {
                start: start_pos,
                end,
            },
        })
    }

    fn primary(&mut self) -> Result<SpannedExpression> {
        // check if musical event
        if self.could_be_musical_event() {
            return self.musical_event();
        }

        let token = self.current_token().clone();

        match &token.token_type {
            TokenType::True => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Boolean(true)),
                    span: token.span,
                })
            }
            TokenType::False => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Boolean(false)),
                    span: token.span,
                })
            }
            TokenType::Number(n) => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Number(*n)),
                    span: token.span,
                })
            }
            TokenType::String(s) => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::String(s.to_owned())),
                    span: token.span,
                })
            }
            TokenType::Pitch(p) => {
                self.advance();
                if let Some(pitch) = Literal::parse_pitch(p) {
                    Ok(Spanned {
                        node: Expression::Literal(pitch),
                        span: token.span,
                    })
                } else {
                    Err(ErrorSource::Parsing(format!("Invalid pitch: {p}")).with_span(token.span))
                }
            }
            TokenType::Duration(d) => {
                self.advance();
                if let Some(duration) = Literal::parse_duration(d) {
                    Ok(Spanned {
                        node: Expression::Literal(duration),
                        span: token.span,
                    })
                } else {
                    Err(ErrorSource::Parsing(format!("Invalid duration: {d}"))
                        .with_span(token.span))
                }
            }
            TokenType::Dynamic(d) => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Dynamic(d.to_owned())),
                    span: token.span,
                })
            }
            TokenType::Attribute(a) => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Attribute(a.to_owned())),
                    span: token.span,
                })
            }
            TokenType::Tilde => {
                self.advance();
                Ok(Spanned {
                    node: Expression::Literal(Literal::Rest { duration: None }),
                    span: token.span,
                })
            }
            TokenType::Identifier(name) => {
                if self.could_be_pitch_identifier(name) {
                    self.advance();
                    if let Some(pitch) = Literal::parse_pitch(name) {
                        Ok(Spanned {
                            node: Expression::Literal(pitch),
                            span: token.span,
                        })
                    } else {
                        Ok(Spanned {
                            node: Expression::Identifier(name.to_owned()),
                            span: token.span,
                        })
                    }
                } else {
                    self.advance();
                    Ok(Spanned {
                        node: Expression::Identifier(name.to_owned()),
                        span: token.span,
                    })
                }
            }
            TokenType::LeftParen => self.grouping_or_tuple(),
            TokenType::LeftBracket => self.list(),
            TokenType::LeftBrace => self.set_or_block(),
            TokenType::If => self.if_expression(),
            TokenType::For => self.for_expression(),
            TokenType::While => self.while_expression(),
            TokenType::Part => self.part_expression(),
            TokenType::Staff => self.staff_expression(),
            TokenType::Timeline => self.timeline_expression(),
            TokenType::Movement => self.movement_expression(),
            TokenType::Score => self.score_expression(),
            _ => Err(self.error("Unexpected token")),
        }
    }

    fn call(&mut self) -> Result<SpannedExpression> {
        let mut expr = self.primary()?;

        while self.match_token(&TokenType::LeftParen) {
            let (args, kwargs) = self.parse_arguments()?;
            self.consume(TokenType::RightParen, "Expected ')' after arguments")?;
            let end = self.previous_token().span.end;
            expr = Spanned {
                node: Expression::Call {
                    callee: Box::new(expr.clone()),
                    args,
                    kwargs,
                },
                span: Span {
                    start: expr.span.start,
                    end,
                },
            };
        }

        Ok(expr)
    }

    #[allow(clippy::type_complexity)]
    fn parse_arguments(
        &mut self,
    ) -> Result<(Vec<SpannedExpression>, Vec<(String, SpannedExpression)>)> {
        let mut args = Vec::new();
        let mut kwargs = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                // Check if this is a named argument (identifier = expression)
                if let TokenType::Identifier(name) = &self.current_token().token_type {
                    // Look ahead to see if next token is =
                    if self.current + 1 < self.tokens.len()
                        && matches!(self.tokens[self.current + 1].token_type, TokenType::Assign)
                    {
                        let name = name.clone();
                        self.advance(); // consume identifier
                        self.advance(); // consume =
                        let value = self.expression()?;
                        kwargs.push((name, value));
                    } else {
                        // Regular positional argument
                        args.push(self.expression()?);
                    }
                } else {
                    // Regular positional argument
                    args.push(self.expression()?);
                }

                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        Ok((args, kwargs))
    }

    fn match_unary_op(&mut self) -> Option<UnaryOp> {
        match self.current_token().token_type {
            TokenType::Minus => {
                self.advance();
                Some(UnaryOp::Minus)
            }
            TokenType::Not => {
                self.advance();
                Some(UnaryOp::Not)
            }
            _ => None,
        }
    }
    fn unary(&mut self) -> Result<SpannedExpression> {
        if let Some(op) = self.match_unary_op() {
            let start = self.previous_token().span.start;
            let operand = self.unary()?;
            let span = Span::new(start, operand.span.end);
            return Ok(Spanned {
                node: Expression::Unary {
                    operator: op,
                    operand: Box::new(operand),
                },
                span,
            });
        }
        self.call()
    }

    /// Parses left-associative binary operators at a given precedence level.
    ///
    /// This is a helper method for implementing precedence climbing in the
    /// expression parser. It handles operators like +, -, *, /, etc.
    fn parse_binary_left_associative<F>(
        &mut self,
        mut next_precedence: F,
        operators: &[TokenType],
    ) -> Result<SpannedExpression>
    where
        F: FnMut(&mut Self) -> Result<SpannedExpression>,
    {
        let mut expr = next_precedence(self)?;

        while operators.iter().any(|op| self.check(op)) {
            let op_token_type = &self.current_token().token_type;
            let binary_op = match op_token_type {
                TokenType::Multiply => BinaryOp::Multiply,
                TokenType::Divide => BinaryOp::Divide,
                TokenType::Modulo => BinaryOp::Modulo,
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Subtract,
                TokenType::Greater => BinaryOp::Greater,
                TokenType::GreaterEqual => BinaryOp::GreaterEqual,
                TokenType::Less => BinaryOp::Less,
                TokenType::LessEqual => BinaryOp::LessEqual,
                TokenType::Equal => BinaryOp::Equal,
                TokenType::NotEqual => BinaryOp::NotEqual,
                TokenType::And => BinaryOp::And,
                TokenType::Or => BinaryOp::Or,
                _ => unreachable!(),
            };

            self.advance();
            let right = next_precedence(self)?;
            let span = Span::new(expr.span.start, right.span.end);

            expr = Spanned {
                node: Expression::Binary {
                    left: Box::new(expr),
                    operator: binary_op,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(
            Self::unary,
            &[TokenType::Multiply, TokenType::Divide, TokenType::Modulo],
        )
    }

    fn term(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(Self::factor, &[TokenType::Plus, TokenType::Minus])
    }

    fn comparison(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(
            Self::term,
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
        )
    }

    fn equality(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(
            Self::comparison,
            &[TokenType::Equal, TokenType::NotEqual],
        )
    }

    fn logical_and(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(Self::equality, &[TokenType::And])
    }

    fn logical_or(&mut self) -> Result<SpannedExpression> {
        self.parse_binary_left_associative(Self::logical_and, &[TokenType::Or])
    }

    fn pipe(&mut self) -> Result<SpannedExpression> {
        let mut expr = self.logical_or()?;

        while self.match_token(&TokenType::Pipe) {
            let right = self.logical_or()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Spanned {
                node: Expression::Pipe {
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<SpannedExpression> {
        self.pipe()
    }

    fn variable_declaration(&mut self) -> Result<SpannedStatement> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Let, "Expected 'let'")?;

        let name = self.expect_identifier("a variable name")?;
        self.advance();

        self.consume(TokenType::Assign, "Expected '=' after variable name")?;
        let value = self.expression()?;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        )?;

        Ok(Spanned {
            node: Statement::VariableDeclaration { name, value },
            span: Span {
                start,
                end: self.previous_token().span.end,
            },
        })
    }

    fn function_declaration(&mut self) -> Result<SpannedStatement> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Def, "Expected 'def'")?;

        let name = self.expect_identifier("a function name")?;
        self.advance();

        self.consume(TokenType::LeftParen, "Expected '(' after function name")?;

        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                let param = self.expect_identifier("a parameter name")?;
                params.push(param);
                self.advance();

                if !self.match_token(&TokenType::Comma) {
                    break;
                }

                if self.check(&TokenType::RightParen) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after parameters")?;
        self.consume(TokenType::LeftBrace, "Expected '{' before function body")?;

        let mut body = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            self.skip_comments();
            if self.check(&TokenType::RightBrace) {
                break;
            }
            let decl = self.declaration()?;
            self.collect_trailing_comments(&decl.span);
            body.push(decl);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after function body")?;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after function declaration",
        )?;

        Ok(Spanned {
            node: Statement::FunctionDeclaration { name, params, body },
            span: Span {
                start,
                end: self.previous_token().span.end,
            },
        })
    }

    /// Parses metadata declarations
    ///
    /// Metadata provides information about the musical composition that affects
    /// interpretation but isn't part of the musical content itself.
    fn metadata_declaration(&mut self) -> Result<SpannedStatement> {
        let start = self.current_token().span.start;
        self.consume(TokenType::At, "Expected '@'")?;

        let key = match &self.current_token().token_type {
            TokenType::Identifier(key) => {
                let key = key.clone();
                self.advance();
                key
            }
            _ => return Err(self.error("Expected metadata key")),
        };
        // For metadata that commonly takes multiple values, parse them as a tuple
        let value = match key.as_str() {
            "time_signature" => {
                let expr = self.expression()?;
                if let Expression::Binary {
                    left,
                    operator: BinaryOp::Divide,
                    right,
                } = expr.node
                {
                    Spanned {
                        node: Expression::Tuple {
                            elements: vec![*left, *right],
                        },
                        span: expr.span,
                    }
                } else if let Expression::Literal(Literal::Duration {
                    numerator,
                    denominator,
                    dots: 0,
                }) = expr.node
                {
                    let num_expr = Spanned {
                        node: Expression::Literal(Literal::Number(f64::from(numerator))),
                        span: expr.span,
                    };
                    let den_expr = Spanned {
                        node: Expression::Literal(Literal::Number(f64::from(denominator))),
                        span: expr.span,
                    };
                    Spanned {
                        node: Expression::Tuple {
                            elements: vec![num_expr, den_expr],
                        },
                        span: expr.span,
                    }
                } else {
                    return Err(
                        self.error("Time signature must be in numerator/denominator format.")
                    );
                }
            }
            _ => self.expression()?,
        };
        self.consume(TokenType::Semicolon, "Expected ';' after metadata")?;

        Ok(Spanned {
            node: Statement::Metadata { key, value },
            span: Span {
                start,
                end: self.previous_token().span.end,
            },
        })
    }
    fn return_statement(&mut self) -> Result<SpannedStatement> {
        let start = self.current_token().span.start;
        self.consume(TokenType::Return, "Expected 'return'")?;

        let value = if self.check(&TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return statement")?;

        Ok(Spanned {
            node: Statement::Return { value },
            span: Span {
                start,
                end: self.previous_token().span.end,
            },
        })
    }

    fn statement(&mut self) -> Result<SpannedStatement> {
        match &self.current_token().token_type {
            TokenType::Return => self.return_statement(),
            _ => {
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Spanned {
                    node: Statement::Expression(expr.clone()),
                    span: expr.span,
                })
            }
        }
    }

    fn declaration(&mut self) -> Result<SpannedStatement> {
        match &self.current_token().token_type {
            TokenType::Let => self.variable_declaration(),
            TokenType::Def => self.function_declaration(),
            TokenType::At => self.metadata_declaration(),
            _ => self.statement(),
        }
    }

    /// Parses the entire token stream into a Carmen program.
    ///
    /// This is the main entry point for parsing. It consumes all tokens
    /// and produces a complete AST representing the Carmen program.
    ///
    /// # Returns
    /// A `Program` containing all parsed statements and collected comments.
    pub fn parse(mut self) -> Result<Program> {
        let start_span = self.current_token().span;
        let mut statements = Vec::new();

        while !self.is_at_end() {
            self.skip_comments();
            if self.is_at_end() {
                break;
            }

            let stmt = self.declaration()?;
            self.collect_trailing_comments(&stmt.span);
            statements.push(stmt);

            self.skip_comments();
        }

        let end_span = if statements.is_empty() {
            start_span
        } else {
            statements.last().unwrap().span
        };

        Ok(Program {
            statements,
            comments: self.comments.clone(),
            span: Span::new(start_span.start, end_span.end),
        })
    }
}

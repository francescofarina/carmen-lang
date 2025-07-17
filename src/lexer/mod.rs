//! Lexical analysis.
//!
//! This module provides tokenization functionality for Carmen.
//! The lexer converts source text into a stream oftokens that can be consumed by the parser.
//!
//! # Musical Concepts
//!
//! Carmen supports several musical constructs as first-class tokens:
//! - **Pitches**: Note names with optional accidentals and octave numbers (e.g., `C4`, `F#`, `Bb2`)
//! - **Durations**: Fractional note durations (e.g., `1/4`, `1/8.`, `3/16`)
//! - **Dynamics**: Musical volume markings (e.g., `pp`, `mf`, `ff`)
//! - **Attributes**: Musical articulations (e.g., `staccato`, `legato`)

use crate::errors::{AddSpan, ErrorSource, Position, Result, Span};

/// Represents the different types of tokens that can be produced by the lexer.
///
/// Carmen tokens fall into several categories:
/// - **Literals**: Numbers, strings, and musical constructs
/// - **Keywords**: Language keywords for control flow and musical structures
/// - **Operators**: Arithmetic, comparison, and logical operators
/// - **Delimiters**: Punctuation for grouping and separation
/// - **Special**: Domain-specific symbols and comments
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    Number(f64),
    Pitch(String),
    Duration(String),
    Dynamic(String),
    Attribute(String),
    String(String),
    Identifier(String),

    // Keywords
    Let,
    Def,
    If,
    Else,
    For,
    While,
    In,
    Return,
    Part,
    Staff,
    Timeline,
    Movement,
    Score,
    And,
    Or,
    Not,
    True,
    False,

    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Pipe,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Dot,
    Colon,

    // Special
    At,
    Tilde,
    Comment(String),

    // EOF
    Eof,
}

/// A token with its type and source location information.
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Self {
        Self { token_type, span }
    }
}

/// The main lexer implementation.
///
/// The lexer performs tokenization by scanning through the input character by character,
/// recognizing patterns that correspond to different token types. It maintains position
/// information for error reporting and debugging.
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_position: Position,
}

impl Lexer {
    /// Creates a new lexer for the given input string.
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            current_position: Position::start(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.position];
        self.position += 1;

        if c == '\n' {
            self.current_position.line += 1;
            self.current_position.column = 1;
        } else {
            self.current_position.column += 1;
        }
        self.current_position.offset += 1;

        c
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn make_token(&self, token_type: TokenType, start: Position) -> Token {
        Token::new(token_type, Span::new(start, self.current_position))
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.input[self.position] != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    /// Parses a string literal, handling escape sequences.
    fn string_literal(&mut self, start: Position) -> Result<Token> {
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let c = self.advance();
            if c == '\\' && self.is_at_end() {
                match self.advance() {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    c => {
                        value.push('\\');
                        value.push(c);
                    }
                }
            } else {
                value.push(c);
            }
        }

        if self.is_at_end() {
            return Err(
                ErrorSource::Lexical("Unterminated string literal".to_string())
                    .with_span(Span::new(start, self.current_position)),
            );
        }

        self.advance();
        Ok(self.make_token(TokenType::String(value), start))
    }

    fn previous(&self) -> char {
        self.input[self.position - 1]
    }

    /// Validates whether a string represents a valid fractional duration.
    ///
    /// Accepts fractions in the form `numerator/denominator` with optional dots
    /// for dotted rhythms. The denominator must be positive and within reasonable
    /// limits for musical notation (≤128), and the numerator must be positive
    /// and not excessively large relative to the denominator.
    fn is_valid_fractional_duration(&self, duration_str: &str) -> bool {
        let tuplet_max_denominator = 128;
        let tuplet_max_numerator_multiplier = 4;

        // Remove dots to check the base fraction
        let base_str = duration_str.trim_end_matches('.');

        if let Some(slash_pos) = base_str.find('/') {
            let numerator_str = &base_str[..slash_pos];
            let denominator_str = &base_str[slash_pos + 1..];

            if let (Ok(num), Ok(den)) =
                (numerator_str.parse::<u32>(), denominator_str.parse::<u32>())
            {
                // Accept any positive numerator and denominator within reasonable limits
                den > 0
                    && den <= tuplet_max_denominator
                    && num > 0
                    && num <= den * tuplet_max_numerator_multiplier // Allow some flexibility
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Attempts to parse a fractional duration starting from a digit.
    ///
    /// This method is called when a digit is followed by a '/' character.
    /// It tries to parse the complete fractional duration (e.g., "1/4", "3/8.")
    /// and backtracks if the result is not a valid musical duration.
    ///
    /// Returns `Some(Token)` if a valid duration is parsed, `None` otherwise.
    fn try_fractional_duration(&mut self, start: Position) -> Option<Token> {
        // save current position to potentially backtrack
        let saved_pos = self.position;
        let saved_current_pos = self.current_position;

        let first_num = if saved_pos > 0 {
            self.input[saved_pos - 1] // first number already consumed
        } else {
            return None;
        };

        if !first_num.is_ascii_digit() {
            return None;
        }

        if self.peek() != '/' {
            return None;
        }

        self.advance();

        // Check if what follows is a number
        if !self.peek().is_ascii_digit() {
            // Backtrack
            self.position = saved_pos;
            self.current_position = saved_current_pos;
            return None;
        }

        // Parse the denominator
        let mut denominator = String::new();
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            denominator.push(self.advance());
        }

        // Parse optional dots
        let mut dots = 0;
        while !self.is_at_end() && self.peek() == '.' {
            dots += 1;
            self.advance();
        }

        // Construct the duration string
        let mut duration_str = format!("{first_num}/{denominator}");
        for _ in 0..dots {
            duration_str.push('.');
        }

        // Validate it's a sensible duration
        if self.is_valid_fractional_duration(&duration_str) {
            Some(self.make_token(TokenType::Duration(duration_str), start))
        } else {
            // Backtrack
            self.position = saved_pos;
            self.current_position = saved_current_pos;
            None
        }
    }

    /// Parses a numeric literal, which can be either a regular number or a fractional duration.
    ///
    /// First attempts to parse as a fractional duration if the pattern matches.
    /// If that fails, falls back to parsing as a regular floating-point number.
    fn number_literal(&mut self, start: Position) -> Result<Token> {
        let mut number = String::new();
        number.push(self.previous());

        // First, try to parse as fractional duration if it looks like one
        if self.peek() == '/' {
            if let Some(duration_token) = self.try_fractional_duration(start) {
                return Ok(duration_token);
            }
        }

        // Otherwise, parse as regular number
        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '.') {
            number.push(self.advance());
        }

        match number.parse::<f64>() {
            Ok(value) => Ok(self.make_token(TokenType::Number(value), start)),
            Err(_) => Err(ErrorSource::Lexical(format!("Invalid number: {number}"))
                .with_span(Span::new(start, self.current_position))),
        }
    }

    /// Parses an identifier, keyword, or musical construct.
    ///
    /// Determines the token type based on the text content:
    /// 1. First checks if it's a language keyword
    /// 2. Then checks if it's a musical construct (pitch, duration, dynamic, attribute)
    /// 3. Otherwise treats it as a regular identifier
    fn identifier_or_keyword(&mut self, start: Position) -> Result<Token> {
        let mut text = String::new();
        text.push(self.previous());

        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == '_') {
            text.push(self.advance());
        }

        let token_type = match text.as_str() {
            "let" => TokenType::Let,
            "def" => TokenType::Def,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "in" => TokenType::In,
            "return" => TokenType::Return,
            "part" => TokenType::Part,
            "staff" => TokenType::Staff,
            "timeline" => TokenType::Timeline,
            "movement" => TokenType::Movement,
            "score" => TokenType::Score,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "not" => TokenType::Not,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => {
                // Check if it's a musical construct
                if self.is_pitch(&text) {
                    TokenType::Pitch(text)
                } else if self.is_duration(&text) {
                    TokenType::Duration(text)
                } else if self.is_dynamic(&text) {
                    TokenType::Dynamic(text)
                } else if self.is_attribute(&text) {
                    TokenType::Attribute(text)
                } else {
                    TokenType::Identifier(text)
                }
            }
        };

        Ok(self.make_token(token_type, start))
    }

    /// Determines if a string represents a valid pitch notation.
    ///
    /// Accepts pitch names in the format: `[a-g][accidentals][octave]`
    /// - Note name: a, b, c, d, e, f, or g (case insensitive)
    /// - Accidentals: Any combination of 's', '#' (sharp) or 'b' (flat)
    /// - Octave: Optional digit(s) for octave number
    ///
    /// Examples: `c`, `F#`, `bb4`, `Css2`
    fn is_pitch(&self, text: &str) -> bool {
        if text.is_empty() {
            return false;
        }

        let chars: Vec<char> = text.chars().collect();
        let first = chars[0].to_ascii_lowercase();

        if !matches!(first, 'a'..='g') {
            return false;
        }

        let mut i = 1;

        // Check for accidentals
        while i < chars.len() && (chars[i] == 's' || chars[i] == '#' || chars[i] == 'b') {
            i += 1;
        }

        // Check for octave number
        if i < chars.len() {
            chars[i..].iter().all(|c| c.is_ascii_digit())
        } else {
            true // Pitch class without octave
        }
    }

    /// Determines if a string represents a valid duration notation.
    ///
    /// Currently supports fractional durations and durations starting with "1/".
    fn is_duration(&self, text: &str) -> bool {
        if text.contains('/') {
            return self.is_valid_fractional_duration(text);
        }

        text.starts_with("1/")
    }

    /// Determines if a string represents a valid dynamic marking.
    ///
    /// Supports standard musical dynamics from pianissimo to fortissimo:
    /// `pppp`, `ppp`, `pp`, `p`, `mp`, `mf`, `f`, `ff`, `fff`, `ffff`
    fn is_dynamic(&self, text: &str) -> bool {
        matches!(
            text,
            "pppp" | "ppp" | "pp" | "p" | "mp" | "mf" | "f" | "ff" | "fff" | "ffff"
        )
    }

    /// Determines if a string represents a valid musical attribute/articulation.
    ///
    /// Currently supports: `staccato`, `legato`, `accent`, `tenuto`
    fn is_attribute(&self, text: &str) -> bool {
        matches!(text, "staccato" | "legato" | "accent" | "tenuto")
    }

    fn next_token(&mut self) -> Result<Token> {
        let start = self.current_position;

        match self.advance() {
            '(' => Ok(self.make_token(TokenType::LeftParen, start)),
            ')' => Ok(self.make_token(TokenType::RightParen, start)),
            '{' => Ok(self.make_token(TokenType::LeftBrace, start)),
            '}' => Ok(self.make_token(TokenType::RightBrace, start)),
            '[' => Ok(self.make_token(TokenType::LeftBracket, start)),
            ']' => Ok(self.make_token(TokenType::RightBracket, start)),
            ',' => Ok(self.make_token(TokenType::Comma, start)),
            ';' => Ok(self.make_token(TokenType::Semicolon, start)),
            '.' => Ok(self.make_token(TokenType::Dot, start)),
            ':' => Ok(self.make_token(TokenType::Colon, start)),
            '+' => Ok(self.make_token(TokenType::Plus, start)),
            '-' => Ok(self.make_token(TokenType::Minus, start)),
            '*' => Ok(self.make_token(TokenType::Multiply, start)),
            '%' => Ok(self.make_token(TokenType::Modulo, start)),
            '~' => Ok(self.make_token(TokenType::Tilde, start)),
            '@' => Ok(self.make_token(TokenType::At, start)),
            '/' => {
                if self.match_char('/') {
                    self.line_comment(start)
                } else {
                    Ok(self.make_token(TokenType::Divide, start))
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::Equal, start))
                } else {
                    Ok(self.make_token(TokenType::Assign, start))
                }
            }
            '!' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::NotEqual, start))
                } else {
                    Err(ErrorSource::Lexical("Unexpected character '!'".to_string())
                        .with_span(Span::single(start)))
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::LessEqual, start))
                } else {
                    Ok(self.make_token(TokenType::Less, start))
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::GreaterEqual, start))
                } else {
                    Ok(self.make_token(TokenType::Greater, start))
                }
            }
            '|' => {
                if self.match_char('>') {
                    Ok(self.make_token(TokenType::Pipe, start))
                } else {
                    Err(ErrorSource::Lexical("Expected '>' after '|'".to_string())
                        .with_span(Span::single(start)))
                }
            }
            '"' => self.string_literal(start),
            c if c.is_ascii_digit() => self.number_literal(start),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier_or_keyword(start),
            c => Err(ErrorSource::Lexical(format!("Unexpected character '{c}'"))
                .with_span(Span::single(start))),
        }
    }

    /// Parses a line comment starting with `//`.
    fn line_comment(&mut self, start: Position) -> Result<Token> {
        let mut comment = String::new();

        while !self.is_at_end() && self.peek() != '\n' {
            comment.push(self.advance());
        }

        Ok(self.make_token(TokenType::Comment(comment), start))
    }

    /// Tokenizes the entire input, returning a vector of tokens.
    ///
    /// Processes the input from start to finish, skipping whitespace and
    /// producing tokens for all recognized patterns. Always ends with an EOF token.
    ///
    /// # Errors
    ///
    /// Returns an error if any invalid syntax is encountered, such as:
    /// - Unterminated string literals
    /// - Invalid number formats
    /// - Unexpected characters
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        tokens.push(Token::new(
            TokenType::Eof,
            Span::single(self.current_position),
        ));

        Ok(tokens)
    }
}

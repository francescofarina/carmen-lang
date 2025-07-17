use carmen_lang::errors::Result;
use carmen_lang::lexer::{Lexer, Token, TokenType};

// Helper function to tokenize input and return all tokens (excluding EOF)
fn tokenize_all(input: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    // Return all tokens except the EOF token
    Ok(tokens[0..tokens.len() - 1].to_vec())
}

// Helper function to get first token from input
fn first_token(input: &str) -> Result<Token> {
    let tokens = tokenize_all(input)?;
    if tokens.is_empty() {
        panic!("No tokens found in input: {input}");
    }
    Ok(tokens[0].clone())
}

// Helper to assert token type matches expected
fn assert_token_type(token: &Token, expected: &TokenType) {
    assert_eq!(
        &token.token_type, expected,
        "Expected token type {:?} but got {:?}",
        expected, token.token_type
    );
}

// Helper to test a single token
fn test_single_token(input: &str, expected: TokenType) -> Result<()> {
    let token = first_token(input)?;
    assert_token_type(&token, &expected);
    Ok(())
}

// Helper to verify a sequence of token types
fn assert_token_sequence(tokens: &[Token], expected_types: &[TokenType]) {
    assert_eq!(
        tokens.len(),
        expected_types.len(),
        "Token count mismatch. Got {} tokens, expected {}",
        tokens.len(),
        expected_types.len()
    );

    for (i, (token, expected)) in tokens.iter().zip(expected_types.iter()).enumerate() {
        assert_eq!(
            &token.token_type, expected,
            "Token mismatch at position {}: expected {:?}, got {:?}",
            i, expected, token.token_type
        );
    }
}

#[cfg(test)]
mod literal_tests {
    use super::*;

    #[test]
    fn test_number_literals() -> Result<()> {
        let test_cases = [
            ("123", TokenType::Number(123.0)),
            ("0", TokenType::Number(0.0)),
            ("123.456", TokenType::Number(123.456)),
            ("0.123", TokenType::Number(0.123)),
            ("9999.9999", TokenType::Number(9999.9999)),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_pitch_literals() -> Result<()> {
        let test_cases = [
            ("c", TokenType::Pitch("c".to_string())),
            ("C4", TokenType::Pitch("C4".to_string())),
            ("Eb3", TokenType::Pitch("Eb3".to_string())),
            ("Fs5", TokenType::Pitch("Fs5".to_string())),
            ("Gss2", TokenType::Pitch("Gss2".to_string())), // Double sharp
            ("Abbbb7", TokenType::Pitch("Abbbb7".to_string())), // Multiple flats
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_duration_literals() -> Result<()> {
        let test_cases = [
            ("1/4", TokenType::Duration("1/4".to_string())),
            ("1/8.", TokenType::Duration("1/8.".to_string())),
            ("1/16..", TokenType::Duration("1/16..".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_dynamic_literals() -> Result<()> {
        let test_cases = [
            ("ppp", TokenType::Dynamic("ppp".to_string())),
            ("pp", TokenType::Dynamic("pp".to_string())),
            ("p", TokenType::Dynamic("p".to_string())),
            ("mp", TokenType::Dynamic("mp".to_string())),
            ("mf", TokenType::Dynamic("mf".to_string())),
            ("f", TokenType::Pitch("f".to_string())), // TODO: f is currently lexed as a pitch and then parsed/interpreted correctly based on context
            ("ff", TokenType::Dynamic("ff".to_string())),
            ("fff", TokenType::Dynamic("fff".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_attribute_literals() -> Result<()> {
        let test_cases = [
            ("staccato", TokenType::Attribute("staccato".to_string())),
            ("legato", TokenType::Attribute("legato".to_string())),
            ("accent", TokenType::Attribute("accent".to_string())),
            ("tenuto", TokenType::Attribute("tenuto".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_string_literals() -> Result<()> {
        let test_cases = [
            ("\"hello\"", TokenType::String("hello".to_string())),
            ("\"\"", TokenType::String("".to_string())),
            (
                "\"special\nchars\t\r\"",
                TokenType::String("special\nchars\t\r".to_string()),
            ),
            (
                "\"with\\backslash\"",
                TokenType::String("with\\backslash".to_string()),
            ),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_identifiers() -> Result<()> {
        let test_cases = [
            ("variable", TokenType::Identifier("variable".to_string())),
            (
                "_underscore",
                TokenType::Identifier("_underscore".to_string()),
            ),
            ("mixed_123", TokenType::Identifier("mixed_123".to_string())),
            ("x", TokenType::Identifier("x".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod keyword_tests {
    use super::*;

    #[test]
    fn test_keywords() -> Result<()> {
        let test_cases = [
            ("let", TokenType::Let),
            ("def", TokenType::Def),
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("for", TokenType::For),
            ("while", TokenType::While),
            ("in", TokenType::In),
            ("return", TokenType::Return),
            ("part", TokenType::Part),
            ("staff", TokenType::Staff),
            ("timeline", TokenType::Timeline),
            ("movement", TokenType::Movement),
            ("score", TokenType::Score),
            ("and", TokenType::And),
            ("or", TokenType::Or),
            ("not", TokenType::Not),
            ("true", TokenType::True),
            ("false", TokenType::False),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod operator_tests {
    use super::*;

    #[test]
    fn test_arithmetic_operators() -> Result<()> {
        let test_cases = [
            ("+", TokenType::Plus),
            ("-", TokenType::Minus),
            ("*", TokenType::Multiply),
            ("/", TokenType::Divide),
            ("%", TokenType::Modulo),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_comparison_operators() -> Result<()> {
        let test_cases = [
            ("==", TokenType::Equal),
            ("!=", TokenType::NotEqual),
            ("<", TokenType::Less),
            (">", TokenType::Greater),
            ("<=", TokenType::LessEqual),
            (">=", TokenType::GreaterEqual),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_assignment_operator() -> Result<()> {
        test_single_token("=", TokenType::Assign)
    }

    #[test]
    fn test_pipe_operator() -> Result<()> {
        test_single_token("|>", TokenType::Pipe)
    }
}

#[cfg(test)]
mod delimiter_tests {
    use super::*;

    #[test]
    fn test_delimiters() -> Result<()> {
        let test_cases = [
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            ("[", TokenType::LeftBracket),
            ("]", TokenType::RightBracket),
            (",", TokenType::Comma),
            (";", TokenType::Semicolon),
            (".", TokenType::Dot),
            (":", TokenType::Colon),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod special_token_tests {
    use super::*;

    #[test]
    fn test_special_tokens() -> Result<()> {
        test_single_token("@", TokenType::At)?;
        test_single_token("~", TokenType::Tilde)
    }
}

#[cfg(test)]
mod comment_tests {
    use super::*;

    #[test]
    fn test_line_comments() -> Result<()> {
        let test_cases = [
            (
                "// This is a comment",
                TokenType::Comment(" This is a comment".to_string()),
            ),
            ("//", TokenType::Comment("".to_string())),
            (
                "// Multiple // slashes",
                TokenType::Comment(" Multiple // slashes".to_string()),
            ),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_code_with_comments() -> Result<()> {
        let input = "let x = 5; // Initialize x";
        let tokens = tokenize_all(input)?;

        let expected = vec![
            TokenType::Let,
            TokenType::Identifier("x".to_string()),
            TokenType::Assign,
            TokenType::Number(5.0),
            TokenType::Semicolon,
            TokenType::Comment(" Initialize x".to_string()),
        ];

        assert_token_sequence(&tokens, &expected);
        Ok(())
    }
}

#[cfg(test)]
mod error_tests {
    use super::*;

    #[test]
    fn test_unterminated_string() {
        let result = tokenize_all("\"unterminated string");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.to_string().contains("Unterminated string literal"));
        }
    }

    #[test]
    fn test_invalid_number() {
        let result = tokenize_all("123.456.789");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.to_string().contains("Invalid number"));
        }
    }

    #[test]
    fn test_invalid_exclamation() {
        let result = tokenize_all("!");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.to_string().contains("Unexpected character '!'"));
        }
    }

    #[test]
    fn test_invalid_pipe() {
        let result = tokenize_all("|");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.to_string().contains("Expected '>' after '|'"));
        }
    }
}

#[cfg(test)]
mod whitespace_tests {
    use super::*;

    #[test]
    fn test_whitespace_handling() -> Result<()> {
        let input = "  \t\r\n  let   x   =   5   ;   ";
        let tokens = tokenize_all(input)?;

        let expected = vec![
            TokenType::Let,
            TokenType::Identifier("x".to_string()),
            TokenType::Assign,
            TokenType::Number(5.0),
            TokenType::Semicolon,
        ];

        assert_token_sequence(&tokens, &expected);
        Ok(())
    }

    #[test]
    fn test_newline_handling() -> Result<()> {
        let input = "let x = 5;\nlet y = 10;";
        let tokens = tokenize_all(input)?;

        let expected = vec![
            TokenType::Let,
            TokenType::Identifier("x".to_string()),
            TokenType::Assign,
            TokenType::Number(5.0),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Identifier("y".to_string()),
            TokenType::Assign,
            TokenType::Number(10.0),
            TokenType::Semicolon,
        ];

        assert_token_sequence(&tokens, &expected);
        Ok(())
    }
}

#[cfg(test)]
mod sequence_tests {
    use super::*;

    #[test]
    fn test_complex_expression() -> Result<()> {
        let input = "let x = (5 + 10) * 3 / 2;";
        let tokens = tokenize_all(input)?;

        let expected = vec![
            TokenType::Let,
            TokenType::Identifier("x".to_string()),
            TokenType::Assign,
            TokenType::LeftParen,
            TokenType::Number(5.0),
            TokenType::Plus,
            TokenType::Number(10.0),
            TokenType::RightParen,
            TokenType::Multiply,
            TokenType::Number(3.0),
            TokenType::Divide,
            TokenType::Number(2.0),
            TokenType::Semicolon,
        ];

        assert_token_sequence(&tokens, &expected);
        Ok(())
    }

    #[test]
    fn test_function_definition() -> Result<()> {
        let input = "def add(x, y) { return x + y; }";
        let tokens = tokenize_all(input)?;

        let expected = vec![
            TokenType::Def,
            TokenType::Identifier("add".to_string()),
            TokenType::LeftParen,
            TokenType::Identifier("x".to_string()),
            TokenType::Comma,
            TokenType::Identifier("y".to_string()),
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Return,
            TokenType::Identifier("x".to_string()),
            TokenType::Plus,
            TokenType::Identifier("y".to_string()),
            TokenType::Semicolon,
            TokenType::RightBrace,
        ];

        assert_token_sequence(&tokens, &expected);
        Ok(())
    }
}

#[cfg(test)]
mod fractional_duration_tests {
    use super::*;

    #[test]
    fn test_fractional_durations() -> Result<()> {
        let test_cases = [
            ("1/4", TokenType::Duration("1/4".to_string())),
            ("1/8", TokenType::Duration("1/8".to_string())),
            ("1/16", TokenType::Duration("1/16".to_string())),
            ("1/32", TokenType::Duration("1/32".to_string())),
            ("1/64", TokenType::Duration("1/64".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }

    #[test]
    fn test_dotted_fractional_durations() -> Result<()> {
        let test_cases = [
            ("1/4.", TokenType::Duration("1/4.".to_string())),
            ("1/8..", TokenType::Duration("1/8..".to_string())),
            ("1/16...", TokenType::Duration("1/16...".to_string())),
        ];

        for (input, expected) in test_cases {
            test_single_token(input, expected)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod span_tests {
    use super::*;

    #[test]
    fn test_token_spans() -> Result<()> {
        let input = "let x = 5;";
        let tokens = tokenize_all(input)?;

        // Check "let" token span
        assert_eq!(tokens[0].span.start.offset, 0);
        assert_eq!(tokens[0].span.end.offset, 3);
        assert_eq!(tokens[0].span.start.line, 1);
        assert_eq!(tokens[0].span.start.column, 1);

        // Check "x" token span
        assert_eq!(tokens[1].span.start.offset, 4);
        assert_eq!(tokens[1].span.end.offset, 5);
        assert_eq!(tokens[1].span.start.line, 1);
        assert_eq!(tokens[1].span.start.column, 5);

        // Check "=" token span
        assert_eq!(tokens[2].span.start.offset, 6);
        assert_eq!(tokens[2].span.end.offset, 7);

        // Check "5" token span
        assert_eq!(tokens[3].span.start.offset, 8);
        assert_eq!(tokens[3].span.end.offset, 9);

        // Check ";" token span
        assert_eq!(tokens[4].span.start.offset, 9);
        assert_eq!(tokens[4].span.end.offset, 10);

        Ok(())
    }
}

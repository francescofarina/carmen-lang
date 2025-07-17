use carmen_lang::errors::{reporter, CarmenError, ErrorSource, Position, Span};

fn setup(source: &str, error_source: ErrorSource, span: Span) -> (String, String, CarmenError) {
    let filename = "test.carmen".to_string();
    let error = CarmenError::new(error_source, span);
    (source.to_string(), filename, error)
}

#[test]
fn test_report_single_char_span() {
    let (source, filename, error) = setup(
        "let x = @;",
        ErrorSource::Lexical("Unknown character".to_string()),
        Span::new(Position::new(1, 9, 8), Position::new(1, 10, 9)),
    );
    let mut buffer = Vec::new();
    reporter::report(&mut buffer, &source, &error, &filename).unwrap();
    let output = String::from_utf8(buffer).unwrap();

    let expected = r#"error: Lexical error
  --> test.carmen:1:9
   |
 1 | let x = @;
   |         ^ Unknown character
"#;
    assert_eq!(output.trim(), expected.trim());
}

#[test]
fn test_report_multi_char_span() {
    let (source, filename, error) = setup(
        "let y = 1.2.3;",
        ErrorSource::Lexical("Invalid number format".to_string()),
        Span::new(Position::new(1, 9, 8), Position::new(1, 14, 13)),
    );
    let mut buffer = Vec::new();
    reporter::report(&mut buffer, &source, &error, &filename).unwrap();
    let output = String::from_utf8(buffer).unwrap();
    let expected = r#"error: Lexical error
  --> test.carmen:1:9
   |
 1 | let y = 1.2.3;
   |         ^~~~~ Invalid number format
"#;
    assert_eq!(output.trim(), expected.trim());
}

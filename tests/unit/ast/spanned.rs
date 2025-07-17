use carmen_lang::ast::spanned::Spanned;
use carmen_lang::ast::{Expression, Literal};
use carmen_lang::errors::{Position, Span};

#[cfg(test)]
mod spanned_tests {
    use super::*;

    fn create_test_span() -> Span {
        Span {
            start: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            end: Position {
                line: 1,
                column: 10,
                offset: 9,
            },
        }
    }

    fn create_different_span() -> Span {
        Span {
            start: Position {
                line: 2,
                column: 5,
                offset: 20,
            },
            end: Position {
                line: 2,
                column: 15,
                offset: 30,
            },
        }
    }

    #[test]
    fn test_spanned_creation() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(42.0));
        let spanned = Spanned {
            node: node.clone(),
            span,
        };

        assert_eq!(spanned.node, node);
        assert_eq!(spanned.span, span);
    }

    #[test]
    fn test_into_inner() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::String("hello".to_string()));
        let spanned = Spanned {
            node: node.clone(),
            span,
        };

        let extracted = spanned.into_inner();
        assert_eq!(extracted, node);
    }

    #[test]
    fn test_map() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(42.0));
        let spanned = Spanned { node, span };

        let mapped = spanned.map(|expr| match expr {
            Expression::Literal(Literal::Number(n)) => {
                Expression::Literal(Literal::Number(n * 2.0))
            }
            other => other,
        });

        assert_eq!(mapped.span, span);
        match mapped.node {
            Expression::Literal(Literal::Number(n)) => assert_eq!(n, 84.0),
            _ => panic!("Expected mapped number literal"),
        }
    }

    #[test]
    fn test_map_preserves_span() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Boolean(true));
        let spanned = Spanned { node, span };

        let mapped = spanned.map(|_| Expression::Identifier("new_value".to_string()));

        assert_eq!(mapped.span, span);
        match mapped.node {
            Expression::Identifier(name) => assert_eq!(name, "new_value"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_try_map_success() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(4.0));
        let spanned = Spanned { node, span };

        let result = spanned.try_map(|expr| -> Result<Expression, String> {
            match expr {
                Expression::Literal(Literal::Number(n)) if n > 0.0 => {
                    Ok(Expression::Literal(Literal::Number(n.sqrt())))
                }
                _ => Err("Invalid number".to_string()),
            }
        });

        assert!(result.is_ok());
        let mapped = result.unwrap();
        assert_eq!(mapped.span, span);
        match mapped.node {
            Expression::Literal(Literal::Number(n)) => {
                assert!((n - 2.0).abs() < f64::EPSILON);
            }
            _ => panic!("Expected number literal"),
        }
    }

    #[test]
    fn test_try_map_failure() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(-42.0));
        let spanned = Spanned { node, span };

        let result = spanned.try_map(|expr| -> Result<Expression, String> {
            match expr {
                Expression::Literal(Literal::Number(n)) if n > 0.0 => {
                    Ok(Expression::Literal(Literal::Number(n.sqrt())))
                }
                _ => Err("Invalid number".to_string()),
            }
        });

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Invalid number");
    }

    #[test]
    fn test_try_map_preserves_span_on_success() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::String("test".to_string()));
        let spanned = Spanned { node, span };

        let result = spanned.try_map(|expr| -> Result<Expression, ()> {
            match expr {
                Expression::Literal(Literal::String(s)) => {
                    Ok(Expression::Literal(Literal::String(s.to_uppercase())))
                }
                _ => Err(()),
            }
        });

        assert!(result.is_ok());
        let mapped = result.unwrap();
        assert_eq!(mapped.span, span);
        match mapped.node {
            Expression::Literal(Literal::String(s)) => assert_eq!(s, "TEST"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_display_forwards_to_inner() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(std::f64::consts::PI));
        let spanned = Spanned { node, span };

        let display_output = format!("{spanned}");
        assert_eq!(display_output, std::f64::consts::PI.to_string());
    }

    #[test]
    fn test_display_with_string_literal() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::String("hello world".to_string()));
        let spanned = Spanned { node, span };

        let display_output = format!("{spanned}");
        assert_eq!(display_output, "\"hello world\"");
    }

    #[test]
    fn test_display_with_boolean_literal() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Boolean(false));
        let spanned = Spanned { node, span };

        let display_output = format!("{spanned}");
        assert_eq!(display_output, "false");
    }

    #[test]
    fn test_display_with_identifier() {
        let span = create_test_span();
        let node = Expression::Identifier("my_variable".to_string());
        let spanned = Spanned { node, span };

        let display_output = format!("{spanned}");
        assert_eq!(display_output, "my_variable");
    }

    #[test]
    fn test_clone() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(42.0));
        let spanned = Spanned {
            node: node.clone(),
            span,
        };

        let cloned = spanned.clone();
        assert_eq!(cloned.node, node);
        assert_eq!(cloned.span, span);
        assert_eq!(spanned, cloned);
    }

    #[test]
    fn test_equality() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::String("test".to_string()));

        let spanned1 = Spanned {
            node: node.clone(),
            span,
        };
        let spanned2 = Spanned {
            node: node.clone(),
            span,
        };

        assert_eq!(spanned1, spanned2);
    }

    #[test]
    fn test_inequality_different_nodes() {
        let span = create_test_span();
        let node1 = Expression::Literal(Literal::Number(42.0));
        let node2 = Expression::Literal(Literal::Number(43.0));

        let spanned1 = Spanned { node: node1, span };
        let spanned2 = Spanned { node: node2, span };

        assert_ne!(spanned1, spanned2);
    }

    #[test]
    fn test_inequality_different_spans() {
        let span1 = create_test_span();
        let span2 = create_different_span();
        let node = Expression::Literal(Literal::Number(42.0));

        let spanned1 = Spanned {
            node: node.clone(),
            span: span1,
        };
        let spanned2 = Spanned {
            node: node.clone(),
            span: span2,
        };

        assert_ne!(spanned1, spanned2);
    }

    #[test]
    fn test_debug_formatting() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Boolean(true));
        let spanned = Spanned { node, span };

        let debug_output = format!("{spanned:?}");
        assert!(debug_output.contains("Spanned"));
        assert!(debug_output.contains("Boolean(true)"));
        assert!(debug_output.contains("line: 1"));
        assert!(debug_output.contains("column: 1"));
    }

    #[test]
    fn test_chained_transformations() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Number(16.0));
        let spanned = Spanned { node, span };

        // Chain multiple transformations
        let result = spanned
            .map(|expr| match expr {
                Expression::Literal(Literal::Number(n)) => {
                    Expression::Literal(Literal::Number(n / 2.0))
                }
                other => other,
            })
            .map(|expr| match expr {
                Expression::Literal(Literal::Number(n)) => {
                    Expression::Literal(Literal::Number(n * 3.0))
                }
                other => other,
            });

        assert_eq!(result.span, span);
        match result.node {
            Expression::Literal(Literal::Number(n)) => assert_eq!(n, 24.0), // (16 / 2) * 3
            _ => panic!("Expected number literal"),
        }
    }

    #[test]
    fn test_complex_expression_mapping() {
        let span = create_test_span();
        let node = Expression::Literal(Literal::Pitch {
            note: "c".to_string(),
            accidentals: 0,
            octave: Some(4),
        });
        let spanned = Spanned { node, span };

        let mapped = spanned.map(|expr| match expr {
            Expression::Literal(Literal::Pitch {
                note,
                accidentals,
                octave,
            }) => {
                Expression::Literal(Literal::Pitch {
                    note: note.to_uppercase(),
                    accidentals: accidentals + 1, // Add a sharp
                    octave,
                })
            }
            other => other,
        });

        assert_eq!(mapped.span, span);
        match mapped.node {
            Expression::Literal(Literal::Pitch {
                note,
                accidentals,
                octave,
            }) => {
                assert_eq!(note, "C");
                assert_eq!(accidentals, 1);
                assert_eq!(octave, Some(4));
            }
            _ => panic!("Expected pitch literal"),
        }
    }
}

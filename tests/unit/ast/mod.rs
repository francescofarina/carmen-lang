pub mod inspector;
pub mod spanned;

use carmen_lang::ast::*;
use carmen_lang::errors::{Position, Span};

#[cfg(test)]
mod literal_tests {
    use super::*;

    #[test]
    fn test_parse_pitch_basic() {
        let pitch = Literal::parse_pitch("c").unwrap();
        assert_eq!(
            pitch,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 0,
                octave: None,
            }
        );
    }

    #[test]
    fn test_parse_pitch_with_octave() {
        let pitch = Literal::parse_pitch("c4").unwrap();
        assert_eq!(
            pitch,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 0,
                octave: Some(4),
            }
        );
    }

    #[test]
    fn test_parse_pitch_with_sharps() {
        let pitch = Literal::parse_pitch("f#4").unwrap();
        assert_eq!(
            pitch,
            Literal::Pitch {
                note: "f".to_string(),
                accidentals: 1,
                octave: Some(4),
            }
        );

        let pitch_double_sharp = Literal::parse_pitch("c##").unwrap();
        assert_eq!(
            pitch_double_sharp,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 2,
                octave: None,
            }
        );
    }

    #[test]
    fn test_parse_pitch_with_flats() {
        let pitch = Literal::parse_pitch("bb3").unwrap();
        assert_eq!(
            pitch,
            Literal::Pitch {
                note: "b".to_string(),
                accidentals: -1,
                octave: Some(3),
            }
        );

        let pitch_double_flat = Literal::parse_pitch("abb").unwrap();
        assert_eq!(
            pitch_double_flat,
            Literal::Pitch {
                note: "a".to_string(),
                accidentals: -2,
                octave: None,
            }
        );
    }

    #[test]
    fn test_parse_pitch_mixed_accidentals() {
        let pitch = Literal::parse_pitch("cs5").unwrap();
        assert_eq!(
            pitch,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 1,
                octave: Some(5),
            }
        );
    }

    #[test]
    fn test_parse_pitch_invalid() {
        assert!(Literal::parse_pitch("").is_none());
        assert!(Literal::parse_pitch("h4").is_none());
        assert!(Literal::parse_pitch("c4x").is_none());
        assert!(Literal::parse_pitch("c4#").is_none());
        assert!(Literal::parse_pitch("4c").is_none());
    }

    #[test]
    fn test_is_dynamic() {
        assert!(Literal::is_dynamic("pp"));
        assert!(Literal::is_dynamic("f"));
        assert!(Literal::is_dynamic("ff"));
        assert!(Literal::is_dynamic("pppp"));
        assert!(Literal::is_dynamic("ffff"));
        assert!(Literal::is_dynamic("mp"));
        assert!(Literal::is_dynamic("mf"));

        assert!(!Literal::is_dynamic("loud"));
        assert!(!Literal::is_dynamic("fffff"));
        assert!(!Literal::is_dynamic("ppppp"));
        assert!(!Literal::is_dynamic(""));
    }

    #[test]
    fn test_is_attribute() {
        assert!(Literal::is_attribute("staccato"));
        assert!(Literal::is_attribute("legato"));
        assert!(Literal::is_attribute("accent"));
        assert!(Literal::is_attribute("tenuto"));
        assert!(Literal::is_attribute("marcato"));
        assert!(Literal::is_attribute("sforzando"));

        assert!(!Literal::is_attribute("loud"));
        assert!(!Literal::is_attribute("fast"));
        assert!(!Literal::is_attribute(""));
    }

    #[test]
    fn test_parse_duration_basic() {
        let duration = Literal::parse_duration("1/4").unwrap();
        assert_eq!(
            duration,
            Literal::Duration {
                numerator: 1,
                denominator: 4,
                dots: 0,
            }
        );
    }

    #[test]
    fn test_parse_duration_with_dots() {
        let duration = Literal::parse_duration("1/2.").unwrap();
        assert_eq!(
            duration,
            Literal::Duration {
                numerator: 1,
                denominator: 2,
                dots: 1,
            }
        );

        let duration_double_dot = Literal::parse_duration("1/4..").unwrap();
        assert_eq!(
            duration_double_dot,
            Literal::Duration {
                numerator: 1,
                denominator: 4,
                dots: 2,
            }
        );
    }

    #[test]
    fn test_parse_duration_whole_numbers() {
        let duration = Literal::parse_duration("2").unwrap();
        assert_eq!(
            duration,
            Literal::Duration {
                numerator: 2,
                denominator: 1,
                dots: 0,
            }
        );

        let duration_with_dots = Literal::parse_duration("1.").unwrap();
        assert_eq!(
            duration_with_dots,
            Literal::Duration {
                numerator: 1,
                denominator: 1,
                dots: 1,
            }
        );
    }

    #[test]
    fn test_parse_duration_invalid() {
        assert!(Literal::parse_duration("").is_none());
        assert!(Literal::parse_duration("1/0").is_none());
        assert!(Literal::parse_duration("a/4").is_none());
        assert!(Literal::parse_duration("1/b").is_none());
        assert!(Literal::parse_duration("1/").is_none());
        assert!(Literal::parse_duration("/4").is_none());
    }

    #[test]
    fn test_parse_rest_basic() {
        let rest = Literal::parse_rest("~").unwrap();
        assert_eq!(rest, Literal::Rest { duration: None });
    }

    #[test]
    fn test_parse_rest_with_duration() {
        let rest = Literal::parse_rest("~1/4").unwrap();
        assert_eq!(
            rest,
            Literal::Duration {
                numerator: 1,
                denominator: 4,
                dots: 0,
            }
        );

        let rest_with_dots = Literal::parse_rest("~1/2.").unwrap();
        assert_eq!(
            rest_with_dots,
            Literal::Duration {
                numerator: 1,
                denominator: 2,
                dots: 1,
            }
        );
    }

    #[test]
    fn test_parse_rest_invalid() {
        assert!(Literal::parse_rest("").is_none());
        assert!(Literal::parse_rest("r").is_none());
        assert!(Literal::parse_rest("~1/0").is_none());
    }
}

#[cfg(test)]
mod binary_op_tests {
    use super::*;

    #[test]
    fn test_binary_op_variants() {
        // Just test that all variants exist and are different
        let ops = [
            BinaryOp::Add,
            BinaryOp::Subtract,
            BinaryOp::Multiply,
            BinaryOp::Divide,
            BinaryOp::Modulo,
            BinaryOp::Equal,
            BinaryOp::NotEqual,
            BinaryOp::Less,
            BinaryOp::Greater,
            BinaryOp::LessEqual,
            BinaryOp::GreaterEqual,
            BinaryOp::And,
            BinaryOp::Or,
        ];

        // Ensure all operators are unique
        for (i, op1) in ops.iter().enumerate() {
            for (j, op2) in ops.iter().enumerate() {
                if i != j {
                    assert_ne!(op1, op2);
                }
            }
        }
    }
}

#[cfg(test)]
mod unary_op_tests {
    use super::*;

    #[test]
    fn test_unary_op_variants() {
        assert_ne!(UnaryOp::Minus, UnaryOp::Not);
    }
}

#[cfg(test)]
mod expression_tests {
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
                column: 5,
                offset: 4,
            },
        }
    }

    fn create_spanned_expr(expr: Expression) -> SpannedExpression {
        Spanned {
            node: expr,
            span: create_test_span(),
        }
    }

    #[test]
    fn test_literal_expression() {
        let expr = Expression::Literal(Literal::Number(42.0));
        match expr {
            Expression::Literal(Literal::Number(n)) => assert_eq!(n, 42.0),
            _ => panic!("Expected literal number"),
        }
    }

    #[test]
    fn test_identifier_expression() {
        let expr = Expression::Identifier("test_var".to_string());
        match expr {
            Expression::Identifier(name) => assert_eq!(name, "test_var"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_binary_expression() {
        let left = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));
        let right = create_spanned_expr(Expression::Literal(Literal::Number(2.0)));

        let expr = Expression::Binary {
            left: Box::new(left),
            operator: BinaryOp::Add,
            right: Box::new(right),
        };

        match expr {
            Expression::Binary { operator, .. } => assert_eq!(operator, BinaryOp::Add),
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_unary_expression() {
        let operand = create_spanned_expr(Expression::Literal(Literal::Number(5.0)));

        let expr = Expression::Unary {
            operator: UnaryOp::Minus,
            operand: Box::new(operand),
        };

        match expr {
            Expression::Unary { operator, .. } => assert_eq!(operator, UnaryOp::Minus),
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_call_expression() {
        let callee = create_spanned_expr(Expression::Identifier("func".to_string()));
        let arg1 = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));
        let arg2 = create_spanned_expr(Expression::Literal(Literal::Number(2.0)));

        let expr = Expression::Call {
            callee: Box::new(callee),
            args: vec![arg1, arg2],
            kwargs: vec![(
                "key".to_string(),
                create_spanned_expr(Expression::Literal(Literal::String("value".to_string()))),
            )],
        };

        match expr {
            Expression::Call { args, kwargs, .. } => {
                assert_eq!(args.len(), 2);
                assert_eq!(kwargs.len(), 1);
                assert_eq!(kwargs[0].0, "key");
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_list_expression() {
        let elem1 = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));
        let elem2 = create_spanned_expr(Expression::Literal(Literal::Number(2.0)));

        let expr = Expression::List {
            elements: vec![elem1, elem2],
        };

        match expr {
            Expression::List { elements } => assert_eq!(elements.len(), 2),
            _ => panic!("Expected list expression"),
        }
    }

    #[test]
    fn test_musical_event_expression() {
        let duration = create_spanned_expr(Expression::Literal(Literal::Duration {
            numerator: 1,
            denominator: 4,
            dots: 0,
        }));
        let pitches = create_spanned_expr(Expression::Literal(Literal::Pitch {
            note: "c".to_string(),
            accidentals: 0,
            octave: Some(4),
        }));
        let dynamic = Some(Box::new(create_spanned_expr(Expression::Literal(
            Literal::Dynamic("mf".to_string()),
        ))));

        let expr = Expression::MusicalEvent {
            duration: Box::new(duration),
            pitches: Box::new(pitches),
            dynamic,
            attributes: vec![],
        };

        match expr {
            Expression::MusicalEvent { dynamic, .. } => assert!(dynamic.is_some()),
            _ => panic!("Expected musical event expression"),
        }
    }

    #[test]
    fn test_pitch_class_set_expression() {
        let expr = Expression::PitchClassSet {
            classes: vec![0, 4, 7], // C major triad
        };

        match expr {
            Expression::PitchClassSet { classes } => {
                assert_eq!(classes, vec![0, 4, 7]);
            }
            _ => panic!("Expected pitch class set expression"),
        }
    }
}

#[cfg(test)]
mod statement_tests {
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

    fn create_spanned_expr(expr: Expression) -> SpannedExpression {
        Spanned {
            node: expr,
            span: create_test_span(),
        }
    }

    fn create_spanned_stmt(stmt: Statement) -> SpannedStatement {
        Spanned {
            node: stmt,
            span: create_test_span(),
        }
    }

    #[test]
    fn test_expression_statement() {
        let expr = create_spanned_expr(Expression::Literal(Literal::Number(42.0)));
        let stmt = Statement::Expression(expr);

        match stmt {
            Statement::Expression(e) => match e.node {
                Expression::Literal(Literal::Number(n)) => assert_eq!(n, 42.0),
                _ => panic!("Expected number literal"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_variable_declaration() {
        let value = create_spanned_expr(Expression::Literal(Literal::String("hello".to_string())));
        let stmt = Statement::VariableDeclaration {
            name: "my_var".to_string(),
            value,
        };

        match stmt {
            Statement::VariableDeclaration { name, .. } => assert_eq!(name, "my_var"),
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_function_declaration() {
        let body_stmt = create_spanned_stmt(Statement::Return { value: None });
        let stmt = Statement::FunctionDeclaration {
            name: "my_func".to_string(),
            params: vec!["param1".to_string(), "param2".to_string()],
            body: vec![body_stmt],
        };

        match stmt {
            Statement::FunctionDeclaration { name, params, body } => {
                assert_eq!(name, "my_func");
                assert_eq!(params.len(), 2);
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_return_statement() {
        let value = Some(create_spanned_expr(Expression::Literal(Literal::Boolean(
            true,
        ))));
        let stmt = Statement::Return { value };

        match stmt {
            Statement::Return { value: Some(_) } => (),
            _ => panic!("Expected return statement with value"),
        }

        let empty_return = Statement::Return { value: None };
        match empty_return {
            Statement::Return { value: None } => (),
            _ => panic!("Expected return statement without value"),
        }
    }

    #[test]
    fn test_metadata_statement() {
        let value = create_spanned_expr(Expression::Literal(Literal::String(
            "My Composition".to_string(),
        )));
        let stmt = Statement::Metadata {
            key: "title".to_string(),
            value,
        };

        match stmt {
            Statement::Metadata { key, .. } => assert_eq!(key, "title"),
            _ => panic!("Expected metadata statement"),
        }
    }
}

#[cfg(test)]
mod comment_tests {
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

    #[test]
    fn test_comment_info_new() {
        let comment = CommentInfo::new("Test comment".to_string(), create_test_span());
        assert_eq!(comment.content, "Test comment");
        assert_eq!(comment.position, CommentPosition::Standalone);
        assert!(comment.associated_span.is_none());
    }

    #[test]
    fn test_comment_with_position() {
        let span = create_test_span();
        let comment = CommentInfo::new("Test comment".to_string(), span)
            .with_position(CommentPosition::Leading, span);

        assert_eq!(comment.position, CommentPosition::Leading);
        assert_eq!(comment.associated_span, Some(span));
    }

    #[test]
    fn test_comment_is_same_line() {
        let comment_span = Span {
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
        };
        let other_span = Span {
            start: Position {
                line: 1,
                column: 15,
                offset: 14,
            },
            end: Position {
                line: 1,
                column: 20,
                offset: 19,
            },
        };

        let comment = CommentInfo::new("Test".to_string(), comment_span);
        assert!(comment.is_same_line(&other_span));

        let different_line_span = Span {
            start: Position {
                line: 2,
                column: 1,
                offset: 20,
            },
            end: Position {
                line: 2,
                column: 10,
                offset: 29,
            },
        };
        assert!(!comment.is_same_line(&different_line_span));
    }

    #[test]
    fn test_comment_is_before() {
        let comment_span = Span {
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
        };
        let after_span = Span {
            start: Position {
                line: 1,
                column: 15,
                offset: 14,
            },
            end: Position {
                line: 1,
                column: 20,
                offset: 19,
            },
        };

        let comment = CommentInfo::new("Test".to_string(), comment_span);
        assert!(comment.is_before(&after_span));

        let before_span = Span {
            start: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            end: Position {
                line: 1,
                column: 5,
                offset: 4,
            },
        };
        assert!(!comment.is_before(&before_span));
    }

    #[test]
    fn test_comment_position_variants() {
        assert_eq!(CommentPosition::Leading, CommentPosition::Leading);
        assert_eq!(CommentPosition::Trailing, CommentPosition::Trailing);
        assert_eq!(CommentPosition::Between(0), CommentPosition::Between(0));
        assert_eq!(CommentPosition::Standalone, CommentPosition::Standalone);

        assert_ne!(CommentPosition::Leading, CommentPosition::Trailing);
        assert_ne!(CommentPosition::Between(0), CommentPosition::Between(1));
    }
}

#[cfg(test)]
mod program_tests {
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

    fn create_spanned_stmt(stmt: Statement) -> SpannedStatement {
        Spanned {
            node: stmt,
            span: create_test_span(),
        }
    }

    #[test]
    fn test_program_creation() {
        let stmt = create_spanned_stmt(Statement::Expression(Spanned {
            node: Expression::Literal(Literal::Number(42.0)),
            span: create_test_span(),
        }));

        let comment = CommentInfo::new("Test comment".to_string(), create_test_span());

        let program = Program {
            statements: vec![stmt],
            comments: vec![comment],
            span: create_test_span(),
        };

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.comments.len(), 1);
    }

    #[test]
    fn test_leading_comments() {
        let span = create_test_span();
        let comment = CommentInfo::new("Leading comment".to_string(), create_test_span())
            .with_position(CommentPosition::Leading, span);

        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: create_test_span(),
        };

        let leading = program.leading_comments(&span);
        assert_eq!(leading.len(), 1);
        assert_eq!(leading[0].content, "Leading comment");
    }

    #[test]
    fn test_trailing_comments() {
        let span = create_test_span();
        let comment = CommentInfo::new("Trailing comment".to_string(), create_test_span())
            .with_position(CommentPosition::Trailing, span);

        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: create_test_span(),
        };

        let trailing = program.trailing_comments(&span);
        assert_eq!(trailing.len(), 1);
        assert_eq!(trailing[0].content, "Trailing comment");
    }

    #[test]
    fn test_standalone_comments() {
        let comment = CommentInfo::new("Standalone comment".to_string(), create_test_span());

        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: create_test_span(),
        };

        let standalone = program.standalone_comments();
        assert_eq!(standalone.len(), 1);
        assert_eq!(standalone[0].content, "Standalone comment");
    }

    #[test]
    fn test_comments_in_range() {
        let comment1 = CommentInfo::new(
            "Comment 1".to_string(),
            Span {
                start: Position {
                    line: 1,
                    column: 1,
                    offset: 5,
                },
                end: Position {
                    line: 1,
                    column: 10,
                    offset: 14,
                },
            },
        );
        let comment2 = CommentInfo::new(
            "Comment 2".to_string(),
            Span {
                start: Position {
                    line: 1,
                    column: 1,
                    offset: 25,
                },
                end: Position {
                    line: 1,
                    column: 10,
                    offset: 34,
                },
            },
        );

        let program = Program {
            statements: vec![],
            comments: vec![comment1, comment2],
            span: create_test_span(),
        };

        let in_range = program.comments_in_range(0, 20);
        assert_eq!(in_range.len(), 1);
        assert_eq!(in_range[0].content, "Comment 1");
    }

    #[test]
    fn test_has_comments_between() {
        let span1 = Span {
            start: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            end: Position {
                line: 1,
                column: 5,
                offset: 4,
            },
        };
        let span2 = Span {
            start: Position {
                line: 1,
                column: 20,
                offset: 19,
            },
            end: Position {
                line: 1,
                column: 25,
                offset: 24,
            },
        };
        let comment = CommentInfo::new(
            "Between comment".to_string(),
            Span {
                start: Position {
                    line: 1,
                    column: 10,
                    offset: 9,
                },
                end: Position {
                    line: 1,
                    column: 15,
                    offset: 14,
                },
            },
        );

        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: create_test_span(),
        };

        assert!(program.has_comments_between(&span1, &span2));

        // Test with no comments between
        let span3 = Span {
            start: Position {
                line: 1,
                column: 30,
                offset: 29,
            },
            end: Position {
                line: 1,
                column: 35,
                offset: 34,
            },
        };
        assert!(!program.has_comments_between(&span2, &span3));
    }
}

#[cfg(test)]
mod display_tests {
    use super::*;

    #[test]
    fn test_literal_display() {
        assert_eq!(Literal::Number(42.0).to_string(), "42");
        assert_eq!(
            Literal::String("hello".to_string()).to_string(),
            "\"hello\""
        );
        assert_eq!(Literal::Boolean(true).to_string(), "true");
        assert_eq!(Literal::Boolean(false).to_string(), "false");
    }

    #[test]
    fn test_pitch_display() {
        let pitch = Literal::Pitch {
            note: "c".to_string(),
            accidentals: 0,
            octave: Some(4),
        };
        assert_eq!(pitch.to_string(), "C4");

        let sharp_pitch = Literal::Pitch {
            note: "f".to_string(),
            accidentals: 1,
            octave: Some(5),
        };
        assert_eq!(sharp_pitch.to_string(), "F#5");

        let flat_pitch = Literal::Pitch {
            note: "b".to_string(),
            accidentals: -1,
            octave: None,
        };
        assert_eq!(flat_pitch.to_string(), "Bb");
    }

    #[test]
    fn test_duration_display() {
        let duration = Literal::Duration {
            numerator: 1,
            denominator: 4,
            dots: 0,
        };
        assert_eq!(duration.to_string(), "1/4");

        let dotted_duration = Literal::Duration {
            numerator: 1,
            denominator: 2,
            dots: 1,
        };
        assert_eq!(dotted_duration.to_string(), "1/2.");
    }

    #[test]
    fn test_rest_display() {
        let rest = Literal::Rest { duration: None };
        assert_eq!(rest.to_string(), "~");

        let timed_rest = Literal::Rest {
            duration: Some(Box::new(Literal::Duration {
                numerator: 1,
                denominator: 4,
                dots: 0,
            })),
        };
        assert_eq!(timed_rest.to_string(), "~1/4");
    }

    #[test]
    fn test_dynamic_display() {
        let dynamic = Literal::Dynamic("ff".to_string());
        assert_eq!(dynamic.to_string(), "ff");
    }

    #[test]
    fn test_attribute_display() {
        let attribute = Literal::Attribute("staccato".to_string());
        assert_eq!(attribute.to_string(), "staccato");
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_literal_pitch_edge_cases() {
        // Test pitch with maximum accidentals
        let pitch_many_sharps = Literal::parse_pitch("c####").unwrap();
        assert_eq!(
            pitch_many_sharps,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 4,
                octave: None,
            }
        );

        let pitch_many_flats = Literal::parse_pitch("fbbbb").unwrap();
        assert_eq!(
            pitch_many_flats,
            Literal::Pitch {
                note: "f".to_string(),
                accidentals: -4,
                octave: None,
            }
        );

        // Test with octave 0
        let pitch_octave_zero = Literal::parse_pitch("c0").unwrap();
        assert_eq!(
            pitch_octave_zero,
            Literal::Pitch {
                note: "c".to_string(),
                accidentals: 0,
                octave: Some(0),
            }
        );

        // Test with high octave
        let pitch_high_octave = Literal::parse_pitch("g9").unwrap();
        assert_eq!(
            pitch_high_octave,
            Literal::Pitch {
                note: "g".to_string(),
                accidentals: 0,
                octave: Some(9),
            }
        );
    }

    #[test]
    fn test_literal_duration_edge_cases() {
        // Test large numerator
        let large_duration = Literal::parse_duration("32/1").unwrap();
        assert_eq!(
            large_duration,
            Literal::Duration {
                numerator: 32,
                denominator: 1,
                dots: 0,
            }
        );

        // Test small denominator with dots
        let complex_duration = Literal::parse_duration("3/64...").unwrap();
        assert_eq!(
            complex_duration,
            Literal::Duration {
                numerator: 3,
                denominator: 64,
                dots: 3,
            }
        );

        // Test zero numerator (valid in some contexts)
        let zero_duration = Literal::parse_duration("0/4").unwrap();
        assert_eq!(
            zero_duration,
            Literal::Duration {
                numerator: 0,
                denominator: 4,
                dots: 0,
            }
        );
    }

    #[test]
    fn test_deeply_nested_expressions() {
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

        fn create_spanned_expr(expr: Expression) -> SpannedExpression {
            Spanned {
                node: expr,
                span: create_test_span(),
            }
        }

        // Create deeply nested binary operations: ((1 + 2) * 3) / 4
        let inner = create_spanned_expr(Expression::Binary {
            left: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                1.0,
            )))),
            operator: BinaryOp::Add,
            right: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                2.0,
            )))),
        });

        let middle = create_spanned_expr(Expression::Binary {
            left: Box::new(inner),
            operator: BinaryOp::Multiply,
            right: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                3.0,
            )))),
        });

        let outer = Expression::Binary {
            left: Box::new(middle),
            operator: BinaryOp::Divide,
            right: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                4.0,
            )))),
        };

        // Test that deeply nested structures are handled correctly
        match outer {
            Expression::Binary { operator, .. } => assert_eq!(operator, BinaryOp::Divide),
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_empty_structures() {
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

        // Test empty function body
        let empty_func = Statement::FunctionDeclaration {
            name: "empty_func".to_string(),
            params: vec![],
            body: vec![],
        };

        match empty_func {
            Statement::FunctionDeclaration { body, .. } => assert!(body.is_empty()),
            _ => panic!("Expected function declaration"),
        }

        // Test empty program
        let empty_program = Program {
            statements: vec![],
            comments: vec![],
            span: create_test_span(),
        };

        assert!(empty_program.statements.is_empty());
        assert!(empty_program.comments.is_empty());
    }

    #[test]
    fn test_complex_comment_positions() {
        fn create_span_at_offset(offset: usize, end_offset: usize) -> Span {
            Span {
                start: Position {
                    line: 1,
                    column: 1,
                    offset,
                },
                end: Position {
                    line: 1,
                    column: 10,
                    offset: end_offset,
                },
            }
        }

        // Create a container span that encompasses both comment positions
        let container_span = create_span_at_offset(0, 50);

        let comment1 = CommentInfo::new("First".to_string(), create_span_at_offset(5, 10))
            .with_position(CommentPosition::Between(0), container_span);

        let comment2 = CommentInfo::new("Second".to_string(), create_span_at_offset(25, 30))
            .with_position(CommentPosition::Between(1), container_span);

        let program = Program {
            statements: vec![],
            comments: vec![comment1, comment2],
            span: container_span,
        };

        let between_0 = program.between_comments(&container_span, 0);
        let between_1 = program.between_comments(&container_span, 1);

        assert_eq!(between_0.len(), 1);
        assert_eq!(between_1.len(), 1);
        assert_eq!(between_0[0].content, "First");
        assert_eq!(between_1[0].content, "Second");
    }

    #[test]
    fn test_boundary_offset_calculations() {
        let span1 = Span {
            start: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            end: Position {
                line: 1,
                column: 5,
                offset: 4,
            },
        };

        let span2 = Span {
            start: Position {
                line: 1,
                column: 10,
                offset: 9,
            },
            end: Position {
                line: 1,
                column: 15,
                offset: 14,
            },
        };

        // Comment exactly between spans
        let comment = CommentInfo::new(
            "Between".to_string(),
            Span {
                start: Position {
                    line: 1,
                    column: 6,
                    offset: 5,
                },
                end: Position {
                    line: 1,
                    column: 9,
                    offset: 8,
                },
            },
        );

        assert!(comment.is_before(&span2));
        assert!(!comment.is_before(&span1));

        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: span1,
        };

        assert!(program.has_comments_between(&span1, &span2));
    }

    #[test]
    fn test_display_formatting_edge_cases() {
        // Test pitch with no octave and multiple accidentals
        let complex_pitch = Literal::Pitch {
            note: "f".to_string(),
            accidentals: -2,
            octave: None,
        };
        assert_eq!(complex_pitch.to_string(), "Fbb");

        // Test duration with many dots
        let dotted_duration = Literal::Duration {
            numerator: 1,
            denominator: 8,
            dots: 3,
        };
        assert_eq!(dotted_duration.to_string(), "1/8...");

        // Test rest with complex duration
        let complex_rest = Literal::Rest {
            duration: Some(Box::new(Literal::Duration {
                numerator: 5,
                denominator: 16,
                dots: 2,
            })),
        };
        assert_eq!(complex_rest.to_string(), "~5/16..");
    }

    #[test]
    fn test_expression_display_coverage() {
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

        fn create_spanned_expr(expr: Expression) -> SpannedExpression {
            Spanned {
                node: expr,
                span: create_test_span(),
            }
        }

        // Test pitch class set display
        let pcs = Expression::PitchClassSet {
            classes: vec![0, 1, 11],
        };
        assert_eq!(pcs.to_string(), "{0, 1, 11}");

        // Test musical event display
        let event = Expression::MusicalEvent {
            duration: Box::new(create_spanned_expr(Expression::Literal(
                Literal::Duration {
                    numerator: 1,
                    denominator: 4,
                    dots: 0,
                },
            ))),
            pitches: Box::new(create_spanned_expr(Expression::Literal(Literal::Pitch {
                note: "c".to_string(),
                accidentals: 0,
                octave: Some(4),
            }))),
            dynamic: Some(Box::new(create_spanned_expr(Expression::Literal(
                Literal::Dynamic("f".to_string()),
            )))),
            attributes: vec![create_spanned_expr(Expression::Literal(
                Literal::Attribute("staccato".to_string()),
            ))],
        };

        let event_str = event.to_string();
        assert!(event_str.contains("1/4"));
        assert!(event_str.contains("C4"));
        assert!(event_str.contains("f"));
        assert!(event_str.contains("staccato"));

        // Test pipe expression display
        let pipe = Expression::Pipe {
            left: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
            right: Box::new(create_spanned_expr(Expression::Identifier(
                "process".to_string(),
            ))),
        };
        assert_eq!(pipe.to_string(), "42 |> process");
    }
}

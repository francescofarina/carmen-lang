use carmen_lang::ast::inspector::{inspect_ast, inspect_ast_to_file, AstInspector};
use carmen_lang::ast::*;
use carmen_lang::errors::{Position, Span};
use std::env;
use std::fs;

#[cfg(test)]
mod ast_inspector_tests {
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
    fn test_dump_empty_program() {
        let mut inspector = AstInspector::new();
        let program = Program {
            statements: vec![],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("=== CARMEN AST DUMP ==="));
        assert!(output.contains("Statements: 0 total"));
    }

    #[test]
    fn test_dump_program_with_comments() {
        let mut inspector = AstInspector::new();
        let comment = CommentInfo::new("Test comment".to_string(), create_test_span());
        let program = Program {
            statements: vec![],
            comments: vec![comment],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Comments: 1 total"));
        assert!(output.contains("// Test comment"));
        assert!(output.contains("(standalone)"));
    }

    #[test]
    fn test_dump_program_with_statements() {
        let mut inspector = AstInspector::new();
        let stmt = create_spanned_stmt(Statement::Expression(create_spanned_expr(
            Expression::Literal(Literal::Number(42.0)),
        )));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Statements: 1 total"));
        assert!(output.contains("Statement 1"));
        assert!(output.contains("Type: Expression Statement"));
    }

    #[test]
    fn test_dump_expression_statement() {
        let mut inspector = AstInspector::new();
        let expr = create_spanned_expr(Expression::Literal(Literal::String("hello".to_string())));
        let stmt = create_spanned_stmt(Statement::Expression(expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Expression Statement"));
        assert!(output.contains("Type: String"));
        assert!(output.contains("Value: \"hello\""));
    }

    #[test]
    fn test_dump_variable_declaration() {
        let mut inspector = AstInspector::new();
        let value = create_spanned_expr(Expression::Literal(Literal::Number(123.0)));
        let stmt = create_spanned_stmt(Statement::VariableDeclaration {
            name: "my_var".to_string(),
            value,
        });
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Variable Declaration"));
        assert!(output.contains("Name: my_var"));
        assert!(output.contains("Value: 123"));
    }

    #[test]
    fn test_dump_function_declaration() {
        let mut inspector = AstInspector::new();
        let return_stmt = create_spanned_stmt(Statement::Return {
            value: Some(create_spanned_expr(Expression::Literal(Literal::Boolean(
                true,
            )))),
        });
        let stmt = create_spanned_stmt(Statement::FunctionDeclaration {
            name: "test_func".to_string(),
            params: vec!["param1".to_string(), "param2".to_string()],
            body: vec![return_stmt],
        });
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Function Declaration"));
        assert!(output.contains("Name: test_func"));
        assert!(output.contains("Parameters: [param1, param2]"));
        assert!(output.contains("Body: 1 statements"));
        assert!(output.contains("Body Statement 1:"));
        assert!(output.contains("Type: Return Statement"));
    }

    #[test]
    fn test_dump_return_statement() {
        let mut inspector = AstInspector::new();
        let stmt_with_value = create_spanned_stmt(Statement::Return {
            value: Some(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
        });
        let stmt_without_value = create_spanned_stmt(Statement::Return { value: None });

        let program = Program {
            statements: vec![stmt_with_value, stmt_without_value],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Return Statement"));
        assert!(output.contains("Value: 42"));
        assert!(output.contains("Value: None"));
    }

    #[test]
    fn test_dump_metadata_statement() {
        let mut inspector = AstInspector::new();
        let value = create_spanned_expr(Expression::Literal(Literal::String(
            "My Composition".to_string(),
        )));
        let stmt = create_spanned_stmt(Statement::Metadata {
            key: "title".to_string(),
            value,
        });
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Metadata"));
        assert!(output.contains("Key: title"));
        assert!(output.contains("Value: \"My Composition\""));
    }

    #[test]
    fn test_dump_literal_expressions() {
        let mut inspector = AstInspector::new();

        // Test number literal
        let number_expr =
            create_spanned_expr(Expression::Literal(Literal::Number(std::f64::consts::PI)));
        let number_stmt = create_spanned_stmt(Statement::Expression(number_expr));

        // Test boolean literal
        let bool_expr = create_spanned_expr(Expression::Literal(Literal::Boolean(false)));
        let bool_stmt = create_spanned_stmt(Statement::Expression(bool_expr));

        let program = Program {
            statements: vec![number_stmt, bool_stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Number"));
        assert!(output.contains(&format!("Value: {}", std::f64::consts::PI)));
        assert!(output.contains("Type: Boolean"));
        assert!(output.contains("Value: false"));
    }

    #[test]
    fn test_dump_pitch_literal() {
        let mut inspector = AstInspector::new();
        let pitch_expr = create_spanned_expr(Expression::Literal(Literal::Pitch {
            note: "c".to_string(),
            accidentals: 1,
            octave: Some(4),
        }));
        let stmt = create_spanned_stmt(Statement::Expression(pitch_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Pitch"));
        assert!(output.contains("Note: C"));
        assert!(output.contains("Accidentals: 1"));
        assert!(output.contains("Octave: 4"));
    }

    #[test]
    fn test_dump_pitch_class_literal() {
        let mut inspector = AstInspector::new();
        let pitch_expr = create_spanned_expr(Expression::Literal(Literal::Pitch {
            note: "f".to_string(),
            accidentals: -1,
            octave: None,
        }));
        let stmt = create_spanned_stmt(Statement::Expression(pitch_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Pitch"));
        assert!(output.contains("Note: F"));
        assert!(output.contains("Accidentals: -1"));
        assert!(output.contains("Octave: None (Pitch Class)"));
    }

    #[test]
    fn test_dump_duration_literal() {
        let mut inspector = AstInspector::new();
        let duration_expr = create_spanned_expr(Expression::Literal(Literal::Duration {
            numerator: 1,
            denominator: 4,
            dots: 1,
        }));
        let stmt = create_spanned_stmt(Statement::Expression(duration_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Duration"));
        assert!(output.contains("Base: 1/4"));
        assert!(output.contains("Dots: 1"));
    }

    #[test]
    fn test_dump_dynamic_literal() {
        let mut inspector = AstInspector::new();
        let dynamic_expr =
            create_spanned_expr(Expression::Literal(Literal::Dynamic("ff".to_string())));
        let stmt = create_spanned_stmt(Statement::Expression(dynamic_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Dynamic"));
        assert!(output.contains("Value: ff"));
    }

    #[test]
    fn test_dump_attribute_literal() {
        let mut inspector = AstInspector::new();
        let attr_expr = create_spanned_expr(Expression::Literal(Literal::Attribute(
            "staccato".to_string(),
        )));
        let stmt = create_spanned_stmt(Statement::Expression(attr_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Attribute"));
        assert!(output.contains("Value: staccato"));
    }

    #[test]
    fn test_dump_rest_literal() {
        let mut inspector = AstInspector::new();

        // Rest without duration
        let rest_expr = create_spanned_expr(Expression::Literal(Literal::Rest { duration: None }));
        let rest_stmt = create_spanned_stmt(Statement::Expression(rest_expr));

        // Rest with duration
        let rest_with_duration = create_spanned_expr(Expression::Literal(Literal::Rest {
            duration: Some(Box::new(Literal::Duration {
                numerator: 1,
                denominator: 2,
                dots: 0,
            })),
        }));
        let rest_duration_stmt = create_spanned_stmt(Statement::Expression(rest_with_duration));

        let program = Program {
            statements: vec![rest_stmt, rest_duration_stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Rest"));
        assert!(output.contains("Duration: Inherited from context"));
        assert!(output.contains("Duration:"));
        assert!(output.contains("Base: 1/2"));
    }

    #[test]
    fn test_dump_identifier_expression() {
        let mut inspector = AstInspector::new();
        let id_expr = create_spanned_expr(Expression::Identifier("my_variable".to_string()));
        let stmt = create_spanned_stmt(Statement::Expression(id_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Identifier"));
        assert!(output.contains("Name: my_variable"));
    }

    #[test]
    fn test_dump_binary_expression() {
        let mut inspector = AstInspector::new();
        let left = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));
        let right = create_spanned_expr(Expression::Literal(Literal::Number(2.0)));
        let binary_expr = create_spanned_expr(Expression::Binary {
            left: Box::new(left),
            operator: BinaryOp::Add,
            right: Box::new(right),
        });
        let stmt = create_spanned_stmt(Statement::Expression(binary_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Binary Operation"));
        assert!(output.contains("Operator: Add"));
        assert!(output.contains("Left:"));
        assert!(output.contains("Right:"));
    }

    #[test]
    fn test_dump_unary_expression() {
        let mut inspector = AstInspector::new();
        let operand = create_spanned_expr(Expression::Literal(Literal::Number(5.0)));
        let unary_expr = create_spanned_expr(Expression::Unary {
            operator: UnaryOp::Minus,
            operand: Box::new(operand),
        });
        let stmt = create_spanned_stmt(Statement::Expression(unary_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Unary Operation"));
        assert!(output.contains("Operator: Minus"));
        assert!(output.contains("Operand:"));
    }

    #[test]
    fn test_dump_call_expression() {
        let mut inspector = AstInspector::new();
        let callee = create_spanned_expr(Expression::Identifier("func".to_string()));
        let arg1 = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));
        let arg2 = create_spanned_expr(Expression::Literal(Literal::Number(2.0)));
        let call_expr = create_spanned_expr(Expression::Call {
            callee: Box::new(callee),
            args: vec![arg1, arg2],
            kwargs: vec![(
                "key".to_string(),
                create_spanned_expr(Expression::Literal(Literal::String("value".to_string()))),
            )],
        });
        let stmt = create_spanned_stmt(Statement::Expression(call_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Function Call"));
        assert!(output.contains("Arguments: 2 total"));
        assert!(output.contains("Named Arguments: 1 total"));
        assert!(output.contains("key:"));
    }

    #[test]
    fn test_dump_pipe_expression() {
        let mut inspector = AstInspector::new();
        let left = create_spanned_expr(Expression::Literal(Literal::Number(42.0)));
        let right = create_spanned_expr(Expression::Identifier("process".to_string()));
        let pipe_expr = create_spanned_expr(Expression::Pipe {
            left: Box::new(left),
            right: Box::new(right),
        });
        let stmt = create_spanned_stmt(Statement::Expression(pipe_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Pipe Operation"));
        assert!(output.contains("Input:"));
        assert!(output.contains("Function:"));
    }

    #[test]
    fn test_dump_collection_expressions() {
        let mut inspector = AstInspector::new();
        let elem = create_spanned_expr(Expression::Literal(Literal::Number(1.0)));

        // Test list
        let list_expr = create_spanned_expr(Expression::List {
            elements: vec![elem.clone()],
        });
        let list_stmt = create_spanned_stmt(Statement::Expression(list_expr));

        // Test tuple
        let tuple_expr = create_spanned_expr(Expression::Tuple {
            elements: vec![elem.clone()],
        });
        let tuple_stmt = create_spanned_stmt(Statement::Expression(tuple_expr));

        // Test set
        let set_expr = create_spanned_expr(Expression::Set {
            elements: vec![elem],
        });
        let set_stmt = create_spanned_stmt(Statement::Expression(set_expr));

        let program = Program {
            statements: vec![list_stmt, tuple_stmt, set_stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: List (1 elements)"));
        assert!(output.contains("Type: Tuple (1 elements)"));
        assert!(output.contains("Type: Set (1 elements)"));
        assert!(output.contains("Element 1:"));
    }

    #[test]
    fn test_dump_pitch_class_set_expression() {
        let mut inspector = AstInspector::new();
        let pcs_expr = create_spanned_expr(Expression::PitchClassSet {
            classes: vec![0, 4, 7], // C major triad
        });
        let stmt = create_spanned_stmt(Statement::Expression(pcs_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Pitch Class Set"));
        assert!(output.contains("Classes: [0, 4, 7]"));
    }

    #[test]
    fn test_dump_musical_event_expression() {
        let mut inspector = AstInspector::new();
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
        let attr = create_spanned_expr(Expression::Literal(Literal::Attribute(
            "staccato".to_string(),
        )));

        let event_expr = create_spanned_expr(Expression::MusicalEvent {
            duration: Box::new(duration),
            pitches: Box::new(pitches),
            dynamic,
            attributes: vec![attr],
        });
        let stmt = create_spanned_stmt(Statement::Expression(event_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Musical Event"));
        assert!(output.contains("Duration:"));
        assert!(output.contains("Pitches:"));
        assert!(output.contains("Dynamic:"));
        assert!(output.contains("Attributes: 1 total"));
    }

    #[test]
    fn test_dump_structural_expressions() {
        let mut inspector = AstInspector::new();
        let body = create_spanned_expr(Expression::Literal(Literal::Number(42.0)));

        // Test Part
        let part_expr = create_spanned_expr(Expression::Part {
            name: Some(Box::new(create_spanned_expr(Expression::Literal(
                Literal::String("Violin".to_string()),
            )))),
            body: Box::new(body.clone()),
        });

        // Test Score
        let score_expr = create_spanned_expr(Expression::Score {
            name: Some(Box::new(create_spanned_expr(Expression::Literal(
                Literal::String("Symphony".to_string()),
            )))),
            body: Box::new(body),
        });

        let program = Program {
            statements: vec![
                create_spanned_stmt(Statement::Expression(part_expr)),
                create_spanned_stmt(Statement::Expression(score_expr)),
            ],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Part"));
        assert!(output.contains("Type: Score"));
        assert!(output.contains("Name:"));
        assert!(output.contains("Body:"));
    }

    #[test]
    fn test_inspect_ast_function() {
        let stmt = create_spanned_stmt(Statement::Expression(create_spanned_expr(
            Expression::Literal(Literal::Number(42.0)),
        )));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspect_ast(&program);
        assert!(output.contains("=== CARMEN AST DUMP ==="));
        assert!(output.contains("Type: Expression Statement"));
        assert!(output.contains("Value: 42"));
    }

    #[test]
    fn test_inspect_ast_to_file() {
        let stmt = create_spanned_stmt(Statement::Expression(create_spanned_expr(
            Expression::Literal(Literal::String("test".to_string())),
        )));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        // Create a portable temporary file path
        let temp_dir = env::temp_dir();
        let temp_path = temp_dir.join("carmen_test_ast_output.txt");
        let temp_path_str = temp_path.to_str().unwrap();

        let result = inspect_ast_to_file(&program, temp_path_str);
        assert!(result.is_ok());

        // Verify file was created and contains expected content
        if temp_path.exists() {
            let content = fs::read_to_string(&temp_path).unwrap();
            assert!(content.contains("=== CARMEN AST DUMP ==="));
            assert!(content.contains("Type: String"));
            assert!(content.contains("Value: \"test\""));

            // Clean up temporary file
            let _ = fs::remove_file(&temp_path);
        }
    }

    #[test]
    fn test_complex_nested_structure() {
        let mut inspector = AstInspector::new();

        // Create a complex nested structure: if expression with block body
        let condition = create_spanned_expr(Expression::Binary {
            left: Box::new(create_spanned_expr(Expression::Identifier("x".to_string()))),
            operator: BinaryOp::Greater,
            right: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                0.0,
            )))),
        });

        let then_stmt = create_spanned_stmt(Statement::Expression(create_spanned_expr(
            Expression::Literal(Literal::String("positive".to_string())),
        )));

        let then_branch = create_spanned_expr(Expression::Block {
            statements: vec![then_stmt],
        });

        let if_expr = create_spanned_expr(Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: None,
        });

        let stmt = create_spanned_stmt(Statement::Expression(if_expr));
        let program = Program {
            statements: vec![stmt],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: If Expression"));
        assert!(output.contains("Condition:"));
        assert!(output.contains("Then:"));
        assert!(output.contains("Type: Binary Operation"));
        assert!(output.contains("Type: Block (1 statements)"));
    }

    #[test]
    fn test_dump_control_flow_expressions() {
        let mut inspector = AstInspector::new();

        // Test For expression
        let for_expr = create_spanned_expr(Expression::For {
            variable: "i".to_string(),
            iterable: Box::new(create_spanned_expr(Expression::List {
                elements: vec![create_spanned_expr(Expression::Literal(Literal::Number(
                    1.0,
                )))],
            })),
            body: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
        });

        // Test While expression
        let while_expr = create_spanned_expr(Expression::While {
            condition: Box::new(create_spanned_expr(Expression::Literal(Literal::Boolean(
                true,
            )))),
            body: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                1.0,
            )))),
        });

        let program = Program {
            statements: vec![
                create_spanned_stmt(Statement::Expression(for_expr)),
                create_spanned_stmt(Statement::Expression(while_expr)),
            ],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: For Loop"));
        assert!(output.contains("Variable: i"));
        assert!(output.contains("Iterable:"));
        assert!(output.contains("Type: While Loop"));
        assert!(output.contains("Condition:"));
    }

    #[test]
    fn test_dump_staff_and_timeline_expressions() {
        let mut inspector = AstInspector::new();

        // Test Staff expression
        let staff_expr = create_spanned_expr(Expression::Staff {
            number: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                1.0,
            )))),
            body: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
        });

        // Test Timeline expression
        let timeline_expr = create_spanned_expr(Expression::Timeline {
            body: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
        });

        // Test Movement expression
        let movement_expr = create_spanned_expr(Expression::Movement {
            name: Some(Box::new(create_spanned_expr(Expression::Literal(
                Literal::String("Allegro".to_string()),
            )))),
            body: Box::new(create_spanned_expr(Expression::Literal(Literal::Number(
                42.0,
            )))),
        });

        let program = Program {
            statements: vec![
                create_spanned_stmt(Statement::Expression(staff_expr)),
                create_spanned_stmt(Statement::Expression(timeline_expr)),
                create_spanned_stmt(Statement::Expression(movement_expr)),
            ],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Staff"));
        assert!(output.contains("Number:"));
        assert!(output.contains("Type: Timeline"));
        assert!(output.contains("Type: Movement"));
    }

    #[test]
    fn test_dump_block_expression() {
        let mut inspector = AstInspector::new();

        let inner_stmt = create_spanned_stmt(Statement::Expression(create_spanned_expr(
            Expression::Literal(Literal::Number(1.0)),
        )));

        let block_expr = create_spanned_expr(Expression::Block {
            statements: vec![inner_stmt],
        });

        let program = Program {
            statements: vec![create_spanned_stmt(Statement::Expression(block_expr))],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Block (1 statements)"));
        assert!(output.contains("Statement 1:"));
    }

    #[test]
    fn test_dump_empty_collections() {
        let mut inspector = AstInspector::new();

        // Test empty list
        let empty_list = create_spanned_expr(Expression::List { elements: vec![] });

        // Test empty tuple
        let empty_tuple = create_spanned_expr(Expression::Tuple { elements: vec![] });

        // Test empty set
        let empty_set = create_spanned_expr(Expression::Set { elements: vec![] });

        let program = Program {
            statements: vec![
                create_spanned_stmt(Statement::Expression(empty_list)),
                create_spanned_stmt(Statement::Expression(empty_tuple)),
                create_spanned_stmt(Statement::Expression(empty_set)),
            ],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: List (0 elements)"));
        assert!(output.contains("Type: Tuple (0 elements)"));
        assert!(output.contains("Type: Set (0 elements)"));
    }

    #[test]
    fn test_dump_call_expression_without_args() {
        let mut inspector = AstInspector::new();
        let callee = create_spanned_expr(Expression::Identifier("func".to_string()));
        let call_expr = create_spanned_expr(Expression::Call {
            callee: Box::new(callee),
            args: vec![],
            kwargs: vec![],
        });

        let program = Program {
            statements: vec![create_spanned_stmt(Statement::Expression(call_expr))],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Function Call"));
        assert!(output.contains("Function:"));
        // Should not contain "Arguments:" or "Named Arguments:" when empty
        assert!(!output.contains("Arguments: 0 total"));
        assert!(!output.contains("Named Arguments: 0 total"));
    }

    #[test]
    fn test_dump_musical_event_without_optional_fields() {
        let mut inspector = AstInspector::new();
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

        let event_expr = create_spanned_expr(Expression::MusicalEvent {
            duration: Box::new(duration),
            pitches: Box::new(pitches),
            dynamic: None,
            attributes: vec![],
        });

        let program = Program {
            statements: vec![create_spanned_stmt(Statement::Expression(event_expr))],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Musical Event"));
        assert!(output.contains("Duration:"));
        assert!(output.contains("Pitches:"));
        // Should not contain "Dynamic:" or "Attributes:" when empty/None
        assert!(!output.contains("Dynamic:"));
        assert!(!output.contains("Attributes: 0 total"));
    }

    #[test]
    fn test_dump_structural_expressions_without_names() {
        let mut inspector = AstInspector::new();
        let body = create_spanned_expr(Expression::Literal(Literal::Number(42.0)));

        // Test Part without name
        let part_expr = create_spanned_expr(Expression::Part {
            name: None,
            body: Box::new(body.clone()),
        });

        // Test Score without name
        let score_expr = create_spanned_expr(Expression::Score {
            name: None,
            body: Box::new(body.clone()),
        });

        // Test Movement without name
        let movement_expr = create_spanned_expr(Expression::Movement {
            name: None,
            body: Box::new(body),
        });

        let program = Program {
            statements: vec![
                create_spanned_stmt(Statement::Expression(part_expr)),
                create_spanned_stmt(Statement::Expression(score_expr)),
                create_spanned_stmt(Statement::Expression(movement_expr)),
            ],
            comments: vec![],
            span: create_test_span(),
        };

        let output = inspector.dump_program(&program);
        assert!(output.contains("Type: Part"));
        assert!(output.contains("Type: Score"));
        assert!(output.contains("Type: Movement"));
        assert!(output.contains("Body:"));
    }
}

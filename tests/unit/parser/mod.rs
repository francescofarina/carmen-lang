use carmen_lang::ast::*;
use carmen_lang::errors::{CarmenError, ErrorSource, Result};
use carmen_lang::lexer::{Lexer, Token};
use carmen_lang::parser::Parser;

// Helper to parse a vector of tokens and get the program AST
fn parse_program(tokens: Vec<Token>) -> Result<Program> {
    let parser = Parser::new(tokens);
    parser.parse()
}

// Helper to tokenize a string and then parse it into a program AST
fn tokenize_and_parse_program(input: &str) -> Result<Program> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    parse_program(tokens)
}

// Helper to get the first statement from a program
fn get_first_statement(program: Program) -> SpannedStatement {
    assert!(!program.statements.is_empty(), "Program has no statements");
    program.statements.into_iter().next().unwrap()
}

// Helper to parse and get the first statement
fn parse_first_statement(input: &str) -> Result<SpannedStatement> {
    let program = tokenize_and_parse_program(input)?;
    Ok(get_first_statement(program))
}

// Helper to parse a string and get the first statement's expression node
fn parse_expression_node(input: &str) -> Result<Expression> {
    let stmt = parse_first_statement(input)?;
    match stmt.node {
        Statement::Expression(expr) => Ok(expr.node),
        _ => panic!("Expected an expression statement, got {:?}", stmt.node),
    }
}

#[cfg(test)]
mod expression_parsing_tests {
    use super::*;

    #[test]
    fn test_parse_literals() {
        assert!(
            matches!(parse_expression_node("123;").unwrap(), Expression::Literal(Literal::Number(n)) if n == 123.0)
        );
        assert!(
            matches!(parse_expression_node("\"test\";").unwrap(), Expression::Literal(Literal::String(s)) if s == "test")
        );
        assert!(matches!(
            parse_expression_node("true;").unwrap(),
            Expression::Literal(Literal::Boolean(true))
        ));
        assert!(matches!(
            parse_expression_node("C4;").unwrap(),
            Expression::Literal(Literal::Pitch { .. })
        ));
        assert!(matches!(
            parse_expression_node("1/4;").unwrap(),
            Expression::Literal(Literal::Duration { .. })
        ));
    }

    #[test]
    fn test_parse_identifier() {
        assert!(
            matches!(parse_expression_node("my_var;").unwrap(), Expression::Identifier(s) if s == "my_var")
        );
    }

    #[test]
    fn test_binary_expression() {
        let expr = parse_expression_node("5 + 3;").unwrap();
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                assert!(matches!(left.node, Expression::Literal(Literal::Number(n)) if n == 5.0));
                assert_eq!(operator, BinaryOp::Add);
                assert!(matches!(right.node, Expression::Literal(Literal::Number(n)) if n == 3.0));
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_unary_expression() {
        let expr = parse_expression_node("-10;").unwrap();
        match expr {
            Expression::Unary { operator, operand } => {
                assert_eq!(operator, UnaryOp::Minus);
                assert!(
                    matches!(operand.node, Expression::Literal(Literal::Number(n)) if n == 10.0)
                );
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_function_call() {
        let expr = parse_expression_node("my_func(a, 1);").unwrap();
        match expr {
            Expression::Call {
                callee,
                args,
                kwargs,
            } => {
                assert!(matches!(callee.node, Expression::Identifier(s) if s == "my_func"));
                assert_eq!(args.len(), 2);
                assert!(kwargs.is_empty());
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_pipe_expression() {
        let expr = parse_expression_node("c4 |> transpose(2);").unwrap();
        assert!(matches!(expr, Expression::Pipe { .. }));
    }

    #[test]
    fn test_list_expression() {
        let expr = parse_expression_node("[1, 2, 3];").unwrap();
        assert!(matches!(expr, Expression::List { elements } if elements.len() == 3));
    }

    #[test]
    fn test_tuple_expression() {
        let expr = parse_expression_node("(c4, e4, g4);").unwrap();
        assert!(matches!(expr, Expression::Tuple { elements } if elements.len() == 3));
    }

    #[test]
    fn test_set_expression() {
        let expr = parse_expression_node("{a, 2, c};").unwrap();
        assert!(matches!(expr, Expression::Set { elements } if elements.len() == 3));
        let expr = parse_expression_node("{1, 2, 3};").unwrap();
        assert!(
            matches!(expr, Expression::PitchClassSet { classes: elements } if elements.len() == 3)
        );
    }

    #[test]
    fn test_pitch_class_set_expression() {
        let expr = parse_expression_node("{0, 4, 7};").unwrap();
        assert!(matches!(expr, Expression::PitchClassSet { classes } if classes == vec![0, 4, 7]));
    }
}

#[cfg(test)]
mod statement_parsing_tests {
    use super::*;

    #[test]
    fn test_variable_declaration() {
        let stmt = parse_first_statement("let x = 10;").unwrap();
        assert!(matches!(stmt.node, Statement::VariableDeclaration { name, .. } if name == "x"));

        // TODO: expect error: pitch are reserved
        // let stmt = parse_first_statement("let a = 10;").unwrap();
    }

    #[test]
    fn test_function_declaration() {
        let stmt = parse_first_statement("def my_func(x, y) { return x + y; };").unwrap();
        match stmt.node {
            Statement::FunctionDeclaration { name, params, body } => {
                assert_eq!(name, "my_func");
                assert_eq!(params, vec!["x", "y"]);
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_return_statement() {
        let stmt = parse_first_statement("return 42;").unwrap();
        assert!(matches!(stmt.node, Statement::Return { .. }));
    }

    #[test]
    fn test_metadata_statement() {
        let stmt = parse_first_statement("@tempo 120;").unwrap();
        match stmt.node {
            Statement::Metadata { key, .. } => {
                assert_eq!(key, "tempo");
            }
            _ => panic!("Expected metadata statement"),
        }
    }
}

#[cfg(test)]
mod musical_construct_parsing_tests {
    use super::*;

    #[test]
    fn test_musical_event() {
        let expr = parse_expression_node("1/4 c4;").unwrap();
        assert!(matches!(expr, Expression::MusicalEvent { .. }));
    }

    #[test]
    fn test_part_expression() {
        let expr = parse_expression_node("part { 1/4 c4; };").unwrap();
        assert!(matches!(expr, Expression::Part { .. }));
    }

    #[test]
    fn test_staff_expression() {
        let expr = parse_expression_node("staff 1 { 1/4 c4; };").unwrap();
        assert!(matches!(expr, Expression::Staff { .. }));
    }

    #[test]
    fn test_timeline_expression() {
        let expr = parse_expression_node("timeline { part { 1/4 c4; }; };").unwrap();
        assert!(matches!(expr, Expression::Timeline { .. }));
    }
}

#[cfg(test)]
mod error_handling_tests {
    use super::*;

    #[test]
    fn test_missing_semicolon() {
        let result = tokenize_and_parse_program("let x = 5");
        assert!(result.is_err());
        if let Err(CarmenError { source, .. }) = result {
            assert!(matches!(source, ErrorSource::Parsing(msg) if msg.contains("Expected ';'")));
        }
    }

    #[test]
    fn test_invalid_identifier() {
        let result = tokenize_and_parse_program("let if = 5;");
        assert!(result.is_err());
        if let Err(CarmenError { source, .. }) = result {
            assert!(
                matches!(source, ErrorSource::Parsing(msg) if msg.contains("Cannot use keyword 'if'"))
            );
        }
    }
}

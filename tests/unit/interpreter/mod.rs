pub mod arg_extraction;
pub mod builtins;

use carmen_lang::errors::Result;
use carmen_lang::interpreter::{Interpreter, Value};
use carmen_lang::lexer::Lexer;
use carmen_lang::parser::Parser;

// Helper to interpret a piece of code and return the final value
use carmen_lang::ast::Program;

// Helper to interpret a program AST and return the final value
fn interpret_program(program: &Program) -> Result<Value> {
    let mut interpreter = Interpreter::new();
    interpreter.interpret(program)
}

// Helper to interpret a piece of code from a string and return the final value
fn interpret_code(input: &str) -> Result<Value> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    let parser = Parser::new(tokens);
    let program = parser.parse()?;
    interpret_program(&program)
}

#[cfg(test)]
mod interpretation_tests {
    use super::*;
    use carmen_lang::core::{Pitch, PitchClass};

    #[test]
    fn test_interpret_literals() {
        assert_eq!(interpret_code("123;").unwrap(), Value::Number(123.0));
        assert_eq!(
            interpret_code("\"hello\";").unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(interpret_code("true;").unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_variable_declaration_and_retrieval() {
        let program = "let x = 10; x;";
        assert_eq!(interpret_code(program).unwrap(), Value::Number(10.0));
    }

    #[test]
    fn test_binary_operations() {
        assert_eq!(interpret_code("5 + 3;").unwrap(), Value::Number(8.0));
        assert_eq!(interpret_code("10 - 4;").unwrap(), Value::Number(6.0));
        assert_eq!(interpret_code("3 * 7;").unwrap(), Value::Number(21.0));
        assert_eq!(interpret_code("20 / 5;").unwrap(), Value::Number(4.0));
        assert_eq!(
            interpret_code("true and false;").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            interpret_code("true or false;").unwrap(),
            Value::Boolean(true)
        );
    }

    #[test]
    fn test_unary_operations() {
        assert_eq!(interpret_code("-15;").unwrap(), Value::Number(-15.0));
        assert_eq!(interpret_code("not true;").unwrap(), Value::Boolean(false));
    }

    #[test]
    fn test_function_declaration_and_call() {
        let program = "def add(x, y) { return x + y; }; add(5, 8);";
        assert_eq!(interpret_code(program).unwrap(), Value::Number(13.0));
    }

    #[test]
    fn test_pipe_operator() {
        let program = "C4 |> transpose(2);";
        let expected_pitch = Pitch {
            pitch_class: PitchClass::new(2), // D
            octave: 4,
        };
        assert_eq!(
            interpret_code(program).unwrap(),
            Value::Pitch(expected_pitch)
        );
    }

    #[test]
    fn test_if_expression() {
        assert_eq!(
            interpret_code("if (true) { 10; } else { 20; };").unwrap(),
            Value::Number(10.0)
        );
        assert_eq!(
            interpret_code("if (false) { 10; } else { 20; };").unwrap(),
            Value::Number(20.0)
        );
    }

    #[test]
    fn test_block_scoping() {
        let program = "let x = 5; { let x = 10; }; x;";
        assert_eq!(interpret_code(program).unwrap(), Value::Number(5.0));
    }
}

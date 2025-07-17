use carmen_lang::core::{Pitch, PitchClass, PitchClassSet};
use carmen_lang::errors::Result;
use carmen_lang::interpreter::{Interpreter, Value};
use carmen_lang::lexer::Lexer;
use carmen_lang::parser::Parser;

fn interpret_code(input: &str) -> Result<Value> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    let parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program)
}

#[cfg(test)]
mod builtin_function_tests {
    use super::*;

    #[test]
    fn test_transpose() {
        let result = interpret_code("transpose(C4, 2);").unwrap();
        let expected_pitch = Pitch {
            pitch_class: PitchClass::new(2),
            octave: 4,
        };
        assert_eq!(result, Value::Pitch(expected_pitch));
    }

    #[test]
    fn test_invert() {
        let result = interpret_code("invert(c4, 0);").unwrap();
        let expected_pitch = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        assert_eq!(result, Value::Pitch(expected_pitch));
    }

    #[test]
    fn test_pitch_interval() {
        let result = interpret_code("pitch_interval(C4, G4);").unwrap();
        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn test_normal_form() {
        let result = interpret_code("normal_form({0, 7, 4});").unwrap();
        let expected_pcs = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        assert_eq!(result, Value::PitchClassSet(expected_pcs));
    }

    #[test]
    fn test_prime_form() {
        let result = interpret_code("prime_form({2, 6, 9});").unwrap(); // D major
        let expected_pcs = PitchClassSet::from_u8_values(vec![0, 3, 7]);
        assert_eq!(result, Value::PitchClassSet(expected_pcs));
    }

    #[test]
    fn test_interval_class_vector() {
        let result = interpret_code("interval_class_vector({0, 4, 7});").unwrap();
        let expected_vec = Value::List(vec![
            Value::Number(0.0), // ic1
            Value::Number(0.0), // ic2
            Value::Number(1.0), // ic3
            Value::Number(1.0), // ic4
            Value::Number(1.0), // ic5
            Value::Number(0.0), // ic6
        ]);
        assert_eq!(result, expected_vec);
    }
}

use carmen_lang::core::{Chord, Pitch, PitchClass};
use carmen_lang::errors::{ErrorSource, Span};
use carmen_lang::extract_args;
use carmen_lang::extract_three;
use carmen_lang::extract_two;
use carmen_lang::interpreter::arg_extraction::{
    extract_kwarg, extract_mixed_args, ArgExtractor, ValueExtractor,
};
use carmen_lang::interpreter::Value;

fn dummy_span() -> Span {
    Span {
        start: Default::default(),
        end: Default::default(),
    }
}

#[cfg(test)]
mod value_extractor_tests {
    use super::*;

    #[test]
    fn test_extract_f64() {
        assert_eq!(Value::Number(123.0).extract(), Some(123.0));
        assert_eq!(Value::String("test".to_string()).extract(), None::<f64>);
    }

    #[test]
    fn test_extract_bool() {
        assert_eq!(Value::Boolean(true).extract(), Some(true));
        assert_eq!(Value::Nil.extract(), Some(false));
        assert_eq!(Value::Number(0.0).extract(), Some(false));
        assert_eq!(Value::Number(1.0).extract(), Some(true));
    }

    #[test]
    fn test_extract_pitch_class() {
        let pc = PitchClass::new(5);
        assert_eq!(Value::PitchClass(pc).extract(), Some(pc));
        assert_eq!(Value::Number(7.0).extract(), Some(PitchClass::new(7)));
        assert_eq!(Value::Number(12.0).extract(), None::<PitchClass>);
    }

    #[test]
    fn test_extract_pitch() {
        let p = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        assert_eq!(Value::Pitch(p).extract(), Some(p));
        assert_eq!(
            Value::PitchClass(PitchClass::new(0)).extract(),
            None::<Pitch>
        );
    }
}

#[cfg(test)]
mod arg_extractor_tests {
    use super::*;

    #[test]
    fn test_extract_single_arg() {
        let args = vec![Value::Number(1.0), Value::Number(2.0)];
        let extracted = ArgExtractor::<1>::extract::<f64>(&args, &dummy_span()).unwrap();
        assert_eq!(extracted, vec![1.0]);
    }

    #[test]
    fn test_extract_multiple_args() {
        let args = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)];
        let extracted = ArgExtractor::<2>::extract::<f64>(&args, &dummy_span()).unwrap();
        assert_eq!(extracted, vec![1.0, 2.0]);
    }

    #[test]
    fn test_extract_from_tuple() {
        let args = vec![Value::Tuple(vec![Value::Number(1.0), Value::Number(2.0)])];
        let extracted = ArgExtractor::<2>::extract::<f64>(&args, &dummy_span()).unwrap();
        assert_eq!(extracted, vec![1.0, 2.0]);
    }

    #[test]
    fn test_extract_from_chord() {
        let chord = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(2),
                    octave: 4,
                },
            ],
        };
        let args = vec![Value::Chord(chord)];
        let extracted = ArgExtractor::<2>::extract::<Pitch>(&args, &dummy_span()).unwrap();
        assert_eq!(extracted.len(), 2);
        assert_eq!(extracted[0].pitch_class.0, 0);
        assert_eq!(extracted[1].pitch_class.0, 2);
    }

    #[test]
    fn test_not_enough_args() {
        let args = vec![Value::Number(1.0)];
        let err = ArgExtractor::<2>::extract::<f64>(&args, &dummy_span()).unwrap_err();
        assert!(matches!(err.source, ErrorSource::Runtime(_)));
    }

    #[test]
    fn test_invalid_arg_type() {
        let args = vec![Value::String("hello".to_string())];
        let err = ArgExtractor::<1>::extract::<f64>(&args, &dummy_span()).unwrap_err();
        assert!(matches!(err.source, ErrorSource::Runtime(_)));
    }
}

#[cfg(test)]
mod mixed_arg_extraction_tests {
    use super::*;

    #[test]
    fn test_extract_single_value() {
        let args = vec![Value::Number(1.0)];
        let extracted = extract_mixed_args(&args, &dummy_span()).unwrap();
        assert_eq!(extracted, vec![Value::Number(1.0)]);
    }

    #[test]
    fn test_extract_from_tuple() {
        let args = vec![Value::Tuple(vec![
            Value::Number(1.0),
            Value::String("test".to_string()),
        ])];
        let extracted = extract_mixed_args(&args, &dummy_span()).unwrap();
        assert_eq!(
            extracted,
            vec![Value::Number(1.0), Value::String("test".to_string())]
        );
    }

    #[test]
    fn test_extract_from_chord() {
        let chord = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(2),
                    octave: 4,
                },
            ],
        };
        let args = vec![Value::Chord(chord)];
        let extracted = extract_mixed_args(&args, &dummy_span()).unwrap();
        assert_eq!(extracted.len(), 2);
        assert!(matches!(extracted[0], Value::Pitch(_)));
        assert!(matches!(extracted[1], Value::Pitch(_)));
    }

    #[test]
    fn test_extract_multiple_direct_args() {
        let args = vec![Value::Number(1.0), Value::String("test".to_string())];
        let extracted = extract_mixed_args(&args, &dummy_span()).unwrap();
        assert_eq!(
            extracted,
            vec![Value::Number(1.0), Value::String("test".to_string())]
        );
    }
}

#[cfg(test)]
mod kwarg_extraction_tests {
    use super::*;

    #[test]
    fn test_extract_existing_kwarg() {
        let named_args = vec![
            ("name".to_string(), Value::String("Alice".to_string())),
            ("age".to_string(), Value::Number(30.0)),
        ];
        let age = extract_kwarg(&named_args, "age", 25.0);
        assert_eq!(age, 30.0);
    }

    #[test]
    fn test_extract_non_existing_kwarg_with_default() {
        let named_args = vec![("name".to_string(), Value::String("Alice".to_string()))];
        let age = extract_kwarg(&named_args, "age", 25.0);
        assert_eq!(age, 25.0);
    }

    #[test]
    fn test_extract_kwarg_different_type() {
        let named_args = vec![("hello".to_string(), Value::Boolean(true))];
        let is_admin = extract_kwarg(&named_args, "hello", false);
        assert!(is_admin);
    }
}

#[cfg(test)]
mod macro_tests {
    use super::*;

    #[test]
    fn test_extract_args_macro() {
        let args = vec![Value::Number(1.0), Value::Number(2.0)];
        let extracted = extract_args!(&args, &dummy_span(), 2, f64).unwrap();
        assert_eq!(extracted, vec![1.0, 2.0]);
    }

    #[test]
    fn test_extract_two_macro() -> Result<(), Box<dyn std::error::Error>> {
        let args = vec![Value::Number(1.0), Value::Number(2.0)];
        let (a, b) = extract_two!(&args, &dummy_span(), f64);
        assert_eq!(a, 1.0);
        assert_eq!(b, 2.0);
        Ok(())
    }

    #[test]
    fn test_extract_three_macro() -> Result<(), Box<dyn std::error::Error>> {
        let args = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)];
        let (a, b, c) = extract_three!(&args, &dummy_span(), f64);
        assert_eq!(a, 1.0);
        assert_eq!(b, 2.0);
        assert_eq!(c, 3.0);
        Ok(())
    }
}

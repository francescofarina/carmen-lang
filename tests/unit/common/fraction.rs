use carmen_lang::common::fraction::{Fraction, TupletInfo};
use std::str::FromStr;

#[cfg(test)]
mod fraction_tests {
    use super::*;

    #[test]
    fn test_new_fraction() {
        let f = Fraction::new(1, 2);
        assert_eq!(f.numerator, 1);
        assert_eq!(f.denominator, 2);

        let f_simplified = Fraction::new(2, 4);
        assert_eq!(f_simplified.numerator, 1);
        assert_eq!(f_simplified.denominator, 2);

        let f_whole = Fraction::new(5, 1);
        assert_eq!(f_whole.numerator, 5);
        assert_eq!(f_whole.denominator, 1);
    }

    #[test]
    #[should_panic]
    fn test_new_fraction_zero_denominator() {
        Fraction::new(1, 0);
    }

    #[test]
    fn test_to_f64() {
        let f = Fraction::new(1, 2);
        assert_eq!(f.to_f64(), 0.5);

        let f_zero = Fraction::new(0, 5);
        assert_eq!(f_zero.to_f64(), 0.0);
    }

    #[test]
    fn test_is_standard_binary() {
        // Standard powers of two
        assert!(Fraction::new(1, 1).is_standard_binary());
        assert!(Fraction::new(1, 2).is_standard_binary());
        assert!(Fraction::new(1, 4).is_standard_binary());
        assert!(Fraction::new(1, 8).is_standard_binary());
        assert!(Fraction::new(1, 16).is_standard_binary());

        // Dotted notes (numerator is 2^k - 1)
        assert!(Fraction::new(3, 8).is_standard_binary()); // Dotted quarter
        assert!(Fraction::new(7, 16).is_standard_binary()); // Double-dotted eighth

        // Not standard binary
        assert!(!Fraction::new(2, 3).is_standard_binary());
        assert!(!Fraction::new(1, 3).is_standard_binary());
        assert!(!Fraction::new(3, 5).is_standard_binary());
        assert!(!Fraction::new(0, 4).is_standard_binary());
    }

    #[test]
    fn test_with_dots() {
        let quarter = Fraction::new(1, 4);

        let dotted_quarter = quarter.with_dots(1);
        assert_eq!(dotted_quarter, Fraction::new(3, 8));

        let double_dotted_quarter = quarter.with_dots(2);
        assert_eq!(double_dotted_quarter, Fraction::new(7, 16));

        let no_dots = quarter.with_dots(0);
        assert_eq!(no_dots, quarter);
    }

    #[test]
    fn test_to_tuplet_info() {
        // Standard durations should return None
        assert!(Fraction::new(1, 4).to_tuplet_info().is_none());
        assert!(Fraction::new(3, 8).to_tuplet_info().is_none());

        // Triplet
        let triplet = Fraction::new(1, 6); // Quarter note triplet
        let info = triplet.to_tuplet_info().unwrap();
        assert_eq!(
            info,
            TupletInfo {
                tuplet_number: 3,
                normal_number: 2,
                base_duration: 4
            }
        );

        // Quintuplet
        let quintuplet = Fraction::new(1, 10); // Eighth note quintuplet
        let info = quintuplet.to_tuplet_info().unwrap();
        assert_eq!(
            info,
            TupletInfo {
                tuplet_number: 5,
                normal_number: 4,
                base_duration: 8
            }
        );
    }

    #[test]
    fn test_reciprocal() {
        let f = Fraction::new(2, 3);
        assert_eq!(f.reciprocal(), Fraction::new(3, 2));
    }

    #[test]
    #[should_panic]
    fn test_reciprocal_of_zero() {
        Fraction::new(0, 5).reciprocal();
    }

    #[test]
    fn test_fraction_ordering() {
        assert!(Fraction::new(1, 2) > Fraction::new(1, 3));
        assert!(Fraction::new(1, 2) < Fraction::new(3, 4));
        assert_eq!(Fraction::new(1, 2), Fraction::new(2, 4));
    }

    #[test]
    fn test_fraction_from_str() {
        assert_eq!(Fraction::from_str("1/2"), Ok(Fraction::new(1, 2)));
        assert_eq!(Fraction::from_str("3"), Ok(Fraction::new(3, 1)));
        assert!(Fraction::from_str("1/0").is_err());
        assert!(Fraction::from_str("a/b").is_err());
        assert!(Fraction::from_str("1/b").is_err());
        assert!(Fraction::from_str("a/2").is_err());
    }

    #[test]
    fn test_fraction_arithmetic() {
        let a = Fraction::new(1, 2);
        let b = Fraction::new(1, 3);

        assert_eq!(a + b, Fraction::new(5, 6));
        assert_eq!(a - b, Fraction::new(1, 6));
        assert_eq!(a * b, Fraction::new(1, 6));
        assert_eq!(a * 3, Fraction::new(3, 2));
    }

    #[test]
    fn test_fraction_sum() {
        let fractions = vec![
            Fraction::new(1, 2),
            Fraction::new(1, 3),
            Fraction::new(1, 6),
        ];
        let sum: Fraction = fractions.into_iter().sum();
        assert_eq!(sum, Fraction::new(1, 1));
    }
}

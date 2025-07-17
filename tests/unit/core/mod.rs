use carmen_lang::common::fraction::Fraction;
use carmen_lang::core::traits::{Interval, Invert, Transpose};
use carmen_lang::core::{
    Attribute, Chord, Clef, ContextChange, Duration, Dynamic, EventContent, KeySignature,
    MetadataKey, MetadataValue, Movement, MusicalEvent, Part, Pitch, PitchClass, PitchClassSet,
    Score, Staff, Timeline, Voice,
};
use std::collections::HashSet;
use std::str::FromStr;

#[cfg(test)]
mod pitch_class_tests {
    use super::*;

    #[test]
    fn test_new_pitch_class() {
        assert_eq!(PitchClass::new(0).0, 0);
        assert_eq!(PitchClass::new(11).0, 11);
        assert_eq!(PitchClass::new(12).0, 0); // Wraps around
        assert_eq!(PitchClass::new(13).0, 1);
        assert_eq!(PitchClass::new(255).0, 3); // Test large values
    }

    #[test]
    fn test_from_note_name() {
        assert_eq!(
            PitchClass::from_note_name("c", 0).unwrap(),
            PitchClass::new(0)
        );
        assert_eq!(
            PitchClass::from_note_name("C", 0).unwrap(),
            PitchClass::new(0)
        );
        assert_eq!(
            PitchClass::from_note_name("d", 0).unwrap(),
            PitchClass::new(2)
        );
        assert_eq!(
            PitchClass::from_note_name("e", 0).unwrap(),
            PitchClass::new(4)
        );
        assert_eq!(
            PitchClass::from_note_name("f", 0).unwrap(),
            PitchClass::new(5)
        );
        assert_eq!(
            PitchClass::from_note_name("g", 0).unwrap(),
            PitchClass::new(7)
        );
        assert_eq!(
            PitchClass::from_note_name("a", 0).unwrap(),
            PitchClass::new(9)
        );
        assert_eq!(
            PitchClass::from_note_name("b", 0).unwrap(),
            PitchClass::new(11)
        );

        // Test with accidentals
        assert_eq!(
            PitchClass::from_note_name("c", 1).unwrap(),
            PitchClass::new(1)
        ); // C#
        assert_eq!(
            PitchClass::from_note_name("c", -1).unwrap(),
            PitchClass::new(11)
        ); // Cb
        assert_eq!(
            PitchClass::from_note_name("f", 1).unwrap(),
            PitchClass::new(6)
        ); // F#
        assert_eq!(
            PitchClass::from_note_name("b", -1).unwrap(),
            PitchClass::new(10)
        ); // Bb

        // Test invalid note names
        assert!(PitchClass::from_note_name("h", 0).is_err());
        assert!(PitchClass::from_note_name("x", 0).is_err());
    }

    #[test]
    fn test_interval_to() {
        let c = PitchClass::new(0);
        let d = PitchClass::new(2);
        let g = PitchClass::new(7);

        assert_eq!(c.interval_to(&d), 2);
        assert_eq!(d.interval_to(&c), 10);
        assert_eq!(c.interval_to(&g), 7);
        assert_eq!(g.interval_to(&c), 5);
        assert_eq!(c.interval_to(&c), 0);
    }

    #[test]
    fn test_interval_class_to() {
        let c = PitchClass::new(0);
        let d = PitchClass::new(2);
        let g = PitchClass::new(7);

        assert_eq!(c.interval_class_to(&d), 2);
        assert_eq!(d.interval_class_to(&c), 2); // Interval class is symmetrical
        assert_eq!(c.interval_class_to(&g), 5);
        assert_eq!(g.interval_class_to(&c), 5);
        assert_eq!(c.interval_class_to(&c), 0);
    }

    #[test]
    fn test_transpose() {
        let c = PitchClass::new(0);
        assert_eq!(c.transpose(0), c);
        assert_eq!(c.transpose(1), PitchClass::new(1));
        assert_eq!(c.transpose(12), c);
        assert_eq!(c.transpose(-1), PitchClass::new(11));
        assert_eq!(c.transpose(-12), c);

        let f_sharp = PitchClass::new(6);
        assert_eq!(f_sharp.transpose(6), c);
        assert_eq!(f_sharp.transpose(-6), c);
    }

    #[test]
    fn test_invert() {
        let c = PitchClass::new(0);
        let d = PitchClass::new(2);
        let e = PitchClass::new(4);

        // Invert around C (axis = 0)
        assert_eq!(c.invert(&c), c);
        assert_eq!(d.invert(&c), PitchClass::new(10)); // D inverts to Bb
        assert_eq!(e.invert(&c), PitchClass::new(8)); // E inverts to Ab

        // Invert around different axis
        let f_sharp = PitchClass::new(6);
        assert_eq!(c.invert(&f_sharp), f_sharp);
        assert_eq!(d.invert(&f_sharp), PitchClass::new(4));
    }
}

#[cfg(test)]
mod pitch_tests {
    use super::*;

    #[test]
    fn test_new_pitch() {
        let pitch = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        assert_eq!(pitch.pitch_class.0, 0);
        assert_eq!(pitch.octave, 4);
    }

    #[test]
    fn test_from_midi() {
        let middle_c = Pitch::from_midi(60);
        assert_eq!(middle_c.pitch_class.0, 0);
        assert_eq!(middle_c.octave, 4);

        let a_440 = Pitch::from_midi(69);
        assert_eq!(a_440.pitch_class.0, 9);
        assert_eq!(a_440.octave, 4);

        // Test boundary cases
        let lowest = Pitch::from_midi(0);
        assert_eq!(lowest.pitch_class.0, 0);
        assert_eq!(lowest.octave, -1);

        let highest = Pitch::from_midi(127);
        assert_eq!(highest.pitch_class.0, 7);
        assert_eq!(highest.octave, 9);
    }

    #[test]
    fn test_to_midi() {
        let middle_c = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        assert_eq!(middle_c.to_midi(), 60);

        let a_440 = Pitch {
            pitch_class: PitchClass::new(9),
            octave: 4,
        };
        assert_eq!(a_440.to_midi(), 69);

        // Test clamping
        let very_low = Pitch {
            pitch_class: PitchClass::new(0),
            octave: -1,
        };
        assert_eq!(very_low.to_midi(), 0);

        let very_high = Pitch {
            pitch_class: PitchClass::new(7),
            octave: 10,
        };
        assert_eq!(very_high.to_midi(), 127);
    }

    #[test]
    fn test_from_note_name() {
        let middle_c = Pitch::from_note_name("c", 0, 4);
        assert!(middle_c.is_ok());
        let pitch = middle_c.unwrap();
        assert_eq!(pitch.pitch_class.0, 0);
        assert_eq!(pitch.octave, 4);

        let f_sharp_3 = Pitch::from_note_name("f", 1, 3);
        assert!(f_sharp_3.is_ok());
        let pitch = f_sharp_3.unwrap();
        assert_eq!(pitch.pitch_class.0, 6);
        assert_eq!(pitch.octave, 3);

        assert!(Pitch::from_note_name("x", 0, 4).is_err());
    }

    #[test]
    fn test_pitch_interval() {
        let c4 = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let c5 = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 5,
        };
        let d4 = Pitch {
            pitch_class: PitchClass::new(2),
            octave: 4,
        };

        assert_eq!(c4.interval_to(&c5), 12);
        assert_eq!(c5.interval_to(&c4), -12);
        assert_eq!(c4.interval_to(&d4), 2);
        assert_eq!(d4.interval_to(&c4), -2);
    }

    #[test]
    fn test_pitch_transpose() {
        let c4 = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let d4 = c4.transpose(2);
        assert_eq!(d4.pitch_class.0, 2);
        assert_eq!(d4.octave, 4);

        let c5 = c4.transpose(12);
        assert_eq!(c5.pitch_class.0, 0);
        assert_eq!(c5.octave, 5);

        let b3 = c4.transpose(-1);
        assert_eq!(b3.pitch_class.0, 11);
        assert_eq!(b3.octave, 3);
    }
}

#[cfg(test)]
mod pitch_class_set_tests {
    use super::*;

    #[test]
    fn test_new_pitch_class_set() {
        let pcs = PitchClassSet::new(vec![
            PitchClass::new(0),
            PitchClass::new(4),
            PitchClass::new(7),
        ]);
        assert_eq!(pcs.classes.len(), 3);
        assert!(pcs.classes.contains(&PitchClass::new(0)));
        assert!(pcs.classes.contains(&PitchClass::new(4)));
        assert!(pcs.classes.contains(&PitchClass::new(7)));
    }

    #[test]
    fn test_from_u8_values() {
        let pcs = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        assert_eq!(pcs.classes.len(), 3);
        assert!(pcs.classes.contains(&PitchClass::new(0)));
        assert!(pcs.classes.contains(&PitchClass::new(4)));
        assert!(pcs.classes.contains(&PitchClass::new(7)));
    }

    #[test]
    fn test_from_pitches() {
        let pitches = vec![
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            },
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 4,
            },
            Pitch {
                pitch_class: PitchClass::new(7),
                octave: 5,
            },
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 3,
            }, // Duplicate pitch class
        ];
        let pcs = PitchClassSet::from_pitches(&pitches);
        assert_eq!(pcs.classes.len(), 3); // Duplicates removed
    }

    #[test]
    fn test_normal_form() {
        // Test C major triad (0, 4, 7)
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let normal = c_major.normal_form();
        assert_eq!(normal, PitchClassSet::from_u8_values(vec![0, 4, 7]));

        // Test different ordering of same set
        let scrambled = PitchClassSet::from_u8_values(vec![4, 7, 0]);
        let scrambled_normal = scrambled.normal_form();
        assert_eq!(normal, scrambled_normal);

        // Test chromatic cluster (0, 1, 2)
        let cluster = PitchClassSet::from_u8_values(vec![0, 2, 1]);
        let cluster_normal = cluster.normal_form();
        assert_eq!(cluster_normal, PitchClassSet::from_u8_values(vec![0, 1, 2]));

        // Test empty set
        let empty = PitchClassSet::new(vec![]);
        assert_eq!(empty.normal_form().classes.len(), 0);
    }

    #[test]
    fn test_prime_form() {
        // Test C major triad
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        // Test that different transpositions give same prime form
        let d_major = PitchClassSet::from_u8_values(vec![2, 6, 9]);
        assert_eq!(c_major.prime_form(), d_major.prime_form());

        // Test empty set
        let empty = PitchClassSet::new(vec![]);
        assert_eq!(empty.prime_form().classes.len(), 0);
    }

    #[test]
    fn test_interval_class_vector() {
        // Test C major triad (0, 4, 7)
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let icv = c_major.interval_class_vector();
        // C major triad should have: 1 major third (4), 1 perfect fifth (5), 1 perfect fourth (3)
        // Converted to interval classes: ic3=1, ic4=1, ic5=1
        assert_eq!(icv[2], 1); // ic3
        assert_eq!(icv[3], 1); // ic4
        assert_eq!(icv[4], 1); // ic5
        assert_eq!(icv[0] + icv[1] + icv[5], 0); // Other interval classes should be 0

        // Test chromatic scale (all pitch classes)
        let chromatic = PitchClassSet::from_u8_values((0..12).collect());
        let chromatic_icv = chromatic.interval_class_vector();
        // Each interval class should appear the same number of times
        for &count in &chromatic_icv {
            assert!(count > 0);
        }

        // Test empty set
        let empty = PitchClassSet::new(vec![]);
        let empty_icv = empty.interval_class_vector();
        assert_eq!(empty_icv, [0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn test_transpose_pitch_class_set() {
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let d_major = c_major.transpose(2);

        let expected: HashSet<PitchClass> =
            vec![2, 6, 9].into_iter().map(PitchClass::new).collect();
        assert_eq!(d_major.classes, expected);

        // Test negative transposition
        let bb_major = c_major.transpose(-2);
        let expected_bb: HashSet<PitchClass> =
            vec![10, 2, 5].into_iter().map(PitchClass::new).collect();
        assert_eq!(bb_major.classes, expected_bb);
    }

    #[test]
    fn test_invert_pitch_class_set() {
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let inverted = c_major.invert(&PitchClass::new(0));

        // Inversion of C major around C should give C, Ab, F (0, 8, 5)
        let expected: HashSet<PitchClass> =
            vec![0, 8, 5].into_iter().map(PitchClass::new).collect();
        assert_eq!(inverted.classes, expected);
    }

    #[test]
    fn test_set_class_generation() {
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let set_class = c_major.set_class();

        // Should contain 24 members (12 transpositions + 12 inversions, some may overlap)
        assert!(!set_class.is_empty());
        assert!(set_class.len() <= 24);

        // All members should be related by transposition/inversion
        for member in &set_class {
            assert_eq!(member.classes.len(), 3);
        }
    }

    #[test]
    fn test_pitch_class_set_ordering() {
        let set1 = PitchClassSet::from_u8_values(vec![0, 1, 2]);
        let set2 = PitchClassSet::from_u8_values(vec![0, 2, 4]);

        // Test that sets can be ordered consistently
        assert_ne!(set1.cmp(&set2), std::cmp::Ordering::Equal);

        // Test that same sets are equal
        let set1_copy = PitchClassSet::from_u8_values(vec![0, 1, 2]);
        assert_eq!(set1.cmp(&set1_copy), std::cmp::Ordering::Equal);
    }
}

#[cfg(test)]
mod duration_tests {
    use super::*;

    #[test]
    fn test_duration_constructors() {
        let whole = Duration::whole();
        assert_eq!(whole.fraction.to_f64(), 1.0);

        let half = Duration::half();
        assert_eq!(half.fraction.to_f64(), 0.5);

        let quarter = Duration::quarter();
        assert_eq!(quarter.fraction.to_f64(), 0.25);

        let eighth = Duration::eighth();
        assert_eq!(eighth.fraction.to_f64(), 0.125);

        let sixteenth = Duration::sixteenth();
        assert_eq!(sixteenth.fraction.to_f64(), 0.0625);
    }

    #[test]
    fn test_from_fraction() {
        let three_quarters = Duration::from_fraction(3, 4);
        assert_eq!(three_quarters.fraction.to_f64(), 0.75);

        let five_eighths = Duration::from_fraction(5, 8);
        assert_eq!(five_eighths.fraction.to_f64(), 0.625);
    }

    #[test]
    fn test_with_dots() {
        let quarter = Duration::quarter();
        let dotted_quarter = quarter.with_dots(1);
        assert_eq!(dotted_quarter.fraction.to_f64(), 0.375); // 0.25 + 0.125

        let double_dotted_quarter = quarter.with_dots(2);
        assert_eq!(double_dotted_quarter.fraction.to_f64(), 0.4375); // 0.25 + 0.125 + 0.0625

        let no_dots = quarter.with_dots(0);
        assert_eq!(no_dots.fraction.to_f64(), quarter.fraction.to_f64());
    }

    #[test]
    fn test_multiply() {
        let quarter = Duration::quarter();
        let triplet_quarter = &quarter * 2_u32;
        assert!((triplet_quarter.fraction.to_f64() - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn test_add() {
        let quarter = Duration::quarter();
        let eighth = Duration::eighth();
        let sum = &quarter + &eighth;
        assert_eq!(sum.fraction.to_f64(), 0.375);
    }

    #[test]
    fn test_to_fractional_string() {
        assert_eq!(Duration::whole().to_fractional_string(), "1/1");
        assert_eq!(Duration::half().to_fractional_string(), "1/2");
        assert_eq!(Duration::quarter().to_fractional_string(), "1/4");
        assert_eq!(Duration::eighth().to_fractional_string(), "1/8");
        assert_eq!(Duration::sixteenth().to_fractional_string(), "1/16");

        let custom = Duration::from_fraction(3, 4);
        assert_eq!(custom.to_fractional_string(), "3/4");
    }

    #[test]
    fn test_is_tuplet() {
        // Standard durations are not tuplets
        assert!(!Duration::whole().is_tuplet());
        assert!(!Duration::half().is_tuplet());
        assert!(!Duration::quarter().is_tuplet());
        assert!(!Duration::eighth().is_tuplet());
        assert!(!Duration::sixteenth().is_tuplet());
        assert!(!Duration::from_fraction(1, 32).is_tuplet());
        assert!(!Duration::from_fraction(1, 64).is_tuplet());

        // Dotted notes are not tuplets
        assert!(!Duration::quarter().with_dots(1).is_tuplet());
        assert!(!Duration::eighth().with_dots(2).is_tuplet());

        // Non-power-of-two denominators are tuplets
        assert!(Duration::from_fraction(1, 3).is_tuplet()); // Triplet of half notes
        assert!(Duration::from_fraction(1, 6).is_tuplet()); // Triplet of quarter notes
        assert!(Duration::from_fraction(1, 12).is_tuplet()); // Triplet of eighth notes
        assert!(Duration::from_fraction(2, 3).is_tuplet()); // A tuplet of two half notes (duplet)
        assert!(Duration::from_fraction(1, 5).is_tuplet()); // Quintuplet
        assert!(Duration::from_fraction(1, 7).is_tuplet()); // Septuplet
    }

    #[test]
    fn test_to_tuplet_info() {
        // Standard durations should return None
        assert!(Duration::quarter().to_tuplet_info().is_none());
        assert!(Duration::half().with_dots(1).to_tuplet_info().is_none());

        // Tuplet durations should return Some
        assert!(Duration::from_fraction(1, 3).to_tuplet_info().is_some());
        assert!(Duration::from_fraction(1, 6).to_tuplet_info().is_some());
        assert!(Duration::from_fraction(1, 12).to_tuplet_info().is_some());
        assert!(Duration::from_fraction(1, 5).to_tuplet_info().is_some());
    }

    #[test]
    fn test_add_tuplets() {
        let eighth_triplet = Duration::from_fraction(1, 12);
        let quarter_triplet = Duration::from_fraction(1, 6);
        let sum = &eighth_triplet + &quarter_triplet;
        // 1/12 + 1/6 = 1/12 + 2/12 = 3/12 = 1/4
        assert_eq!(sum, Duration::quarter());
        assert!(!sum.is_tuplet());
    }

    #[test]
    fn test_multiply_tuplets() {
        let eighth_triplet = Duration::from_fraction(1, 12);
        let three_eighth_triplets = &eighth_triplet * 3;
        // 3 * 1/12 = 3/12 = 1/4
        assert_eq!(three_eighth_triplets, Duration::quarter());
        assert!(!three_eighth_triplets.is_tuplet());
    }
}

#[cfg(test)]
mod dynamic_tests {
    use super::*;

    #[test]
    fn test_dynamic_from_str() {
        assert_eq!(Dynamic::from_str("pppp").unwrap(), Dynamic::Pppp);
        assert_eq!(Dynamic::from_str("ppp").unwrap(), Dynamic::Ppp);
        assert_eq!(Dynamic::from_str("pp").unwrap(), Dynamic::Pp);
        assert_eq!(Dynamic::from_str("p").unwrap(), Dynamic::P);
        assert_eq!(Dynamic::from_str("mp").unwrap(), Dynamic::Mp);
        assert_eq!(Dynamic::from_str("mf").unwrap(), Dynamic::Mf);
        assert_eq!(Dynamic::from_str("f").unwrap(), Dynamic::F);
        assert_eq!(Dynamic::from_str("ff").unwrap(), Dynamic::Ff);
        assert_eq!(Dynamic::from_str("fff").unwrap(), Dynamic::Fff);
        assert_eq!(Dynamic::from_str("ffff").unwrap(), Dynamic::Ffff);

        assert!(Dynamic::from_str("invalid").is_err());
    }

    #[test]
    fn test_dynamic_to_velocity() {
        assert_eq!(Dynamic::Pppp.to_velocity(), 8);
        assert_eq!(Dynamic::Ppp.to_velocity(), 16);
        assert_eq!(Dynamic::Pp.to_velocity(), 32);
        assert_eq!(Dynamic::P.to_velocity(), 48);
        assert_eq!(Dynamic::Mp.to_velocity(), 64);
        assert_eq!(Dynamic::Mf.to_velocity(), 80);
        assert_eq!(Dynamic::F.to_velocity(), 96);
        assert_eq!(Dynamic::Ff.to_velocity(), 112);
        assert_eq!(Dynamic::Fff.to_velocity(), 120);
        assert_eq!(Dynamic::Ffff.to_velocity(), 127);
        assert_eq!(Dynamic::Velocity(42).to_velocity(), 42);
    }
}

#[cfg(test)]
mod attribute_tests {
    use super::*;

    #[test]
    fn test_attribute_from_str() {
        assert_eq!(
            Attribute::from_str("staccato").unwrap(),
            Attribute::Staccato
        );
        assert_eq!(Attribute::from_str("accent").unwrap(), Attribute::Accent);
        assert_eq!(Attribute::from_str("tenuto").unwrap(), Attribute::Tenuto);

        assert!(Attribute::from_str("invalid").is_err());
    }
}

#[cfg(test)]
mod chord_tests {
    use super::*;

    #[test]
    fn test_new_chord() {
        let pitches = vec![
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            }, // C4
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 4,
            }, // E4
            Pitch {
                pitch_class: PitchClass::new(7),
                octave: 4,
            }, // G4
        ];
        let chord = Chord {
            pitches: pitches.clone(),
        };
        assert_eq!(chord.pitches, pitches);
    }

    #[test]
    fn test_to_pitch_class_set() {
        let pitches = vec![
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            }, // C4
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 4,
            }, // E4
            Pitch {
                pitch_class: PitchClass::new(7),
                octave: 5,
            }, // G5
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 3,
            }, // C3 (duplicate pitch class)
        ];
        let chord = Chord { pitches };
        let pcs = chord.to_pitch_class_set();

        assert_eq!(pcs.classes.len(), 3); // Should have 3 unique pitch classes
        assert!(pcs.classes.contains(&PitchClass::new(0)));
        assert!(pcs.classes.contains(&PitchClass::new(4)));
        assert!(pcs.classes.contains(&PitchClass::new(7)));
    }

    #[test]
    fn test_chord_transpose() {
        let c_major = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(4),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(7),
                    octave: 4,
                },
            ],
        };

        let d_major = c_major.transpose(2);
        assert_eq!(d_major.pitches[0].pitch_class.0, 2); // D
        assert_eq!(d_major.pitches[1].pitch_class.0, 6); // F#
        assert_eq!(d_major.pitches[2].pitch_class.0, 9); // A

        // Octaves should remain the same
        for pitch in &d_major.pitches {
            assert_eq!(pitch.octave, 4);
        }
    }
}

#[cfg(test)]
mod musical_event_tests {
    use carmen_lang::common::fraction::Fraction;

    use super::*;

    #[test]
    fn test_note_event() {
        let pitch = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let duration = Duration::quarter();
        let event = MusicalEvent::note(pitch, duration);

        match event.content {
            EventContent::Note(p) => assert_eq!(p, pitch),
            _ => panic!("Expected Note content"),
        }
        assert_eq!(event.duration, duration);
        assert_eq!(event.offset, Fraction::new(0, 1));
        assert!(event.dynamic.is_none());
        assert!(event.attributes.is_empty());
    }

    #[test]
    fn test_chord_event() {
        let chord = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(4),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(7),
                    octave: 4,
                },
            ],
        };
        let duration = Duration::half();
        let event = MusicalEvent::chord(chord.clone(), duration);

        match event.content {
            EventContent::Chord(c) => assert_eq!(c, chord),
            _ => panic!("Expected Chord content"),
        }
    }

    #[test]
    fn test_rest_event() {
        let duration = Duration::whole();
        let event = MusicalEvent::rest(duration);

        match event.content {
            EventContent::Rest => {}
            _ => panic!("Expected Rest content"),
        }
        assert_eq!(event.duration, duration);
    }

    #[test]
    fn test_event_with_attributes() {
        let pitch = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let mut event = MusicalEvent::note(pitch, Duration::quarter())
            .with_attribute(Attribute::Staccato)
            .with_attribute(Attribute::Accent)
            .with_offset(Fraction::new(1, 2));
        event.set_dynamic(Dynamic::F);

        assert_eq!(event.dynamic, Some(Dynamic::F));
        assert_eq!(event.attributes.len(), 2);
        assert!(event.attributes.contains(&Attribute::Staccato));
        assert!(event.attributes.contains(&Attribute::Accent));
        assert_eq!(event.offset, Fraction::new(1, 2));
    }

    #[test]
    fn test_event_transpose() {
        let pitch = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let event = MusicalEvent::note(pitch, Duration::quarter()).with_dynamic(Dynamic::P);

        let transposed = event.transpose(2);

        match transposed.content {
            EventContent::Note(p) => {
                assert_eq!(p.pitch_class.0, 2);
                assert_eq!(p.octave, 4);
            }
            _ => panic!("Expected Note content"),
        }

        // Other attributes should be preserved
        assert_eq!(transposed.dynamic, Some(Dynamic::P));
        assert_eq!(transposed.duration, Duration::quarter());
    }

    #[test]
    fn test_chord_event_transpose() {
        let chord = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Pitch {
                    pitch_class: PitchClass::new(4),
                    octave: 4,
                },
            ],
        };
        let event = MusicalEvent::chord(chord, Duration::half());
        let transposed = event.transpose(3);

        match transposed.content {
            EventContent::Chord(c) => {
                assert_eq!(c.pitches[0].pitch_class.0, 3);
                assert_eq!(c.pitches[1].pitch_class.0, 7);
            }
            _ => panic!("Expected Chord content"),
        }
    }

    #[test]
    fn test_rest_transpose() {
        let rest = MusicalEvent::rest(Duration::quarter());
        let transposed = rest.transpose(5);

        match transposed.content {
            EventContent::Rest => {}
            _ => panic!("Expected Rest content to remain unchanged"),
        }
    }

    #[test]
    fn test_sequence_transpose() {
        let sequence = vec![
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Duration::quarter(),
            ),
            MusicalEvent::rest(Duration::quarter()),
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(2),
                    octave: 4,
                },
                Duration::quarter(),
            ),
        ];

        let event = MusicalEvent {
            content: EventContent::Sequence(sequence),
            duration: Duration::whole(),
            dynamic: None,
            attributes: Vec::new(),
            offset: Fraction::new(0, 1),
        };

        let transposed = event.transpose(2);

        match transposed.content {
            EventContent::Sequence(events) => {
                assert_eq!(events.len(), 3);

                // First event: C4 -> D4
                match &events[0].content {
                    EventContent::Note(p) => assert_eq!(p.pitch_class.0, 2),
                    _ => panic!("Expected Note"),
                }

                // Second event: Rest should remain Rest
                match &events[1].content {
                    EventContent::Rest => {}
                    _ => panic!("Expected Rest"),
                }

                // Third event: D4 -> E4
                match &events[2].content {
                    EventContent::Note(p) => assert_eq!(p.pitch_class.0, 4),
                    _ => panic!("Expected Note"),
                }
            }
            _ => panic!("Expected Sequence content"),
        }
    }
}

#[cfg(test)]
mod clef_tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_clef_from_str() {
        assert_eq!(Clef::from_str("treble").unwrap(), Clef::Treble);
        assert_eq!(Clef::from_str("TREBLE").unwrap(), Clef::Treble);
        assert_eq!(Clef::from_str("Treble").unwrap(), Clef::Treble);
        assert_eq!(Clef::from_str("bass").unwrap(), Clef::Bass);
        assert_eq!(Clef::from_str("alto").unwrap(), Clef::Alto);
        assert_eq!(Clef::from_str("tenor").unwrap(), Clef::Tenor);

        assert!(Clef::from_str("invalid").is_err());
    }
}

#[cfg(test)]
mod key_signature_tests {
    use super::*;

    #[test]
    fn test_key_signature_variants() {
        let c_major = KeySignature::Major(0);
        let g_major = KeySignature::Major(1);
        let a_minor = KeySignature::Minor(0);
        let custom = KeySignature::Custom(vec![1, 3, 6, 8, 10]);

        // Test that they can be created and compared
        assert_ne!(c_major, g_major);
        assert_ne!(c_major, a_minor);
        assert_ne!(c_major, custom);

        // Test cloning
        let c_major_copy = c_major.clone();
        assert_eq!(c_major, c_major_copy);
    }
}

#[cfg(test)]
mod staff_tests {
    use super::*;

    #[test]
    fn test_new_staff() {
        let staff = Staff::new(1);
        assert_eq!(staff.number, 1);
        assert!(staff.voices.is_empty());
        assert!(staff.context_changes.is_empty());
    }

    #[test]
    fn test_staff_add_voice() {
        let event = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            },
            Duration::quarter(),
        );

        let voice = Voice::new().with_event(event.clone());
        let staff = Staff::new(1).add_voice(voice.clone());
        assert_eq!(staff.voices.len(), 1);
        assert_eq!(staff.voices[0], voice);
    }

    #[test]
    fn test_staff_total_duration() {
        let voice1 = Voice::new()
            .with_event(
                MusicalEvent::note(
                    Pitch {
                        pitch_class: PitchClass::new(0),
                        octave: 4,
                    },
                    Duration::quarter(),
                )
                .with_offset(Fraction::new(0, 1)),
            )
            .with_event(MusicalEvent::rest(Duration::half()).with_offset(Fraction::new(1, 4))); // ends at 0.25 + 0.5 = 0.75

        let voice2 = Voice::new().with_event(
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(2),
                    octave: 4,
                },
                Duration::whole(),
            )
            .with_offset(Fraction::new(0, 1)),
        ); // ends at 1.0

        let staff = Staff::new(1).add_voice(voice1).add_voice(voice2);

        // Duration is max of voice durations
        assert_eq!(staff.total_duration(), Fraction::new(1, 1));
    }

    #[test]
    fn test_staff_total_duration_with_sequence() {
        let sequence_events = vec![
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Duration::eighth(),
            )
            .with_offset(Fraction::new(0, 1)),
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(2),
                    octave: 4,
                },
                Duration::eighth(),
            )
            .with_offset(Fraction::new(1, 8)), // total duration of sequence is 0.25
        ];

        let sequence_event = MusicalEvent {
            content: EventContent::Sequence(sequence_events),
            duration: Duration::quarter(), // Container duration
            dynamic: None,
            attributes: Vec::new(),
            offset: Fraction::new(0, 1),
        };

        let voice = Voice::new().with_event(sequence_event);
        let staff = Staff::new(1).add_voice(voice);

        // The total duration is the offset of the last event + its duration
        assert_eq!(staff.total_duration(), Fraction::new(1, 4));
    }
}

#[cfg(test)]
mod part_tests {
    use super::*;

    #[test]
    fn test_new_part() {
        let part = Part::new(Some("Violin I".to_string()));
        assert_eq!(part.name, Some("Violin I".to_string()));
        assert!(part.instrument.is_none());
        assert!(part.channel.is_none());
        assert_eq!(part.offset, Fraction::new(0, 1));
        assert!(part.events.is_empty());
        assert!(part.staves.is_empty());
    }

    #[test]
    fn test_part_builder() {
        let event = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            },
            Duration::quarter(),
        );
        let staff = Staff::new(1);

        let part = Part::new(Some("Piano".to_string()))
            .with_instrument("Piano".to_string())
            .with_channel(1)
            .with_offset(Fraction::new(1, 2))
            .with_event(event.clone())
            .with_staff(staff.clone());

        assert_eq!(part.instrument, Some("Piano".to_string()));
        assert_eq!(part.channel, Some(1));
        assert_eq!(part.offset, Fraction::new(1, 2));
        assert_eq!(part.events.len(), 1);
        assert_eq!(part.staves.len(), 1);
    }

    #[test]
    fn test_part_all_events() {
        let main_event = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            },
            Duration::quarter(),
        );
        let staff_event = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 4,
            },
            Duration::half(),
        );

        let voice = Voice::new().with_event(staff_event.clone());
        let staff = Staff::new(1).add_voice(voice);
        let part = Part::new(None)
            .with_event(main_event.clone())
            .with_staff(staff);

        let all_events = part.all_events();
        assert_eq!(all_events.len(), 2);
        assert_eq!(*all_events[0], main_event);
        assert_eq!(*all_events[1], staff_event);
    }

    #[test]
    fn test_part_total_event_count() {
        let voice = Voice::new().with_event(MusicalEvent::rest(Duration::eighth()));
        let part = Part::new(None)
            .with_event(MusicalEvent::rest(Duration::quarter()))
            .with_event(MusicalEvent::rest(Duration::half()))
            .with_staff(Staff::new(1).add_voice(voice));

        assert_eq!(part.total_event_count(), 3);
    }

    #[test]
    fn test_part_total_duration() {
        let voice = Voice::new().with_event(MusicalEvent::rest(Duration::half()));
        let part = Part::new(None)
            .with_offset(Fraction::new(1, 1))
            .with_event(
                MusicalEvent::note(
                    Pitch {
                        pitch_class: PitchClass::new(0),
                        octave: 4,
                    },
                    Duration::quarter(),
                )
                .with_offset(Fraction::new(1, 2)),
            ) // Event at offset 0.5, duration 0.25, ends at 0.75
            .with_staff(
                Staff::new(1).add_voice(voice), // Duration 0.5
            );

        // Part offset (1.0) + max(main events end time (0.75), staff duration (0.5))
        // = 1.0 + 0.75 = 1.75
        assert_eq!(part.total_duration(), Fraction::new(7, 4));
    }
}

#[cfg(test)]
mod timeline_tests {
    use carmen_lang::core::ContextChange;

    use super::*;

    #[test]
    fn test_new_timeline() {
        let timeline = Timeline::default();
        assert!(timeline.parts.is_empty());
        assert!(timeline.context_changes.is_empty());
    }

    #[test]
    fn test_timeline_builder() {
        let part = Part::new(Some("Test".to_string()));
        let timeline = Timeline::default()
            .with_part(part.clone())
            .with_context_change(ContextChange {
                key: MetadataKey::Composer,
                value: MetadataValue::String("Test Composer".to_string()),
                time_offset: Fraction::new(0, 1),
            });

        assert_eq!(timeline.parts.len(), 1);
        assert_eq!(timeline.parts[0], part);
        assert_eq!(
            timeline.context_changes[0].value,
            MetadataValue::String("Test Composer".to_string())
        );
    }

    #[test]
    fn test_timeline_total_duration() {
        let part1 = Part::new(Some("Part 1".to_string()))
            .with_offset(Fraction::new(0, 1))
            .with_event(MusicalEvent::rest(Duration::whole()));

        let part2 = Part::new(Some("Part 2".to_string()))
            .with_offset(Fraction::new(1, 2))
            .with_event(MusicalEvent::rest(Duration::half()));

        let timeline = Timeline::default().with_parts(vec![part1, part2]);

        // Part1: 0.0 + 1.0 = 1.0
        // Part2: 0.5 + 0.5 = 1.0
        // Max = 1.0
        assert_eq!(timeline.total_duration(), Fraction::new(1, 1));
    }
}

#[cfg(test)]
mod movement_tests {
    use super::*;

    #[test]
    fn test_new_movement() {
        let movement = Movement::new(Some("Allegro".to_string()));
        assert_eq!(movement.name, Some("Allegro".to_string()));
        assert_eq!(movement.timeline, Timeline::default());
        assert!(movement.context_changes.is_empty());
    }

    #[test]
    fn test_movement_with_timeline() {
        let timeline = Timeline::default().with_part(Part::new(Some("Violin".to_string())));

        let movement = Movement::new(Some("Andante".to_string()))
            .with_timeline(timeline.clone())
            .with_context_change(ContextChange {
                key: MetadataKey::Tempo,
                value: MetadataValue::String("slow".to_string()),
                time_offset: Fraction::new(0, 1),
            });

        assert_eq!(movement.timeline, timeline);
        assert_eq!(
            movement.context_changes[0].value,
            MetadataValue::String("slow".to_string())
        );
    }

    #[test]
    fn test_movement_total_duration() {
        let timeline = Timeline::default()
            .with_part(Part::new(None).with_event(MusicalEvent::rest(Duration::whole())));

        let movement = Movement::new(None).with_timeline(timeline);
        assert_eq!(movement.total_duration(), Fraction::new(1, 1));
    }
}

#[cfg(test)]
mod score_tests {
    use super::*;

    #[test]
    fn test_new_score() {
        let score = Score::default();
        assert!(score.name.is_none());
        assert!(score.movements.is_empty());
        assert!(score.timeline.is_none());
        assert!(score.context_changes.is_empty());
    }

    #[test]
    fn test_score_with_timeline() {
        let timeline = Timeline::default().with_part(Part::new(Some("Solo".to_string())));

        let score = Score::default()
            .with_name("Test Piece".to_string())
            .with_timeline(timeline.clone());

        assert_eq!(score.name, Some("Test Piece".to_string()));
        assert_eq!(score.timeline, Some(timeline));
        assert!(!score.is_multi_movement());
    }

    #[test]
    fn test_score_with_movements() {
        let movement1 = Movement::new(Some("I. Allegro".to_string()));
        let movement2 = Movement::new(Some("II. Adagio".to_string()));

        let score = Score::default()
            .with_movement(movement1.clone())
            .with_movement(movement2.clone());

        assert_eq!(score.movements.len(), 2);
        assert_eq!(score.movements[0], movement1);
        assert_eq!(score.movements[1], movement2);
        assert!(score.is_multi_movement());
    }

    #[test]
    fn test_score_total_duration_single_timeline() {
        let timeline = Timeline::default()
            .with_part(Part::new(None).with_event(MusicalEvent::rest(Duration::whole())));

        let score = Score::default().with_timeline(timeline);
        assert_eq!(score.total_duration(), Fraction::new(1, 1));
    }

    #[test]
    fn test_score_total_duration_multi_movement() {
        let movement1 = Movement::new(None).with_timeline(
            Timeline::default()
                .with_part(Part::new(None).with_event(MusicalEvent::rest(Duration::whole()))),
        );

        let movement2 = Movement::new(None).with_timeline(
            Timeline::default()
                .with_part(Part::new(None).with_event(MusicalEvent::rest(Duration::half()))),
        );

        let score = Score::default()
            .with_movement(movement1)
            .with_movement(movement2);

        // Sum of movement durations: 1.0 + 0.5 = 1.5
        assert_eq!(score.total_duration(), Fraction::new(3, 2));
    }
}

//! Tests for inversion operations with both PitchClass and Pitch axes.
//!
//! These tests verify that musical objects can be inverted around different
//! types of axes, demonstrating both chromatic inversion (PitchClass) and
//! octave-preserving inversion (Pitch).

use carmen_lang::core::traits::Invert;
use carmen_lang::core::{Chord, MusicalEvent, Pitch, PitchClass, PitchClassSet};

mod pitch_class_inversion_tests {
    use super::*;

    #[test]
    fn test_pitch_class_invert_around_c() {
        let c = PitchClass::new(0); // C
        let e = PitchClass::new(4); // E
        let g = PitchClass::new(7); // G

        let axis = PitchClass::new(0); // C

        // C inverted around C should be C
        assert_eq!(c.invert(&axis), PitchClass::new(0));

        // E inverted around C should be Ab (8)
        assert_eq!(e.invert(&axis), PitchClass::new(8));

        // G inverted around C should be F (5)
        assert_eq!(g.invert(&axis), PitchClass::new(5));
    }

    #[test]
    fn test_pitch_class_invert_around_fs() {
        let c = PitchClass::new(0); // C
        let fs = PitchClass::new(6); // F#

        let axis = PitchClass::new(6); // F#

        // C inverted around F# should be F# (6 - 0 = 6)
        assert_eq!(c.invert(&axis), PitchClass::new(6));

        // F# inverted around F# should be F# (6 - 6 = 0)
        assert_eq!(fs.invert(&axis), PitchClass::new(0));
    }

    #[test]
    fn test_pitch_class_set_inversion() {
        // C major triad: C-E-G (0-4-7)
        let c_major = PitchClassSet::from_u8_values(vec![0, 4, 7]);
        let axis = PitchClass::new(0); // C

        let inverted = c_major.invert(&axis);

        // Should be C-Ab-F (0-8-5)
        let expected = PitchClassSet::from_u8_values(vec![0, 5, 8]);
        assert_eq!(inverted, expected);
    }

    #[test]
    fn test_chord_pitch_class_inversion() {
        let chord = Chord {
            pitches: vec![
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
            ],
        };

        let axis = PitchClass::new(0); // C
        let inverted = chord.invert(&axis);

        // Pitch classes should be inverted, octaves preserved
        assert_eq!(inverted.pitches[0].pitch_class, PitchClass::new(0)); // C
        assert_eq!(inverted.pitches[1].pitch_class, PitchClass::new(8)); // Ab
        assert_eq!(inverted.pitches[2].pitch_class, PitchClass::new(5)); // F

        // Octaves should be preserved
        assert_eq!(inverted.pitches[0].octave, 4);
        assert_eq!(inverted.pitches[1].octave, 4);
        assert_eq!(inverted.pitches[2].octave, 4);
    }
}

mod pitch_inversion_tests {
    use super::*;

    #[test]
    fn test_pitch_invert_around_c4() {
        let c4 = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        }; // C4 (MIDI 60)
        let e4 = Pitch {
            pitch_class: PitchClass::new(4),
            octave: 4,
        }; // E4 (MIDI 64)
        let g4 = Pitch {
            pitch_class: PitchClass::new(7),
            octave: 4,
        }; // G4 (MIDI 67)

        let axis = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        }; // C4

        // C4 inverted around C4 should be C4
        assert_eq!(c4.invert(&axis), c4);

        // E4 (MIDI 64) inverted around C4 (MIDI 60) should be Ab3 (MIDI 56)
        let inverted_e = e4.invert(&axis);
        assert_eq!(inverted_e.to_midi(), 56);
        assert_eq!(inverted_e.pitch_class, PitchClass::new(8)); // Ab
        assert_eq!(inverted_e.octave, 3);

        // G4 (MIDI 67) inverted around C4 (MIDI 60) should be F3 (MIDI 53)
        let inverted_g = g4.invert(&axis);
        assert_eq!(inverted_g.to_midi(), 53);
        assert_eq!(inverted_g.pitch_class, PitchClass::new(5)); // F
        assert_eq!(inverted_g.octave, 3);
    }

    #[test]
    fn test_pitch_invert_symmetric_around_axis() {
        let axis = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 5,
        }; // C5 (MIDI 72)
        let test_pitch = Pitch {
            pitch_class: PitchClass::new(7),
            octave: 5,
        }; // G5 (MIDI 79)

        // Invert twice should return to original
        let inverted_once = test_pitch.invert(&axis);
        let inverted_twice = inverted_once.invert(&axis);

        assert_eq!(test_pitch, inverted_twice);
    }

    #[test]
    fn test_chord_pitch_inversion() {
        let chord = Chord {
            pitches: vec![
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                }, // C4 (MIDI 60)
                Pitch {
                    pitch_class: PitchClass::new(4),
                    octave: 4,
                }, // E4 (MIDI 64)
                Pitch {
                    pitch_class: PitchClass::new(7),
                    octave: 5,
                }, // G5 (MIDI 79)
            ],
        };

        let axis = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        }; // C4 (MIDI 60)
        let inverted = chord.invert(&axis);

        // C4 around C4 = C4 (MIDI 60)
        assert_eq!(inverted.pitches[0].to_midi(), 60);

        // E4 around C4 = Ab3 (MIDI 56)
        assert_eq!(inverted.pitches[1].to_midi(), 56);
        assert_eq!(inverted.pitches[1].pitch_class, PitchClass::new(8)); // Ab

        // G5 around C4 = F2 (MIDI 41)
        assert_eq!(inverted.pitches[2].to_midi(), 41);
        assert_eq!(inverted.pitches[2].pitch_class, PitchClass::new(5)); // F
    }

    #[test]
    fn test_pitch_inversion_boundary_cases() {
        // Test with extreme MIDI values
        let low_note = Pitch::from_midi(0); // C-1
        let high_note = Pitch::from_midi(127); // G9
        let axis = Pitch::from_midi(60); // C4

        // Inversion should clamp to valid MIDI range
        let inverted_low = low_note.invert(&axis);
        let inverted_high = high_note.invert(&axis);

        assert!(inverted_low.to_midi() <= 127);
        assert!(inverted_high.to_midi() <= 127);
    }
}

mod musical_event_inversion_tests {
    use super::*;
    use carmen_lang::core::{Duration, EventContent};

    #[test]
    fn test_note_event_pitch_class_inversion() {
        let note = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 4,
            }, // E4
            Duration::quarter(),
        );

        let axis = PitchClass::new(0); // C
        let inverted = note.invert(&axis);

        if let EventContent::Note(pitch) = inverted.content {
            assert_eq!(pitch.pitch_class, PitchClass::new(8)); // Ab
            assert_eq!(pitch.octave, 4); // Octave preserved
        } else {
            panic!("Expected Note content");
        }
    }

    #[test]
    fn test_note_event_pitch_inversion() {
        let note = MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 4,
            }, // E4 (MIDI 64)
            Duration::quarter(),
        );

        let axis = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        }; // C4 (MIDI 60)
        let inverted = note.invert(&axis);

        if let EventContent::Note(pitch) = inverted.content {
            assert_eq!(pitch.to_midi(), 56); // Ab3
            assert_eq!(pitch.pitch_class, PitchClass::new(8)); // Ab
            assert_eq!(pitch.octave, 3);
        } else {
            panic!("Expected Note content");
        }
    }

    #[test]
    fn test_chord_event_inversion() {
        let chord = Chord {
            pitches: vec![
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
            ],
        };

        let chord_event = MusicalEvent::chord(chord, Duration::half());
        let axis = PitchClass::new(0); // C
        let inverted = chord_event.invert(&axis);

        if let EventContent::Chord(inverted_chord) = inverted.content {
            assert_eq!(inverted_chord.pitches[0].pitch_class, PitchClass::new(0)); // C
            assert_eq!(inverted_chord.pitches[1].pitch_class, PitchClass::new(8)); // Ab
            assert_eq!(inverted_chord.pitches[2].pitch_class, PitchClass::new(5));
        // F
        } else {
            panic!("Expected Chord content");
        }
    }

    #[test]
    fn test_rest_event_inversion() {
        let rest = MusicalEvent::rest(Duration::whole());
        let axis = PitchClass::new(0);
        let inverted = rest.invert(&axis);

        // Rest should remain unchanged
        assert_eq!(rest.content, inverted.content);
        assert_eq!(rest.duration, inverted.duration);
    }

    #[test]
    fn test_sequence_event_inversion() {
        let events = vec![
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(0),
                    octave: 4,
                },
                Duration::quarter(),
            ), // C4
            MusicalEvent::note(
                Pitch {
                    pitch_class: PitchClass::new(4),
                    octave: 4,
                },
                Duration::quarter(),
            ), // E4
        ];

        let sequence_event = MusicalEvent {
            duration: Duration::half(),
            content: EventContent::Sequence(events),
            dynamic: None,
            attributes: Vec::new(),
            offset: carmen_lang::common::fraction::Fraction::new(0, 1),
        };

        let axis = PitchClass::new(0); // C
        let inverted = sequence_event.invert(&axis);

        if let EventContent::Sequence(inverted_events) = inverted.content {
            assert_eq!(inverted_events.len(), 2);

            // Check first note (C4 -> C4)
            if let EventContent::Note(pitch) = &inverted_events[0].content {
                assert_eq!(pitch.pitch_class, PitchClass::new(0)); // C
            } else {
                panic!("Expected Note content");
            }

            // Check second note (E4 -> Ab4)
            if let EventContent::Note(pitch) = &inverted_events[1].content {
                assert_eq!(pitch.pitch_class, PitchClass::new(8)); // Ab
            } else {
                panic!("Expected Note content");
            }
        } else {
            panic!("Expected Sequence content");
        }
    }
}

mod trait_compatibility_tests {
    use super::*;

    #[test]
    fn test_default_pitch_class_axis() {
        // Test that the default generic parameter works
        let pc = PitchClass::new(4); // E
        let axis = PitchClass::new(0); // C

        // This should work with the default generic parameter
        let inverted: PitchClass = pc.invert(&axis);
        assert_eq!(inverted, PitchClass::new(8)); // Ab
    }

    #[test]
    fn test_explicit_axis_types() {
        let pitch = Pitch {
            pitch_class: PitchClass::new(4),
            octave: 4,
        }; // E4

        // Explicit PitchClass axis
        let pc_axis = PitchClass::new(0);
        let pc_inverted: Pitch = Invert::<PitchClass>::invert(&pitch, &pc_axis);
        assert_eq!(pc_inverted.pitch_class, PitchClass::new(8)); // Ab
        assert_eq!(pc_inverted.octave, 4); // Octave preserved

        // Explicit Pitch axis
        let pitch_axis = Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        };
        let pitch_inverted: Pitch = Invert::<Pitch>::invert(&pitch, &pitch_axis);
        assert_eq!(pitch_inverted.to_midi(), 56); // Ab3
    }

    #[test]
    fn test_musical_theory_examples() {
        // Example from trait documentation: C major triad inversion
        let c_major_chord = Chord {
            pitches: vec![
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
            ],
        };

        let axis = PitchClass::new(0); // C
        let inverted = c_major_chord.invert(&axis);

        // Should yield C-Ab-F
        assert_eq!(inverted.pitches[0].pitch_class, PitchClass::new(0)); // C
        assert_eq!(inverted.pitches[1].pitch_class, PitchClass::new(8)); // Ab
        assert_eq!(inverted.pitches[2].pitch_class, PitchClass::new(5)); // F
    }
}

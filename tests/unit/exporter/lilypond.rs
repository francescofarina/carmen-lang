use carmen_lang::{
    common::fraction::Fraction,
    core::{
        ContextChange, Duration, MetadataKey, MetadataValue, MusicalEvent, Part, Pitch, PitchClass,
        Score, Staff, Timeline, Voice,
    },
    exporter::{lilypond::LilyPondExporter, Exporter},
};

use crate::unit::exporter::common::{
    dynamics_and_attributes_score, multi_part_and_staff_score, simple_chord_score,
    simple_note_score, simple_rest_score,
};
fn get_exporter_output(score: &Score) -> String {
    let exporter = LilyPondExporter {};
    let mut writer = Vec::new();
    exporter.export(score, &mut writer).unwrap();
    String::from_utf8(writer).unwrap()
}

#[test]
fn test_simple_note() {
    let score = simple_note_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("c'4"));
}

#[test]
fn test_simple_chord() {
    let score = simple_chord_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("<c' e'>4"));
}

#[test]
fn test_simple_rest() {
    let score = simple_rest_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("r4"));
}

#[test]
fn test_complex_duration() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(7, 8),
    ));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("c'2.."));
}

#[test]
fn test_tuplet() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(2),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(4),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 2 { c'4 d'4 e'4 }"));
}

#[test]
fn test_mixed_tuplets() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(2),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(4),
            octave: 4,
        },
        Duration::from_fraction(1, 6),
    ));
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(5),
            octave: 4,
        },
        Duration::from_fraction(1, 4),
    ));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 2 { c'4 d'4 e'4 } f'4"));
}

#[test]
fn test_header_generation() {
    let mut score = Score::default();
    score.context_changes.push(ContextChange {
        key: MetadataKey::Title,
        value: MetadataValue::String("Test Title".to_string()),
        time_offset: Fraction::new(0, 1),
    });
    score.context_changes.push(ContextChange {
        key: MetadataKey::Composer,
        value: MetadataValue::String("Test Composer".to_string()),
        time_offset: Fraction::new(0, 1),
    });
    let output = get_exporter_output(&score);
    assert!(output.contains("title = \"Test Title\""));
    assert!(output.contains("composer = \"Test Composer\""));
}

#[test]
fn test_clef_change() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    staff.context_changes.push(ContextChange {
        key: MetadataKey::Clef,
        value: MetadataValue::Clef(carmen_lang::core::Clef::Bass),
        time_offset: Fraction::new(0, 1),
    });
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\clef bass"));
}

#[test]
fn test_time_signature_change() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let staff = Staff::new(1);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score.context_changes.push(ContextChange {
        key: MetadataKey::TimeSignature,
        value: MetadataValue::TimeSignature(3, 4),
        time_offset: Fraction::new(0, 1),
    });
    let output = get_exporter_output(&score);
    assert!(output.contains("\\time 3/4"));
}

#[test]
fn test_tempo_change() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let staff = Staff::new(1);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score.context_changes.push(ContextChange {
        key: MetadataKey::Tempo,
        value: MetadataValue::Number(120.0),
        time_offset: Fraction::new(0, 1),
    });
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tempo 4 = 120"));
}

#[test]
fn test_dynamics_and_attributes() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    let mut event = MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(1, 4),
    );
    event.dynamic = Some(carmen_lang::core::Dynamic::Ff);
    voice.events.push(event);
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let score = dynamics_and_attributes_score();
    let output = get_exporter_output(&score);
    println!("{output}");
    assert!(output.contains("c'4 -."));
    assert!(output.contains("\\ff"));
}

#[test]
fn test_multi_staff_part() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff1 = Staff::new(1);
    let mut voice1 = Voice::new();
    voice1.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(1, 4),
    ));
    staff1.voices.push(voice1);
    let mut staff2 = Staff::new(2);
    let mut voice2 = Voice::new();
    voice2.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 3,
        },
        Duration::from_fraction(1, 4),
    ));
    staff2.voices.push(voice2);
    part.staves.push(staff1);
    part.staves.push(staff2);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\new GrandStaff"));
    assert!(output.contains("c'4"));
    assert!(output.contains("c4"));
}

#[test]
fn test_multi_part_score() {
    let score = multi_part_and_staff_score();
    let output = get_exporter_output(&score);
    println!("{output}");
    assert!(output.contains("<<\n"));
    assert!(output.contains(">>\n"));
    assert!(output.contains("g''2"));
    assert!(output.contains("c1"));
}

#[test]
fn test_empty_score() {
    let score = Score::default();
    let output = get_exporter_output(&score);
    assert!(output.contains("\\version \"2.24.3\""));
    assert!(!output.contains("\\new Staff"));
}

#[test]
fn test_empty_part() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    timeline.parts.push(Part::new(Some("piano".to_string())));
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\new Staff"));
    assert!(!output.contains("c'4"));
}

#[test]
fn test_empty_staff() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.staves.push(Staff::new(1));
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\new Staff"));
    assert!(!output.contains("c'4"));
}

#[test]
fn test_empty_voice() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    let mut staff = Staff::new(1);
    staff.voices.push(Voice::new());
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\new Staff"));
    assert!(!output.contains("c'4"));
}

#[test]
fn test_no_time_signature() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass::new(0),
            octave: 4,
        },
        Duration::from_fraction(1, 4),
    ));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\remove \"Time_signature_engraver\""));
    assert!(output.contains("\\remove \"Bar_engraver\""));
}

#[test]
fn test_part_name_and_instrument_name() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("Violin I".to_string()));
    part.instrument = Some("Violin".to_string());
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("instrumentName = #\"Violin\""));
    assert!(output.contains("shortInstrumentName = #\"Violin\""));
}

#[test]
fn test_part_name_only() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let part = Part::new(Some("Soprano".to_string()));
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("instrumentName = #\"Soprano\""));
    assert!(output.contains("shortInstrumentName = #\"Soprano\""));
}

#[test]
fn test_no_part_name_or_instrument() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    timeline.parts.push(Part::new(None));
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(!output.contains("instrumentName"));
    assert!(!output.contains("shortInstrumentName"));
}

#[test]
fn test_tie_across_barline() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 4,
            },
            Duration::from_fraction(1, 2),
        )
        .with_offset(Fraction::new(3, 4)),
    );
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score.context_changes.push(ContextChange {
        key: MetadataKey::TimeSignature,
        value: MetadataValue::TimeSignature(4, 4),
        time_offset: Fraction::new(0, 1),
    });
    let output = get_exporter_output(&score);
    assert!(output.contains("c'4~ c'4"));
}

#[test]
fn test_tuplet_tie() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("violin".to_string()));
    part.instrument = Some("Violin".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(0, 1)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(1, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 5,
            },
            Duration::from_fraction(4, 12),
        )
        .with_offset(Fraction::new(2, 12)),
    );
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 4 { c''8 d''8 e''8~ } e''4"));
}

#[test]
fn test_tuplet_tie_1_3() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("violin".to_string()));
    part.instrument = Some("Violin".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(0, 1)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(1, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 5,
            },
            Duration::from_fraction(3, 12),
        )
        .with_offset(Fraction::new(2, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(5, 12)),
    );
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 4 { c''8 d''8 e''8~ } \\tuplet 3/2 4 { e''4 d''8 }"));
}

#[test]
fn test_tuplet_tie_1_3_mid() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("violin".to_string()));
    part.instrument = Some("Violin".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(0, 1)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(1, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(2, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(3, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(4, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 5,
            },
            Duration::from_fraction(3, 12),
        )
        .with_offset(Fraction::new(5, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(8, 12)),
    );
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 4 { c''8 c''8 c''8 } \\tuplet 3/2 4 { c''8 d''8 e''8~ } \\tuplet 3/2 4 { e''4 d''8 }"));
}

#[test]
fn test_multiple_tuplet_ties() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("violin".to_string()));
    part.instrument = Some("Violin".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(0, 1)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(1),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(1, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(4, 12),
        )
        .with_offset(Fraction::new(2, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(0),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(6, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(1),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(7, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(8, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(3),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(9, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(4),
                octave: 5,
            },
            Duration::from_fraction(1, 12),
        )
        .with_offset(Fraction::new(10, 12)),
    );
    voice.events.push(
        MusicalEvent::note(
            Pitch {
                pitch_class: PitchClass::new(2),
                octave: 5,
            },
            Duration::from_fraction(4, 12),
        )
        .with_offset(Fraction::new(11, 12)),
    );
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\tuplet 3/2 4 { c''8 cis''8 d''8~ } d''4 \\tuplet 3/2 4 { c''8 cis''8 d''8 } \\tuplet 3/2 4 { dis''8 e''8 d''8~ } d''4"));
}

#[test]
fn test_multi_staff_default_clefs() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("Piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff0 = Staff::new(0);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass(0),
            octave: 4,
        },
        Duration::quarter(),
    ));
    staff0.voices.push(voice);
    part.staves.push(staff0);
    let mut staff1 = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass(0),
            octave: 3,
        },
        Duration::quarter(),
    ));
    staff1.voices.push(voice);
    part.staves.push(staff1);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\clef treble"));
    assert!(output.contains("\\clef treble"));
}

#[test]
fn test_multi_staff_explicit_clefs() {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("Piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff0 = Staff::new(0);
    staff0.context_changes.push(ContextChange {
        key: MetadataKey::Clef,
        value: MetadataValue::Clef(carmen_lang::core::Clef::Alto),
        time_offset: Fraction::new(0, 1),
    });
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass(0),
            octave: 4,
        },
        Duration::quarter(),
    ));
    staff0.voices.push(voice);
    part.staves.push(staff0);
    let mut staff1 = Staff::new(1);
    staff1.context_changes.push(ContextChange {
        key: MetadataKey::Clef,
        value: MetadataValue::Clef(carmen_lang::core::Clef::Tenor),
        time_offset: Fraction::new(0, 1),
    });
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::note(
        Pitch {
            pitch_class: PitchClass(0),
            octave: 3,
        },
        Duration::quarter(),
    ));
    staff1.voices.push(voice);
    part.staves.push(staff1);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    let output = get_exporter_output(&score);
    assert!(output.contains("\\clef alto"));
    assert!(output.contains("\\clef tenor"));
    let alto_pos = output.find("\\clef alto").unwrap();
    let tenor_pos = output.find("\\clef tenor").unwrap();
    assert!(alto_pos < tenor_pos);
}

use carmen_lang::core::{
    Attribute, Chord, Duration, Dynamic, Movement, MusicalEvent, Part, Pitch, PitchClass, Score,
    Staff, Timeline, Voice,
};

pub fn simple_note_score() -> Score {
    let mut score = Score::default();
    score.set_name("Test Score".to_string());
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
    score
}

pub fn simple_chord_score() -> Score {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice.events.push(MusicalEvent::chord(
        Chord {
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
        },
        Duration::from_fraction(1, 4),
    ));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score
}

pub fn simple_rest_score() -> Score {
    let mut score = Score::default();
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    part.instrument = Some("Piano".to_string());
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    voice
        .events
        .push(MusicalEvent::rest(Duration::from_fraction(1, 4)));
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score
}

pub fn dynamics_and_attributes_score() -> Score {
    let mut score = Score::default();
    score.set_name("Test Score".to_string());
    let mut timeline = Timeline::default();
    let mut part = Part::new(Some("piano".to_string()));
    let mut staff = Staff::new(1);
    let mut voice = Voice::new();
    let mut event = MusicalEvent::note(
        Pitch::from_note_name("C", 0, 4).unwrap(),
        Duration::from_fraction(1, 4),
    );
    event.dynamic = Some(Dynamic::Ff);
    event.attributes.push(Attribute::Staccato);
    voice.events.push(event);
    staff.voices.push(voice);
    part.staves.push(staff);
    timeline.parts.push(part);
    score.timeline = Some(timeline);
    score
}

pub fn multi_part_and_staff_score() -> Score {
    let mut score = Score::default();
    let mut timeline = Timeline::default();

    // Part 1
    let mut part1 = Part::new(Some("Violin".to_string()));
    let mut staff1 = Staff::new(1);
    let mut voice1 = Voice::new();
    voice1.events.push(MusicalEvent::note(
        Pitch::from_note_name("G", 0, 5).unwrap(),
        Duration::from_fraction(1, 2),
    ));
    staff1.voices.push(voice1);
    part1.staves.push(staff1);
    timeline.parts.push(part1);

    // Part 2
    let mut part2 = Part::new(Some("Cello".to_string()));
    let mut staff2 = Staff::new(1);
    let mut voice2 = Voice::new();
    voice2.events.push(MusicalEvent::note(
        Pitch::from_note_name("C", 0, 3).unwrap(),
        Duration::from_fraction(1, 1),
    ));
    staff2.voices.push(voice2);
    part2.staves.push(staff2);
    timeline.parts.push(part2);

    score.timeline = Some(timeline);
    score
}

pub fn movements_score() -> Score {
    let mut score = Score::default();
    score.set_name("Multi-movement Score".to_string());

    // Movement 1
    let mut movement1 = Movement::new(Some("Allegro".to_string()));
    let mut timeline1 = Timeline::default();
    let mut part1 = Part::new(Some("Flute".to_string()));
    let mut staff1 = Staff::new(1);
    let mut voice1 = Voice::new();
    voice1.events.push(MusicalEvent::note(
        Pitch::from_note_name("A", 0, 4).unwrap(),
        Duration::from_fraction(1, 4),
    ));
    staff1.voices.push(voice1);
    part1.staves.push(staff1);
    timeline1.parts.push(part1);
    movement1.timeline = timeline1;
    score.movements.push(movement1);

    // Movement 2
    let mut movement2 = Movement::new(Some("Adagio".to_string()));
    let mut timeline2 = Timeline::default();
    let mut part2 = Part::new(Some("Oboe".to_string()));
    let mut staff2 = Staff::new(1);
    let mut voice2 = Voice::new();
    voice2.events.push(MusicalEvent::note(
        Pitch::from_note_name("B", 0, 4).unwrap(),
        Duration::from_fraction(1, 2),
    ));
    staff2.voices.push(voice2);
    part2.staves.push(staff2);
    timeline2.parts.push(part2);
    movement2.timeline = timeline2;
    score.movements.push(movement2);

    score
}

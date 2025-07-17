use carmen_lang::{
    core::Score,
    exporter::{text::TextExporter, Exporter},
};

use crate::unit::exporter::common::{
    dynamics_and_attributes_score, movements_score, multi_part_and_staff_score, simple_chord_score,
    simple_note_score, simple_rest_score,
};

fn get_exporter_output(score: &Score) -> String {
    let exporter = TextExporter::new();
    let mut writer = Vec::new();
    exporter.export(score, &mut writer).unwrap();
    String::from_utf8(writer).unwrap()
}

#[test]
fn test_simple_note() {
    let score = simple_note_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("Note: c4, Duration: 1/4"));
}

#[test]
fn test_simple_chord() {
    let score = simple_chord_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("Chord: [c4, e4], Duration: 1/4"));
}

#[test]
fn test_simple_rest() {
    let score = simple_rest_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("Rest, Duration: 1/4"));
}

#[test]
fn test_exporter_configurations() {
    let score = simple_note_score();
    let default_output = get_exporter_output(&score);
    assert!(default_output.contains("Score: Test Score"));
    assert!(default_output.contains("Total Duration:"));
    assert!(default_output.contains("[T: 0/1]"));
    assert!(default_output.contains("Note: c4"));
}

#[test]
fn test_dynamics_and_attributes() {
    let score = dynamics_and_attributes_score();
    let output = get_exporter_output(&score);
    assert!(output.contains(", Dynamic: Ff"));
    assert!(output.contains(", Attributes: Staccato"));
}

#[test]
fn test_empty_score() {
    let score = Score::default();
    let output = get_exporter_output(&score);
    assert!(output.contains("Score:"));
    assert!(!output.contains("Part 1"));
}

#[test]
fn test_multi_part_and_staff() {
    let score = multi_part_and_staff_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("Part 1: \"Violin\""));
    assert!(output.contains("Note: g5"));
    assert!(output.contains("Part 2: \"Cello\""));
    assert!(output.contains("Note: c3"));
}

#[test]
fn test_movements() {
    let score = movements_score();
    let output = get_exporter_output(&score);
    assert!(output.contains("Movements: 2"));
    assert!(output.contains("Movement 1: \"Allegro\""));
    assert!(output.contains("Note: a4"));
    assert!(output.contains("Movement 2: \"Adagio\""));
    assert!(output.contains("Note: b4"));
}

use carmen_lang::{interpreter::Value, run};

#[test]
fn test_multivoice_staff() {
    let source = r#"
let x = 1/4 [c4, d, e, f];
let y = x |> transpose(4);
let z = (x, y);

score "Voices" {
    timeline {
        part "Violin" {
            @clef "treble";
            z;
        };
    };
};
"#;

    let result = run(source).unwrap();
    match result {
        Value::Score(score) => {
            assert_eq!(score.timeline.as_ref().unwrap().parts.len(), 1);
            let part = &score.timeline.as_ref().unwrap().parts[0];
            assert_eq!(part.staves.len(), 1);
            let staff = &part.staves[0];
            assert_eq!(staff.voices.len(), 2);
            assert_eq!(staff.voices[0].events.len(), 4);
            assert_eq!(staff.voices[1].events.len(), 4);
        }
        _ => panic!("Expected a score"),
    }
}

# Carmen Language

Carmen is a programmatic music composition language

## Syntax

### Basics
Curly braces `{}` are used for blocks and sets. Square brackets `[]` for lists. Parenthesis `()` for tuples and function calls.


Comments start with a `//` and continue to the end of the line. They must be captured by the parser (e.g. to be repositioned correctly in by a formatter) but are ignored by the interpreter.

```carmen
// This is a comment
```

Variables and functions are defined using `let` and `def` keywords respectively.

```carmen
let x = 10; // Variable assignment
def add(a, b) { // Function definition
    return a + b;
};
```

Semicolons are used to terminate statements. In the REPL, expressions without semicolons are evaluated and their results are displayed.

```
1 + 2     // Evaluates and displays: 3
1 + 2;    // Evaluates but doesn't display the result
```

#### Operations
Arithmetic operations use standard symbols: `+`, `-`, `*`, `/`, `%`. Comparisons use `==`, `!=`, `<`, `>`, `<=`, `>=`.
Logical operations use `and`, `or`, `not`.

- `+` and `*` can be used for both numbers and lists (concatenation) - hence on musical sequences (see below).
- arithmetic operators can be used on numbers, durations, and time values.

### Metadata
Metadata are specified as `@` statements. They provide additional information about the composition, such as title, composer, copyright, and other relevant details.

```carmen
@title "My Composition"
@composer "John Doe"
```

### Musical concepts

- **Pitch:** Standard musical notation using lowercase note names (a-g), accidentals (`s` or `#` for sharp, `b` for flat), and an (optional) octave number.
  - Middle C is `c4`. Case is ignored for parsing.
  - Examples: `c4`, `fs5` (F-sharp 5), `eb3` (E-flat 3), `a0`, `b#4` (B-sharp 4).
  - Multiple accidentals are supported: `css4` (C double-sharp 4), `bbb3` (B double-flat 3).

- **Pitch Class:** a group of pitches with the same or enharmonic name, regardless of octave. Can be specified using the note name without octave (e.g., `c`, `d#`, `eb`) or using the integer representation (0-11).

- **Pitch Class Set:** an unordered collection of pitch classes, represented as a set `{ PITCH_CLASS1, PITCH_CLASS2 ... }`, e.g. `{0, 1, 4}`.

- **Chord:** A collection of pitches played simultaneously, represented as a tuple of pitches, e.g. `(c4, e4, g4)` for a C major chord.

- **Duration:** Specified using durations relative to a whole note.
  - Base durations:
    - `1`: Whole note
    - `1/2`: Half note
    - `1/4`: Quarter note
    - `1/8`: Eighth note
    - `1/16`: Sixteenth note
    - `1/32`: Thirty-second note
    - `1/64`: Sixty-fourth note
  - Dotted durations: Add one or more dots `.` after the base symbol. Each dot adds half the value of the preceding duration.
  - Non-standard durations: Use a fraction with a non-standard denominator, e.g. `1/7` for a septuplet.

- **Dynamics:** Specified using standard dynamic markings or MIDI velocity numbers.
  - Dynamic symbols: `pppp`, `ppp`, `pp`, `p`, `mp` (default if unspecified), `mf`, `f`, `ff`, `fff`, `ffff`.
  - Dynamic persist across sequential notes/chords within the same sequence until a new dynamic is explicitly specified.

- **Attribute:** Symbolic tags that modify how a note or chord is performed or notated.
  - Common attributes: `staccato`, `legato`, `accent`, `tenuto`.
  - Attributes apply only to the specific note/chord they follow and do not persist like dynamics.

- **Rest:** Represent silence in the music and are defined with `~`.



### Structuring music

#### Musical Events

Musical events are defined as `duration pitch/chord/list [dynamic] [attribute]... ;`. For example:

```carmen
1/4 c4; // Quarter note C4, default dynamic (mp)
1/4 ~; // Quarter rest
1/8. e5 f; // Dotted eighth E5, forte dynamic
1/2 gs3 pp staccato; // Half note G#3, pianissimo, staccato
1/4 (c4, e4, g4) mf; // Quarter note C major chord (C4 E4 G4), mezzo-forte
1/8 [c4, b3, c4, d4] f; // Sequential quarter notes C4, B3, C4, D4 with forte dynamic
1/8 [c4, b3, ~, d4] f; // Sequential quarter notes C4, B3, rest, D4 with forte dynamic
```

Lists and chords can be combined

```carmen
1/8 [c4, d4, (e4, g4), ~, (g4, b4), c5, ~] p; // Sequence of 1/8 notes, chords and rests with piano dynamic
```

When defining a sequence of notes within a list or tuple, the octave of the last note with a specified octave is implicitly carried over to subsequent notes that lack an octave. This simplifies writing melodic lines.
```carmen
// This sequence...
1/8 [c4, d, e, f];
// ...is equivalent to:
1/8 [c4, d4, e4, f4];

// The octave can be changed mid-sequence:
1/8 [c4, d, e, e5, d, c, b4];
// ...is equivalent to:
1/8 [c4, d4, e4, e5, d5, c5, b4];
```

Tuplets are supported natively as non-standard durations. E.g.
```carmen
1/3 [c3, c, c];
```
is a sequence of 3 notes, each of duration 1/3 of a whole - in classical notation, this will be a triplet of 3 over 2 half notes.

```carmen
1/12 [c3, c, c]; //triplet of 3 over 2 eights
[
  1/12 c3,
  1/24 [c3, c],
  1/12 c3,
]; //  triplet of 3 over 2 eights, with the second and third note being sixteenth
1/7 [c3, d, e, f, g, a, b]; // 7 over 4 quarters
```

#### Sequences

A sequence of musical events that are played in order. It is specified as a list and which manages its own internal time cursor. This allows for complex timing structures and nested sequences.

```carmen
let theme = [
    1/4 c4 p staccato, // Quarter note C4, piano, staccato
    1/8 (d4, fs) mf, // Eighth note chord (D4 F#4), mezzo-forte
    1/2 e4 fff, // Half note E4, fff
]; // Total duration is 1 (4 beats)
```



#### Multi-voice sequences
Multi-voice sequences allow for simultaneous musical expressions, useful for polyphonic music. They are defined using tuples.

```carmen
let violin_1 = [1/8 [c5, d, e] p];
let violin_2 = [1/8 [g4, a, b] f];
let violins = (violin_1, violin_2); // Multi-voice sequence with both voices starting at measure 0
```


#### Parts
A part is a named collection of musical expressions, typically representing an instrument or section. It allows for multi-track compositions and can include sequences, context changes, and other musical elements. Additional attributes like instrument name, MIDI channel, and offsets can be specified for parts. This is crucial for multi-track export (e.g., to MIDI).

```carmen
let violins = part "Violins" {
    (violin_1, violin_2); //starts at measure 0 with both voices
    violin_1; // Repeats Violin 1 sequence
};
```

Additional attributes like instrument name, MIDI channel, and offsets can be specified for parts. This is crucial for multi-track export (e.g., to MIDI).

```carmen
// Offset by a whole note, instrument name, and MIDI channel
let violins = part "Violins" {
    @instrument "Violin"; // Specify instrument name
    @channel 1; // Specify MIDI channel
    @offset 1; // Offset by a whole note

    ...
};
```

Parts can also include context specifications (like tempo, time signature,...) and their changes. They apply to all subsequent musical expressions until overridden. If none are specified, the outer context is used. The outer context has priority over the inner context if both are specified. If no outer context is available, defaults are used (e.g., 120 BPM, no time signature, no key signature).

```carmen
let violins = part "Violins" {
    @tempo 120; // Set tempo
    @time_signature 4 4; // Set time signature

    (violin_1, violin_2);

    @tempo 90; // Change tempo
    @key_signature D; // Set key signature to D

    violin_1;
};
```

##### Multi-staff parts
Parts can also be defined with multiple staves, allowing for complex arrangements with different instruments or voices. Each staff can have its own sequences and context changes.

```carmen
let piano = part "Piano" {

    staff 0 {
        @cleff "treble"
        [1/4 c5, 1/8 d5, 1/2 e5]; // Right hand sequence
    }; // Treble clef for right hand

    staff 1 {
        @cleff "bass"
        [1/4 c3, 1/8 g3, 1/2 e4]; // Left hand sequence
    }; // Bass clef for left hand;

};
```
##### Extending parts
Parts can be extended with additional sequences or context changes after their initial definition. This allows for modular composition and reusing parts in different contexts.

```carmen
let violins = part "Violins" {
    @instrument "Violin";
    (violin_1, violin_2); // Initial definition with both voices
};

// Extend the part with additional sequences
let extended_violins = violins + {
    @tempo 100; // Change tempo for the extension
    (violin_1, violin_2); // Reuse the initial sequences
    violin_1; // Add Violin 1 sequence again
};
```

#### Timeline
A timeline is the top-level container for musical expressions, combining multiple parts and sequences into a single musical flow. It manages the overall time cursor and context changes that apply to all contained parts.

```carmen
let my_timeline = timeline {
    part "Violins" {
        @instrument "Violin";
        (violin_1, violin_2);
    };

    part "Cello" {
        @instrument "Cello";
        [1/4 c3, 1/8 g3, 1/2 e4];
    };
};
```

#### Score
A score is the top-level container for an entire musical composition. It serves as the compilation target for `.carmen` files and provides a structured way to organize metadata, movements, and musical content. Every compiled Carmen file produces a score.

```carmen
score "Symphony No. 1" {
    // Global metadata
    @composer "Jane Doe";
    @title "Symphony No. 1 in C Major";
    @copyright "2024";
    @tempo 120; // Default tempo for all movements

    // Single timeline for simple compositions
    timeline {
        part "Violins" {
            @instrument "Violin";
            [1/4 c5, 1/8 d5, 1/2 e5];
        };

        part "Cello" {
            @instrument "Cello";
            [1/4 c3, 1/8 g3, 1/2 e4];
        };
    };
};
```
##### Multi-movement scores
Scores can contain multiple movements or sections, each with their own timeline and context settings.
```carmen
score "Sonata in D Major" {
    @composer "Jane Composer";
    @title "Sonata in D Major";
    @key_signature "D"; // Global key signature

    movement "Allegro" {
        @tempo 140;
        @time_signature 4 4;

        timeline {
            part "Piano" {
                staff 0 {
                    @clef "treble";
                    [1/4 d5, 1/4 e5, 1/2 fs5];
                };
                staff 1 {
                    @clef "bass";
                    [1/4 d3, 1/4 a3, 1/2 d4];
                };
            };
        };
    };

    movement "Adagio" {
        @tempo 60; // Override global tempo
        @key_signature "G"; // Change key for this movement

        timeline {
            part "Piano" {
                staff 0 {
                    [1/2 g4 p, 1/2 a4, 1/1 b4];
                };
                staff 1 {
                    [1/1 g2 p];
                };
            };
        };
    };
};
```

##### Implicit scores
For simple compositions, the score can be implicit. If no explicit `score` block is defined, the compiler automatically wraps the file content in a score using the filename as the default title.
```carmen
// File: simple_song.carmen
@title "Simple Song";
@composer "John Doe";

timeline {
    part "Piano" {
        [1/4 c4, 1/4 d4, 1/2 e4];
    };
};

// This is automatically wrapped as:
// score "Simple Song" {
//     @title "Simple Song";
//     @composer "John Doe";
//     timeline { ... };
// }

```

## Controlling the Musical Context

These functions modify the musical context from their point of evaluation onwards. They typically add control events to the timeline.

**1. `tempo`:**
Sets the tempo in Beats Per Minute (BPM) and adds a `Tempo` event to the timeline. The tempo affects all subsequent musical expressions until changed again.
```carmen
@tempo 120; // Set initial tempo to 120 BPM
```

**2. `time_signature`:**
Sets the time signature as `@time_signature NUMERATOR DENOMINATOR`. It affects the subsequent rhythmic structure of the music until changed again.

```carmen
@time_signature 4 4; // Common time (4 beats per measure, quarter note gets the beat)
@time_signature 3 4; // Waltz time (3 beats per measure, quarter note gets the beat)
```

> It defaults to `None` if not specified, meaning no time signature is applied. This is useful for free-form music or when the time signature is not relevant.

**3. `key_signature`:**
Sets the key signature.

```carmen
@key_signature C; // No sharps or flats (C major or A minor)
@key_signature G; // One sharp (F#) - G major or E minor
```

> It defaults to `None` if not specified, meaning no key signature is applied. This is useful for free-form music or when the key signature is not relevant.

**4. `clef`:**
Sets the clef for the current part or staff. Clefs determine the pitch range and notation style for the music.

```carmen
@clef "treble"; // Treble clef (G clef)
@clef "bass"; // Bass clef (F clef)
```

> It defaults to `treble` if not specified.

#### Score context hierarchy
Context settings (tempo, key signature, time signature) follow a hierarchical precedence:
1. **Movement-level** settings override score-level settings
2. **Part-level** settings override movement-level settings
3. **Local** settings override part-level settings

```carmen
score "Example" {
    @tempo 120; // Global tempo

    movement "Fast" {
        @tempo 160; // Overrides global tempo for this movement

        timeline {
            part "Violin" {
                @tempo 140; // Overrides movement tempo for this part

                [1/4 c5]; // Uses part tempo (140)
                @tempo 180; // Local tempo change
                [1/4 d5]; // Uses local tempo (180)
            };
        };
    };
};

```
## Programming Constructs


**1. Variables:**

Variables are defined using the `let` keyword. They can hold any Carmen value, including numbers, strings, lists, timelines, and more. Variable names are case-sensitive and must start with a letter or underscore.

```carmen
let root = c4; // Middle C
```

**2. Functions (`def`):**
Define custom functions to encapsulate reusable logic or musical patterns. Function names are case-sensitive. They are defined using the `def` keyword, followed by the function name and its parameters in parentheses. The body of the function is enclosed in curly braces `{}`.

```carmen
def major_third(root) {
    return root + 4; // Returns the pitch 4 semitones above the root
};
```

The preferred way to call a function is using the pipe operator `|>`:

```carmen
let x = c4 |> major_third(); // Calls major_third with c4 as the argument, resulting in e4
```
The piped-in value is taken as the first argument of the function. If the function has more than one argument, the remaining arguments can be passed in parentheses after the pipe operator.

**3. Conditional (`if`/`else`):**
Evaluate expressions based on conditions. The `if` statement checks a condition and executes the corresponding body if true. An optional `else` clause can be provided for the false case.

```carmen
if (condition) {
    // Body executed if condition is true
} else {
    // Body executed if condition is false
};
```

**4. Looping:**
- `for`: Iterates over a sequence or list, executing the body for each element. The loop variable is defined in parentheses before the body.

```carmen
for (item in list) {
    // Body executed for each item in the list
};
```

- `while`: Repeatedly executes the body as long as the condition is true. The condition is checked before each iteration.

```carmen
while (condition) {
    // Body executed as long as condition is true
};
```



### Built-in Functions

Carmen provides several built-in functions.

#### Intervals

- `pitch_interval` or `pi`: Returns the pitch interval between two pitches in semitones in absolute value.

```carmen
(c4, e4) |> pi(); // Returns 4 (C4 to E4 is a major third)
(c4, e5) |> pi(); // Returns 14
```

If the sign of the interval is important, one can pass the `ordered` parameter as `true` to get the signed interval:

```carmen
(e4, c4) |> pi(ordered=true); // Returns -4
```

- `pitch_class_interval` or `pci`: Returns the pitch class interval between two pitch classes in semitones - given two pitches `x` and `y`, it returns whichever is smaller between `(y - x) % 12` and `(x - y) % 12`.

```carmen
(0, 4) |> pci(); // Returns 4 (C to E is a major third)
(c4, e4) |> pci(); // Returns 4
(c4, e5) |> pci(); // Still returns 4
```

If the sign of the interval is important, one can pass the `ordered` parameter as `true` to get the signed interval which will be computed as `(y - x) % 12`:

```carmen
(0, 8) |> pci(); // Returns 8 (C to G is a perfect fifth)
(0, 8) |> pci(ordered=true); // Returns 4
```

#### Transformations

- `normal_form`: Returns the normal form of a pitch class set, which is the most compact representation of the set in ascending order.

```carmen
// F, Ab, A, C#
let pcs = {5, 8, 9, 1}; // Pitch class set
pcs |> normal_form(); // Returns {1, 5, 8, 9} (C#, F, Ab, A)
```

- `transpose` or `T`: transpose a pitch, pitch class, pitch class set, or sequence by a given interval in semitones. Let $T_n$ be the transposition operator by $n$ semitones, then if $y = T_n(x)$ it means that $y-x = n$.

```carmen
c4 |> transpose(4); // Transposes C4 up a major third to E4
0 |> transpose(4); // Transposes pitch class C (0) up a major third to E (4)
{0, 4, 7} |> transpose(2); // Transposes pitch class set {C, E, G} up a whole step to {D, F#, A}
let theme = [1/4 c4, 1/8 d4, 1/2 e5]; // Sequence of musical events
theme |> transpose(2); // Transposes the sequence up a whole step to [q d4; e e4; h f#5]
1/4 [c3, c4, (c3, c4), ~] |> transpose(3) mp; // transpose a sequence within an event
```

for single pitches and pitch classes the operators `+` or `-` can be used as a shorthand for transposing by a given interval in semitones.

- `invert` or `I`: inverts a pitch, pitch class, pitch class set, or sequence around a given axis. The axis can be either a pitch class (for chromatic inversion) or a specific pitch (for octave-preserving inversion).

  - Chromatic Inversion (around pitch class): Let $I_n$ be the inversion operator around pitch class $n$, then if $y = I_n(x)$ it means that $n - x = y \bmod 12$.

  ```carmen
  0 |> invert(4); // Returns 4
  4 |> invert(9); // Returns 5
  {0, 4, 7} |> invert(7); // Returns {7, 3, 0}
  ```

  - Pitch Inversion (around specific pitch): Inverts pitches around a specific pitch, preserving octave relationships.

  ```carmen
  c4 |> invert(c3); // C4 around C3 returns C2
  e4 |> invert(c4); // E4 around C4 returns Ab3
  {c4, e4, g4} |> invert(c4); // C major triad around C4 returns {c4, ab3, f3}
  ```

For single pitches, chromatic inversion operates on the pitch class and preserves the original octave, while pitch inversion operates on the full MIDI value. For sequences, the inversion is applied to each musical event recursively.


### Other constructs
- `interval_class` or `ic`: returns the interval class of a given interval
```carmen
(0, 7) |> ic(); // Returns 5
```
- `interval_class_content`

- `interval_class_vector` or `ic_vector`: return the interval class vector
```carmen
{0, 1, 2, 3} |> ic_vector(); // Returns [3, 2, 1, 0, 0, 0]
```
- `set_class`: Returns the list of pitch class sets that are in the set class of a pitch class set, which is the equivalence class of all pitch class sets that can be derived from it by transposition and inversion.

```carmen
{0, 3, 4} |> set_class(); // Returns [{0, 3, 4}, {1, 4, 5}, {2, 5, 6},...]
```
- `prime_form`: Returns the prime form of a pitch class set, which is the most compact representation of the set in ascending order, normalized to start with the lowest pitch class.

```carmen
{2, 5, 6} |> prime_form(); // Returns {0, 3, 4}
```

## Usage

This section explains how to use the `carmen` command-line tool to run scripts, export scores, and debug your code.

### Running Scripts

To execute a Carmen script file, pass the filename as an argument to the `carmen` command:

```bash
carmen your_script.carmen
```

This will run the script and print any output to the console.

### REPL

If you run `carmen` without any arguments, it will start an interactive REPL session. This is a great way to experiment with the language, test musical ideas, or quickly evaluate expressions.

```bash
carmen
```

You can type Carmen code directly into the REPL and see the results immediately.

### Exporting Scores

Carmen can export your compositions to various formats. The `--export` flag is used for this purpose.

The general syntax is:

```bash
carmen --export <format> <input_file> [output_file]
```

- `<format>`: The desired output format. Currently supported formats are:
    - `text`: A plain text representation of the score.
    - `lilypond` or `ly`: The LilyPond sheet music format.
- `<input_file>`: The Carmen script that generates the score.
- `[output_file]`: (Optional) The path to save the exported file. If omitted, the output will be printed to the standard output.

**Examples:**

```bash
# Export a score to text format and print to the console
carmen --export text my_score.carmen

# Export a score to a LilyPond file
carmen --export lilypond my_score.carmen my_score.ly
```

### Inspecting and Debugging

Carmen provides several tools to help you inspect your code and debug issues.

#### Tokenizing

To see how the lexer breaks your code into tokens, use the `--tokenize` flag:

```bash
carmen --tokenize your_script.carmen
```

This is useful for understanding the low-level lexical analysis of your script.

#### Parsing

To view the Abstract Syntax Tree (AST) generated by the parser, use the `--parse` flag:

```bash
carmen --parse your_script.carmen
```

This shows the structure of your code as the interpreter sees it, which can help diagnose syntax errors.

#### Inspecting the AST

For a more human-readable view of the AST, use the `--inspect` flag. This can help you understand how your code is structured and how different elements are related.

```bash
# Inspect the AST and print to the console
carmen --inspect your_script.carmen

# Inspect the AST and save the output to a file
carmen --inspect your_script.carmen inspection_output.txt
```

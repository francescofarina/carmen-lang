<p align="center">
  <img width="300" height="300" src="docs/assets/logo.svg">
</p>

# Carmen - Programmatic Music Composition

[![Crates.io](https://img.shields.io/crates/v/carmen-lang.svg)](https://crates.io/crates/carmen-lang)
[![Documentation](https://docs.rs/carmen-lang/badge.svg)](https://docs.rs/carmen-lang)
[![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg)](https://github.com/francescofarina/carmen#license)

## Table of Contents

- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Carmen Language Quick Start](#carmen-language-quick-start)
- [Development](#development)
- [Community](#community)
- [License](#license)

**Carmen** is a novel, programmatic language designed for music composition. It provides a flexible and expressive syntax to define musical structures, from simple melodies to complex multi-movement scores. It is built with a focus on clarity, modularity, and control, allowing composers and developers to create music through code.

## Features

- **Expressive Musical Notation:** Define pitches, durations, chords, rests, and dynamics with a clear and concise syntax.
- **Hierarchical Structure:** Organize your music into sequences, multi-voice parts, staves, timelines, movements, and scores.
- **Programmatic Control:** Use variables, functions, conditionals (`if/else`), and loops (`for`, `while`) to generate and manipulate musical material.
- **Powerful Transformations:** Leverage built-in functions for music theory operations like transposition (`T`), inversion (`I`), normal/prime form calculation, and more.
- **Context Management:** Control tempo, time signatures, key signatures, and clefs at any point in the score.
- **Extensible and Modular:** Define reusable musical patterns with functions and extend musical parts to build complex compositions from simpler blocks.
- **Multiple Export Formats:** Export your compositions to various formats, including plain text summaries and **LilyPond** for beautiful sheet music engraving.
- **Developer Tools:** Includes tools for tokenizing, parsing, and inspecting the Abstract Syntax Tree (AST) of Carmen code for debugging and analysis.

## Prerequisites

- **Rust toolchain** (for installation via cargo or building from source)
- **LilyPond** (optional, for generating beautiful sheet music from exported `.ly` files)

## Installation

### Using Cargo (Recommended)

The easiest way to install Carmen is through Cargo, Rust's package manager:

```bash
cargo install carmen-lang
```

This will download, compile, and install the `carmen` executable from [crates.io](https://crates.io/).

### Building from Source

For development purposes or to use the latest unreleased features, you can build from source:

You will need to have the [Rust programming language](https://www.rust-lang.org/tools/install) toolchain installed on your system.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/francescofarina/carmen.git
    cd carmen
    ```
2.  **Build the project:**
    ```bash
    cargo build --release
    ```
    The compiled binary will be located at `target/release/carmen`.

## Usage

The `carmen` executable can be used to run scripts, start an interactive REPL, or export compositions.

### Running a Script

To execute a script file, pass its path to the `carmen` executable:
```bash
carmen your_script.carmen
```

### Interactive REPL

To experiment with the language, you can run `carmen` without any arguments to start the interactive Read-Eval-Print Loop (REPL).

```bash
carmen
```
```
carmen Language REPL
Type 'exit' or press Ctrl+C to quit
carmen> 1/4 c4;
carmen> let theme = [1/8 [c4, d, e, f, g, a, b, c5]];
carmen> theme |> transpose(2);
```

### Exporting a Score

Use the `--export` flag to convert a `.carmen` file into other formats.

```bash
# Export to LilyPond format
carmen --export lilypond your_score.carmen score.ly

# Export to a plain text summary
carmen --export text your_score.carmen summary.txt
```

### Debugging

Carmen provides flags to inspect the compilation process, which is useful for debugging.

- `--tokenize`: Shows the stream of tokens from the lexer.
- `--parse`: Displays the Abstract Syntax Tree (AST).
- `--inspect`: Provides a more detailed, human-readable view of the AST.

```bash
carmen --parse your_script.carmen
```

## Carmen Language Quick Start

Here is a small example of a multi-staff piano part written in Carmen.

```carmen
// File: example.carmen
score "Simple Piano Piece" {
    @composer "Jane Doe";
    @tempo 120;

    timeline {
        part "Piano" {
            staff 0 { // Right hand
                @clef "treble";
                [1/4 c5, 1/8 d5, 1/2 e5];
            };

            staff 1 { // Left hand
                @clef "bass";
                [1/4 c3, 1/8 g3, 1/2 e4];
            };
        };
    };
};
```

To compile this and generate a LilyPond file for sheet music:

```bash
carmen --export lilypond example.carmen example.ly
```

Once you have the `example.ly` file, you can generate a PDF of the sheet music using [LilyPond](http://lilypond.org/):
```bash
lilypond example.ly
```

For a complete guide to the language syntax and features, please see the [**Language Manual**](docs/language_manual.md).

## Examples

### Basic Melody
```carmen
// Simple melody with rests and dynamics
let melody = [
    1/4 c4 mf,
    1/8 d4,
    1/8 e4,
    1/2 f4 p,
    1/4 ~
];
```

### Multi-Voice Bach Chorale
Carmen excels at complex multi-voice compositions. Here's an excerpt from Bach's BWV 147:

```carmen
let voice_1 = [
    1/12 [~, g4, a, b, d5, c, c, e, d, d, g, fs, g, d, b4, g, a, b, c5, d, e, d, c, b4, a, b, g],
    1/12 [fs4, g],
    3/12 a4,
    1/12 [g4, c5, b4, a],
];

let voice_2 = [
    1/12 [~, b3, d4],
    1/4 [d4, e, g, e, b3, a, d4],
    1/2 c4,
    1/4 [a3, fs4]
];

score "BVW 147" {
    @composer "J S Bach";
    @title "BVW 147";
    @key_signature "G";
    @time_signature 3/4;
    @tempo 100;

    timeline {
        part "Violin" {
            @clef "treble";
            voice_1;
        };
        part "Violin 2" {
            @clef "treble";
            voice_2;
        };
    };
};
```

### Musical Transformations
```carmen
// Define a theme and apply transformations
let theme = 1/4 [c4, d, e, f];

// Apply transformations
let transposed = theme |> transpose(7);      // Transpose up a perfect fifth

// Pitch class set analysis
let chord_set = {0, 4, 7};                       // C major triad as pitch classes
let normal = chord_set |> normal_form();         // Get normal form
let prime = chord_set |> prime_form();           // Get prime form
let ic_vec = chord_set |> ic_vector();           // Get interval class vector
let inverted = chord_set |> invert(2);           // Get inversions
```

### Multi-Staff Piano Score
```carmen
score "Piano Piece" {
    @composer "Carmen User";
    @tempo 120;
    @time_signature 4/4;

    timeline {
        part "Piano" {
            staff 0 {
                @clef "treble";
                [1/4 c5, 1/8 d5, 1/2 e5];
            };

            staff 1 {
                @clef "bass";
                [1/4 c3, 1/8 g3, 1/2 e4];
            };
        };
    };
};
```

For more examples, see the [`examples/`](examples/) directory.

## Development

If you want to contribute to Carmen or work on the project for development purposes:

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/francescofarina/carmen.git
    cd carmen
    ```
2.  **Build in debug mode:**
    ```bash
    cargo build
    ```
3.  **Run tests:**
    ```bash
    cargo test
    ```
4.  **Run with your changes:**
    ```bash
    cargo run -- your_script.carmen
    ```

We welcome contributions! Please see our [contributing guidelines](CONTRIBUTING.md) for more information.

## Community

- **Issues & Bug Reports**: [GitHub Issues](https://github.com/francescofarina/carmen/issues)
- **Documentation**: [Language Manual](docs/language_manual.md)
- **Crate Documentation**: [docs.rs/carmen-lang](https://docs.rs/carmen-lang)

## License

This project is dual-licensed under the terms of the [MIT License](LICENSE.MIT) and the [Apache License 2.0](LICENSE.Apache). You may choose to use this software under the terms of either license.

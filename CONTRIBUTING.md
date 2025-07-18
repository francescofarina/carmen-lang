# Contributing to Carmen

Thank you for your interest in contributing to Carmen! This document provides guidelines for contributing to the Carmen programming language for music composition.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Contributing Process](#contributing-process)
- [Commit Message Guidelines](#commit-message-guidelines)
- [Pull Request Process](#pull-request-process)
- [Code Style Guidelines](#code-style-guidelines)
- [Testing](#testing)
- [Documentation](#documentation)
- [Reporting Issues](#reporting-issues)
- [Feature Requests](#feature-requests)
- [Community](#community)

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to @francescofarina.

## Getting Started

### Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) (stable, beta, or nightly)
- Basic understanding of music theory is helpful but not required

### First Time Setup

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/carmen.git
   cd carmen
   ```
3. **Add the upstream remote**:
   ```bash
   git remote add upstream https://github.com/francescofarina/carmen-lang.git
   ```
4. **Build the project**:
   ```bash
   cargo build
   ```
5. **Run the tests**:
   ```bash
   cargo test
   ```

## Development Setup

### Building Carmen

```bash
# Debug build (faster compilation, slower execution)
cargo build

# Release build (slower compilation, faster execution)
cargo build --release
```

### Running Carmen

```bash
# Run from source (debug build)
cargo run

# Run a specific file
cargo run -- path/to/your/file.carmen

# Export to LilyPond
cargo run -- --export lilypond input.carmen output.ly

# Debug parsing
cargo run -- --parse input.carmen
```

### Development Tools

- **Format code**: `cargo fmt`
- **Lint code**: `cargo clippy`
- **Run tests**: `cargo test`
- **Generate documentation**: `cargo doc --open`

## Contributing Process

1. **Check existing issues** to see if your contribution is already being discussed
2. **Create an issue** for significant changes to discuss the approach
3. **Create a feature branch** from `main`
4. **Make your changes** following our guidelines
5. **Test thoroughly** and ensure CI passes
6. **Submit a pull request**

### Branch Naming

Use descriptive branch names with prefixes:
- `feat/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring
- `test/` - Test improvements
- `chore/` - Maintenance tasks

Examples:
- `feat/add-chord-inversions`
- `fix/tempo-parsing-bug`
- `docs/update-language-manual`

## Commit Message Guidelines

We use [Conventional Commits](https://www.conventionalcommits.org/) for clear and semantic commit messages. This helps with automated changelog generation and semantic versioning.

### Commit Message Format

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

### Types

- **feat**: A new feature
- **fix**: A bug fix
- **docs**: Documentation only changes
- **style**: Changes that do not affect the meaning of the code (white-space, formatting, etc.)
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **perf**: A code change that improves performance
- **test**: Adding missing tests or correcting existing tests
- **chore**: Changes to the build process or auxiliary tools and libraries

### Scope

The scope should be the name of the affected component (lexer, parser, interpreter, export, etc.).

### Examples

- `feat(parser): add support for chord inversions`
- `fix(lexer): handle unicode characters in comments`
- `docs(readme): update installation instructions`
- `test(interpreter): add tests for function definitions`
- `chore(ci): update rust toolchain to 1.75`

### Breaking Changes

For breaking changes, add `!` after the type/scope and include `BREAKING CHANGE:` in the footer:

```
feat(parser)!: change note syntax to use sharps by default

BREAKING CHANGE: Note parsing now defaults to sharps instead of flats
```

## Pull Request Process

1. **Update documentation** if you're changing functionality
2. **Add tests** for new features or bug fixes
3. **Ensure all tests pass** locally and in CI
4. **Update CHANGELOG.md** following the existing format
5. **Request review** from maintainers
6. **Address feedback** promptly and respectfully

### Pull Request Template

When creating a pull request, please include:

- **Description**: What does this PR do?
- **Motivation**: Why is this change needed?
- **Testing**: How was this tested?
- **Breaking Changes**: Any breaking changes?
- **Related Issues**: Link to related issues

## Code Style Guidelines

### Rust Code Style

We follow standard Rust conventions:

- Use `cargo fmt` to format your code
- Address all `cargo clippy` warnings
- Follow the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- Use descriptive variable and function names
- Write clear, concise comments for complex logic

### Documentation Style

- Use proper Rust documentation comments (`///` for public items)
- Include examples in documentation when helpful
- Keep comments up-to-date with code changes
- Write clear commit messages and PR descriptions

### Carmen Language Examples

When adding examples:

- Use clear, musical examples that demonstrate the feature
- Include both simple and complex use cases
- Ensure examples are syntactically correct
- Add comments explaining musical concepts when helpful

## Testing

### Test Structure

Carmen uses a comprehensive testing structure organized into two main categories:

#### Unit Tests (`tests/unit/`)
Organized by component, each with their own directory.

#### Integration Tests (`tests/integration/`)
End-to-end tests that verify complete Carmen programs.

### Writing Tests

#### Unit Test Guidelines
- Place tests in the appropriate component directory under `tests/unit/`
- Use helper functions for common operations (see `tests/unit/lexer/mod.rs` for examples)
- Organize related tests into nested modules (e.g., `literal_tests`, `error_tests`)
- Include comprehensive edge cases and error conditions
- Use descriptive test names that explain what is being tested

#### Test Helper Patterns
Follow existing patterns for test helpers:

```rust
// Helper to test single tokens
fn test_single_token(input: &str, expected: TokenType) -> Result<()> {
    let token = first_token(input)?;
    assert_token_type(&token, &expected);
    Ok(())
}

// Helper to verify token sequences
fn assert_token_sequence(tokens: &[Token], expected_types: &[TokenType]) {
    // Implementation...
}
```

#### Integration Test Guidelines
- Test complete Carmen programs from input to final output
- Verify the structure and content of resulting data structures
- Include both simple and complex musical examples
- Test export functionality with actual Carmen code

#### Error Testing
Always include tests for error conditions:

### Test Organization Best Practices

- **Component Tests**: Place unit tests in `tests/unit/[component]/`
- **Integration Tests**: Add to `tests/integration/` for end-to-end functionality
- **Example Validation**: Use files in `examples/` directory for realistic test cases
- **Helper Functions**: Create reusable test utilities for common operations
- **Nested Modules**: Group related tests using `mod` declarations for better organization

## Documentation

### Types of Documentation

1. **API Documentation**: Generated from code comments
2. **Language Manual**: User-facing documentation in `docs/`
3. **README**: Project overview and quick start
4. **Examples**: Working Carmen code in `examples/`

### Updating Documentation

- Update relevant documentation when changing functionality
- Add examples for new features
- Keep installation and usage instructions current
- Ensure all links work and examples compile

## Reporting Issues

### Bug Reports

Please include:

- **Carmen version** or commit hash
- **Operating system** and version
- **Rust version** (`rustc --version`)
- **Minimal example** that reproduces the issue
- **Expected behavior** vs actual behavior
- **Error messages** (full stack trace if available)

### Security Issues

For security vulnerabilities, please contact @francescofarina directly instead of creating a public issue.

## Feature Requests

We welcome feature requests! Please:

1. **Check existing issues** to avoid duplicates
2. **Describe the use case** clearly
3. **Provide examples** of how the feature would be used
4. **Consider the scope** - is this a core language feature or could it be a library?
5. **Be open to discussion** about implementation approaches

### Good Feature Request Example

```
Title: Add support for microtonal intervals

Description:
Support for microtonal music would allow composers to work with quarter-tones
and other non-12-tone equal temperament scales.

Use Case:
- Contemporary classical music often uses microtones
- Non-Western musical traditions use different tuning systems
- Electronic music producers want precise frequency control

Example Syntax:
```carmen
// Quarter-tone sharp
1/4 c4+50; // 50 cents sharp

// Custom tuning
@tuning "just_intonation";
1/4 [c4, e4, g4]; // Pure intervals
```

This would require:
- Extending the note parsing syntax
- Adding tuning system support
- Updating export formats to handle microtones
```

## Community

### Getting Help

- **GitHub Issues**: For bugs and feature requests
- **Other**: Contact @francescofarina for sensitive issues

### Contributing Beyond Code

- **Documentation**: Help improve guides and examples
- **Testing**: Test new features and report issues
- **Examples**: Create interesting Carmen compositions
- **Outreach**: Share Carmen with musicians and developers
- **Tutorials**: Write blog posts or create videos

### Recognition

Contributors are recognized in:
- Git commit history
- CHANGELOG.md for significant contributions
- README.md acknowledgments section
- Project releases and announcements

Thank you for contributing to Carmen! 🎵

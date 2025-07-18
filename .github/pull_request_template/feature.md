## 🎵 New Feature

**PR Title**: Please use conventional commits format: `feat(scope): description`

Example: `feat(parser): add support for microtonal intervals`

### Description

Brief description of the new feature and what it adds to Carmen.

### Related Issue

Closes #[issue-number] (if applicable)

### Feature Category

- [ ] 🎼 Language Syntax (new notation or constructs)
- [ ] 🎵 Music Theory (new transformations, analysis functions)
- [ ] 📤 Export/Output (new formats, improved rendering)
- [ ] 🔧 Developer Tools (debugging, analysis, REPL features)
- [ ] ⚡ Performance/Optimization
- [ ] 📚 Documentation/Examples
- [ ] 🏗️ Infrastructure/Build

### New Syntax/API

If this introduces new Carmen syntax or API, provide examples:

```carmen
// New syntax example
@new_directive "value";
let result = new_function([c4, d4, e4]);
```

### Musical Use Cases

Describe the musical scenarios this feature enables:

- **Compositional technique**: How composers would use this
- **Music theory application**: What theoretical concepts it supports
- **Genre/style relevance**: What types of music benefit from this

### Examples

#### Basic Usage
```carmen
// Simple example demonstrating the feature
1/4 c4;
```

#### Advanced Usage
```carmen
// More complex example showing advanced capabilities
score "Feature Demo" {
    // Advanced usage here
};
```

### Breaking Changes

- [ ] This is not a breaking change
- [ ] This is a breaking change

If breaking, describe:

### Documentation

#### Documentation Added/Updated

- [ ] Language manual (`docs/language_manual.md`)
- [ ] Inline code documentation (`///` comments)
- [ ] README examples
- [ ] New example files in `examples/`
- [ ] API documentation
- [ ] Error message documentation

## Checklist

- [ ] PR title follows conventional commits format (`type(scope): description`)
- [ ] My code follows the project's style guidelines
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] Any dependent changes have been merged and published
- [ ] All CI checks passed

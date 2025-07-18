## ♻️ Refactor

**PR Title**: Please use conventional commits format based on the type of change:
- `refactor(scope): description` for code refactoring
- `perf(scope): description` for performance improvements
- `chore(scope): description` for build/CI improvements

Example: `refactor(parser): simplify expression parsing logic`

### Description

Clear description of the refactoring changes and their purpose.

### Related Issue

Closes #[issue-number] (if applicable)

### Refactor Type

- [ ] ♻️ Code Refactoring (improve code structure without changing functionality)
- [ ] 🧹 Code Cleanup (remove dead code, simplify logic)
- [ ] 📦 Dependency Updates (update external dependencies)
- [ ] 🏗️ Architecture Changes (improve system design)
- [ ] ⚡ Performance Optimization (improve efficiency)
- [ ] 🔧 Build/CI Improvements (improve development workflow)
- [ ] 📁 File Organization (restructure project layout)
- [ ] 🎨 Code Style Improvements (formatting, naming conventions)
- [ ] 🧪 Test Organization (improve test structure)
- [ ] 📝 Code Documentation (improve inline docs)

### Motivation

**Why this refactor is needed**, e.g.:
- Current issues with the code/structure
- Benefits this refactor will provide
- Long-term maintainability improvements

### Changes Made

Clearly outline the changes made in this refactor.

### Future Benefits

Highlight the future benefits of the refactor.

## Checklist

- [ ] PR title follows conventional commits format (`type(scope): description`)
- [ ] My code follows the project's style guidelines
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] Any dependent changes have been merged and published
- [ ] All CI checks passed

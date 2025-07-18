## Description

Brief description of the changes in this PR.

**PR Title Format**: This PR title should follow [Conventional Commits](https://www.conventionalcommits.org/) format:
- `feat(scope): description` for new features
- `fix(scope): description` for bug fixes
- `docs(scope): description` for documentation changes
- `refactor(scope): description` for refactoring
- `test(scope): description` for test improvements
- `chore(scope): description` for maintenance tasks

Example: `feat(parser): add support for chord inversions`

### Related Issue

Closes #[issue-number] (if applicable)

## Type of Change

- [ ] 🐛 Bug fix (non-breaking change which fixes an issue)
- [ ] ✨ New feature (non-breaking change which adds functionality)
- [ ] 💥 Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] 📚 Documentation update
- [ ] 🎨 Code style/formatting changes
- [ ] ♻️ Refactoring (no functional changes)
- [ ] ⚡ Performance improvement
- [ ] 🧪 Test improvements
- [ ] 🔧 Build/CI changes
- [ ] 🧹 Chore (maintenance, dependencies, etc.)

## Motivation and Context

Why is this change required? What problem does it solve?

If it fixes an open issue, please link to the issue here (e.g., "Fixes #123").

## Changes Made

Detailed description of the changes:

- Change 1
- Change 2
- Change 3

## Carmen Code Examples

If applicable, provide Carmen code examples that demonstrate the changes:

```carmen
// Example showing new feature or fix
1/4 c4;
```

**Before** (if applicable):
```carmen
// Previous behavior
```

**After**:
```carmen
// New behavior
```

## Breaking Changes

If this is a breaking change, describe:

1. **What breaks**: What existing functionality is affected?
2. **Migration path**: How should users update their code?
3. **Justification**: Why is this breaking change necessary?

Example:
```carmen
// Old syntax (no longer works)
old_syntax_example;

// New syntax (required)
new_syntax_example;
```

## Checklist

- [ ] PR title follows conventional commits format (`type(scope): description`)
- [ ] My code follows the project's style guidelines
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] Any dependent changes have been merged and published
- [ ] All CI checks passed

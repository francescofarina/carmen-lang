## 🐛 Bug Fix

**PR Title**: Please use conventional commits format: `fix(scope): description`

Example: `fix(lexer): handle unicode characters in comments`

### Description

Clear and concise description of the bug that this PR fixes.

### Related Issue

Fixes #[issue-number]

### Bug Category

- [ ] 🔤 Lexer (tokenization issues)
- [ ] 📝 Parser (syntax parsing problems)
- [ ] 🌳 AST (abstract syntax tree issues)
- [ ] ⚙️ Interpreter (execution/evaluation bugs)
- [ ] 📤 Export (output generation problems)
- [ ] 💬 REPL (interactive mode issues)
- [ ] 🖥️ CLI (command-line interface bugs)
- [ ] 📚 Documentation (incorrect information)
- [ ] 🧪 Tests (test-related issues)
- [ ] 🏗️ Build/CI (compilation or CI problems)

### Root Cause

Detailed explanation of what was causing the bug:

**Problem**: What was happening incorrectly?

**Cause**: Why was it happening? (e.g., incorrect logic, missing validation, wrong assumption)

**Impact**: What functionality was affected?

### Solution

Explain how the bug was fixed:

**Approach**: What strategy was used to fix the issue?

**Changes**: What specific changes were made?

**Why this fix**: Why is this the right solution?

### Test Cases

#### Reproduction Test
- [ ] Added test that reproduces the original bug
- [ ] Test fails on main branch
- [ ] Test passes with this fix

#### Regression Tests
- [ ] Verified existing functionality still works
- [ ] Added edge case tests
- [ ] Tested similar scenarios

### Backward Compatibility

- [ ] This fix is fully backward compatible
- [ ] This fix includes minor breaking changes (documented below)
- [ ] This fix requires major breaking changes (justified below)

### Performance Impact

- [ ] No performance impact
- [ ] Performance improvement (describe)
- [ ] Minor performance cost (justify)
### Related Issues

List any related bugs that might exist or were discovered.


### Additional Context

#### Alternative Solutions Considered
Why this approach was chosen over alternatives:

1. **Alternative 1**: Why it wasn't chosen
2. **Alternative 2**: Why it wasn't chosen
3. **Chosen approach**: Why it's the best solution

## Checklist

- [ ] PR title follows conventional commits format (`type(scope): description`)
- [ ] My code follows the project's style guidelines
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] Any dependent changes have been merged and published
- [ ] All CI checks passed

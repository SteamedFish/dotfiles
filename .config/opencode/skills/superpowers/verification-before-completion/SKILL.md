---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing, before committing or creating PRs - requires running verification commands and confirming output before making any success claims; evidence before assertions always
---

# Verification Before Completion

## Overview

Claiming work is complete without verification is dishonesty, not efficiency.

**Core principle:** Evidence before claims, always.

**Violating the letter of this rule is violating the spirit of this rule.**

## The Iron Law

```
NO COMPLETION CLAIMS WITHOUT FRESH VERIFICATION EVIDENCE
```

If you haven't run the verification command in this message, you cannot claim it passes.

## The Gate Function

```
BEFORE claiming any status or expressing satisfaction:

1. IDENTIFY: What command proves this claim?
2. RUN: Execute the FULL command (fresh, complete)
3. READ: Full output, check exit code, count failures
4. VERIFY: Does output confirm the claim?
   - If NO: State actual status with evidence
   - If YES: State claim WITH evidence
5. ONLY THEN: Make the claim

Skip any step = lying, not verifying
```

## Common Failures

| Claim | Requires | Not Sufficient |
|-------|----------|----------------|
| Tests pass | Test command output: 0 failures | Previous run, "should pass" |
| Linter clean | Linter output: 0 errors | Partial check, extrapolation |
| Build succeeds | Build command: exit 0 | Linter passing, logs look good |
| Bug fixed | Test original symptom: passes | Code changed, assumed fixed |
| Regression test works | Red-green cycle verified | Test passes once |
| Agent completed | VCS diff shows changes | Agent reports "success" |
| Requirements met | Line-by-line checklist | Tests passing |
| Code formatted | Formatter run on files | "Looks formatted" |
| Security checked | No API keys/secrets in code | "I was careful" |

## Code Quality Verification

**Before claiming "code is ready" or "quality checks pass", verify these:**

### Mandatory Code Checking Tools by Language

| Language | Required Tools | Verify Command |
|----------|---------------|----------------|
| **Bash/Shell** | shellcheck | `shellcheck script.sh` (exit 0) |
| **Python** | ruff (or pylint) | `ruff check .` (0 errors) |
| **JavaScript/TypeScript** | eslint | `eslint src/` (0 errors) |
| **Go** | go vet | `go vet ./...` (exit 0) |
| **Rust** | clippy | `cargo clippy` (0 errors) |
| **Emacs Lisp** | byte-compile | `emacs --batch -f batch-byte-compile file.el` (0 warnings) |

**Install check:** If tool missing, install before verifying. "Tool not found" ≠ "checks pass".

### Code Formatting Rules

| File Type | Rule | Verification |
|-----------|------|--------------|
| **NEW files** | Use formatter (black, prettier, gofmt) | Run formatter, verify output |
| **MODIFIED files** | Format ONLY changed lines | Preserve untouched areas, format edits only |
| **Consistency** | Match existing style if no formatter | Check surrounding code patterns |

**Priority order for style consistency:**
1. Formatter config in project (if exists)
2. Existing code patterns in same file
3. Existing code patterns in project
4. Language standard conventions

### Security Checklist

Before claiming "secure" or "ready to commit":

| Check | Requirement | Verification |
|-------|-------------|--------------|
| **API Keys** | Not in code | Grep for patterns: `grep -r "api[_-]key" src/` (0 matches) |
| **.env files** | In .gitignore | `git check-ignore .env` (returns yes) |
| **Sensitive logs** | No passwords/tokens logged | Review log statements |
| **Input validation** | Validated before use | Check data entry points |
| **Dependencies** | No known vulnerabilities | Run `npm audit` / `pip-audit` (0 high/critical) |

### Performance Verification

**Only verify when performance is a requirement:**

| Claim | Requires | Tool |
|-------|----------|------|
| "Performs well" | Profiling data showing acceptable metrics | time, perf, profiler |
| "No memory leaks" | Memory usage stable over time | valgrind, heap profiler |
| "Optimized" | Before/after benchmarks | benchmark suite |

**Don't claim performance without measurements.**

## Common Failures

- Using "should", "probably", "seems to"
- Expressing satisfaction before verification ("Great!", "Perfect!", "Done!", etc.)
- About to commit/push/PR without verification
- Trusting agent success reports
- Relying on partial verification
- Thinking "just this once"
- Tired and wanting work over
- **ANY wording implying success without having run verification**

## Rationalization Prevention

| Excuse | Reality |
|--------|---------|
| "Should work now" | RUN the verification |
| "I'm confident" | Confidence ≠ evidence |
| "Just this once" | No exceptions |
| "Linter passed" | Linter ≠ compiler |
| "Agent said success" | Verify independently |
| "I'm tired" | Exhaustion ≠ excuse |
| "Partial check is enough" | Partial proves nothing |
| "Different words so rule doesn't apply" | Spirit over letter |

## Key Patterns

**Tests:**
```
✅ [Run test command] [See: 34/34 pass] "All tests pass"
❌ "Should pass now" / "Looks correct"
```

**Regression tests (TDD Red-Green):**
```
✅ Write → Run (pass) → Revert fix → Run (MUST FAIL) → Restore → Run (pass)
❌ "I've written a regression test" (without red-green verification)
```

**Build:**
```
✅ [Run build] [See: exit 0] "Build passes"
❌ "Linter passed" (linter doesn't check compilation)
```

**Requirements:**
```
✅ Re-read plan → Create checklist → Verify each → Report gaps or completion
❌ "Tests pass, phase complete"
```

**Agent delegation:**
```
✅ Agent reports success → Check VCS diff → Verify changes → Report actual state
❌ Trust agent report
```

## Why This Matters

From 24 failure memories:
- your human partner said "I don't believe you" - trust broken
- Undefined functions shipped - would crash
- Missing requirements shipped - incomplete features
- Time wasted on false completion → redirect → rework
- Violates: "Honesty is a core value. If you lie, you'll be replaced."

## When To Apply

**ALWAYS before:**
- ANY variation of success/completion claims
- ANY expression of satisfaction
- ANY positive statement about work state
- Committing, PR creation, task completion
- Moving to next task
- Delegating to agents

**Rule applies to:**
- Exact phrases
- Paraphrases and synonyms
- Implications of success
- ANY communication suggesting completion/correctness

## The Bottom Line

**No shortcuts for verification.**

Run the command. Read the output. THEN claim the result.

This is non-negotiable.

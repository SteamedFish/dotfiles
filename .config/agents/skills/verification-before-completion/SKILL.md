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

### Code Quality Standards

All code must meet these standards before claiming completion:

| Quality Dimension | Definition | Verification |
|------------------|------------|--------------|
| **Readability** | Clear structure, meaningful names, logical flow | Review: Can someone unfamiliar understand quickly? |
| **Maintainability** | Easy to modify, extend, debug | Review: Can features be added without rewrite? |
| **Testability** | Straightforward to validate behavior | Check: Can you test without mocking everything? |
| **Efficiency** | Appropriate performance for requirements | Verify: No obvious O(n²) when O(n) possible |
| **Robustness** | Handles edge cases, errors gracefully | Check: Null, empty, boundary values handled |

**How to verify:** Code review against these criteria BEFORE claiming "complete" or "ready".

### Error Handling Standards

**Before claiming error handling is correct:**

| Principle | Requirement | Verification |
|-----------|-------------|--------------|
| **Fail fast** | Detect and report errors early at source | Check: Validation at entry points, not deep in call stack |
| **Meaningful messages** | Context: what failed, why, how to fix | Review: Error messages include relevant data |
| **No silent failures** | Never catch and ignore without logging | Grep: No empty `catch {}` blocks |
| **Graceful degradation** | System continues with reduced functionality | Test: Partial failures don't crash entire system |
| **Centralized handling** | Consistent error handling patterns | Review: Use common error handler, not ad-hoc `try-catch` |

**Example verification:**
```bash
# Check for silent failures
grep -r "catch.*{[[:space:]]*}" src/
# Should return 0 matches

# Check for meaningful errors
grep -r "throw new Error('error')" src/
# Generic errors should have context
```

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

### Dependency Management Verification

**Before claiming dependencies are properly managed:**

| Requirement | Verification | Tool |
|-------------|--------------|------|
| **Use package managers** | npm, pip, cargo, etc. | Check: `package.json`, `requirements.txt`, `Cargo.toml` exist |
| **Lock versions** | Deterministic builds | Check: `package-lock.json`, `poetry.lock`, `Cargo.lock` committed |
| **No vulnerabilities** | Security audit passes | Run: `npm audit`, `pip-audit`, `cargo audit` (0 high/critical) |
| **Keep updated** | Dependencies not ancient | Check: Last updated < 6 months, use dependabot/renovate |
| **Evaluate before adding** | Maintenance, community, security reviewed | Document: Why this library vs alternatives |

**Add new dependency checklist:**
- [ ] Actively maintained (commits in last 6 months)
- [ ] Healthy community (stars, issues response time)
- [ ] No critical security issues
- [ ] Minimal transitive dependencies
- [ ] License compatible with project

**Example verification:**
```bash
# Check lock files are committed
git ls-files | grep -E "(package-lock\.json|poetry\.lock|Cargo\.lock)"

# Run security audit
npm audit --audit-level=high
# OR
pip-audit --require-hashes

# Check dependency freshness
npm outdated
```

### Documentation Standards Verification

**Before claiming documentation is complete:**

| Standard | Requirement | Verification |
|----------|-------------|--------------|
| **Accurate** | Reflects actual behavior | Test: Follow docs to use feature - does it work? |
| **Complete** | Covers all relevant aspects | Check: Public APIs, config options, examples documented |
| **Clear** | Understandable to target audience | Review: Someone unfamiliar can follow without confusion |
| **Current** | Updated with code changes | Check: Docs modified in same commit as code |

**Documentation types checklist:**

- [ ] **README** - Project purpose, setup, basic usage
- [ ] **API docs** - All public functions, parameters, return values
- [ ] **Inline comments** - WHY not WHAT (complex logic, workarounds, gotchas)
- [ ] **Examples** - Common use cases with working code
- [ ] **CHANGELOG** - User-facing changes documented

**Inline comment quality:**
```typescript
// ❌ BAD: Redundant with code (explains WHAT)
// Get user by ID
function getUserById(id: string): User { /*...*/ }

// ✅ GOOD: Explains WHY (adds context)
// Must query cache first to avoid rate limit on user API
// Cache miss penalty: 200ms, but prevents 429 errors
function getUserById(id: string): User { /*...*/ }
```

**API documentation quality:**
```typescript
// ❌ BAD: Missing details
/**
 * Retries an operation
 */
function retry(fn: Function): Promise<any>

// ✅ GOOD: Complete documentation
/**
 * Retries a failed async operation up to 3 times with exponential backoff.
 * 
 * @param fn - Async function to retry. Must return Promise.
 * @param options - Retry configuration (optional)
 * @param options.maxRetries - Maximum attempts (default: 3)
 * @param options.backoff - Backoff strategy: 'linear' | 'exponential' (default: 'exponential')
 * @returns Promise resolving to fn's result
 * @throws Last error if all retries exhausted
 * 
 * @example
 * const data = await retry(() => fetchUser(id), { maxRetries: 5 });
 */
function retry<T>(
  fn: () => Promise<T>, 
  options?: RetryOptions
): Promise<T>
```



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

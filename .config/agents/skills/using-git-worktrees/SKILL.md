---
name: using-git-worktrees
description: Use when starting feature work that needs isolation from current workspace or before executing implementation plans - creates isolated git worktrees with smart directory selection and safety verification
---

# Using Git Worktrees

## Overview

Git worktrees create isolated workspaces sharing the same repository, allowing work on multiple branches simultaneously without switching.

**Core principle:** Systematic directory selection + safety verification = reliable isolation.

**Announce at start:** "I'm using the using-git-worktrees skill to set up an isolated workspace."

## Directory Selection Process

Follow this priority order:

### 1. Check Existing Directories

```bash
# Check in priority order
ls -d .worktrees 2>/dev/null     # Preferred (hidden)
ls -d worktrees 2>/dev/null      # Alternative
```

**If found:** Use that directory. If both exist, `.worktrees` wins.

### 2. Check AGENTS.md

```bash
grep -i "worktree.*director" AGENTS.md 2>/dev/null
```

**If preference specified:** Use it without asking.

### 3. Ask User

If no directory exists and no AGENTS.md preference:

```
No worktree directory found. Where should I create worktrees?

1. .worktrees/ (project-local, hidden)
2. ~/.config/superpowers/worktrees/<project-name>/ (global location)

Which would you prefer?
```

## Safety Verification

### For Project-Local Directories (.worktrees or worktrees)

**MUST verify directory is ignored before creating worktree:**

```bash
# Check if directory is ignored (respects local, global, and system gitignore)
git check-ignore -q .worktrees 2>/dev/null || git check-ignore -q worktrees 2>/dev/null
```

**If NOT ignored:**

Per Jesse's rule "Fix broken things immediately":
1. Add appropriate line to .gitignore
2. Commit the change
3. Proceed with worktree creation

**Why critical:** Prevents accidentally committing worktree contents to repository.

### For Global Directory (~/.config/superpowers/worktrees)

No .gitignore verification needed - outside project entirely.

## Creation Steps

### 1. Detect Project Name

```bash
project=$(basename "$(git rev-parse --show-toplevel)")
```

### 2. Create Worktree

```bash
# Determine full path
case $LOCATION in
  .worktrees|worktrees)
    path="$LOCATION/$BRANCH_NAME"
    ;;
  ~/.config/superpowers/worktrees/*)
    path="~/.config/superpowers/worktrees/$project/$BRANCH_NAME"
    ;;
esac

# Create worktree with new branch
git worktree add "$path" -b "$BRANCH_NAME"
cd "$path"
```

### 3. Run Project Setup

Auto-detect and run appropriate setup:

```bash
# Node.js
if [ -f package.json ]; then npm install; fi

# Rust
if [ -f Cargo.toml ]; then cargo build; fi

# Python
if [ -f requirements.txt ]; then pip install -r requirements.txt; fi
if [ -f pyproject.toml ]; then poetry install; fi

# Go
if [ -f go.mod ]; then go mod download; fi
```

### 4. Verify Clean Baseline

Run tests to ensure worktree starts clean:

```bash
# Examples - use project-appropriate command
npm test
cargo test
pytest
go test ./...
```

**If tests fail:** Report failures, ask whether to proceed or investigate.

**If tests pass:** Report ready.

### 5. Report Location

```
Worktree ready at <full-path>
Tests passing (<N> tests, 0 failures)
Ready to implement <feature-name>
```

## Quick Reference

| Situation | Action |
|-----------|--------|
| `.worktrees/` exists | Use it (verify ignored) |
| `worktrees/` exists | Use it (verify ignored) |
| Both exist | Use `.worktrees/` |
| Neither exists | Check AGENTS.md → Ask user |
| Directory not ignored | Add to .gitignore + commit |
| Tests fail during baseline | Report failures + ask |
| No package.json/Cargo.toml | Skip dependency install |

## Common Mistakes

### Skipping ignore verification

- **Problem:** Worktree contents get tracked, pollute git status
- **Fix:** Always use `git check-ignore` before creating project-local worktree

### Assuming directory location

- **Problem:** Creates inconsistency, violates project conventions
- **Fix:** Follow priority: existing > AGENTS.md > ask

### Proceeding with failing tests

- **Problem:** Can't distinguish new bugs from pre-existing issues
- **Fix:** Report failures, get explicit permission to proceed

### Hardcoding setup commands

- **Problem:** Breaks on projects using different tools
- **Fix:** Auto-detect from project files (package.json, etc.)

## Example Workflow

```
You: I'm using the using-git-worktrees skill to set up an isolated workspace.

[Check .worktrees/ - exists]
[Verify ignored - git check-ignore confirms .worktrees/ is ignored]
[Create worktree: git worktree add .worktrees/auth -b feature/auth]
[Run npm install]
[Run npm test - 47 passing]

Worktree ready at /Users/jesse/myproject/.worktrees/auth
Tests passing (47 tests, 0 failures)
Ready to implement auth feature
```

## Red Flags

**Never:**
- Create worktree without verifying it's ignored (project-local)
- Skip baseline test verification
- Proceed with failing tests without asking
- Assume directory location when ambiguous
- Skip AGENTS.md check
- **Work directly on main/master branch** (create feature branch instead)
- **Skip pushing commits to remote** (push after EVERY commit)
- **Merge to main/master with failing tests** (fix tests first)
- **Create PR from main/master** (always use separate branch)

**Always:**
- Follow directory priority: existing > AGENTS.md > ask
- Verify directory is ignored for project-local
- Auto-detect and run project setup
- Verify clean test baseline
- **Create new branch for all changes** (via `git worktree add -b`)
- **Push after every commit** (backup + visibility)
- **Verify tests pass before merging** (keep main/master stable)
- **Use separate branch for PRs** (prevents later changes from polluting PR)

## Commit Discipline

Once worktree is ready, follow these commit practices:

### Branching Strategy

**CRITICAL: Always create a new branch for changes. NEVER work directly on main/master unless explicitly approved.**

```bash
# Worktree already creates a new branch via `git worktree add -b <branch-name>`
# This is the correct pattern - each feature/fix gets its own branch
```

**Why mandatory:**
- Keeps main/master stable and production-ready
- Allows work-in-progress without blocking others
- Enables clean PR workflow without pollution from later changes
- Makes rollback safe (can delete branch without affecting main)

### Commit Granularity

- **Create commits for each logical unit of work**
  - One feature/fix per commit
  - Keep commits focused and atomic
  - Avoid combining unrelated changes

### Commit Messages

**Follow the project's existing convention:**

1. **Check existing style:**
   ```bash
   git log --oneline -20
   ```

2. **Use the pattern you observe:**

   | Project Type | Convention | Example |
   |--------------|------------|---------|
   | **Versioned libraries/apps** | Semantic style | `feat: add JWT auth`, `fix: handle null user` |
   | **Dotfiles, configs, scripts** | Plain descriptive | `Add dark mode to vimrc`, `Fix typo in bashrc` |
   | **Mixed/unclear** | Ask user which to follow | — |

3. **Write descriptive messages:**
   - First line: concise summary (50 chars max)
   - Body (optional): explain WHY, not WHAT
   - Reference issues/tickets if applicable

### Push After Commit

**REQUIRED: Push to remote after each commit (if remote exists)**

```bash
# Check if remote is configured
git remote -v

# Push after EACH commit
git push origin <branch-name>

# First push needs -u flag
git push -u origin <branch-name>
```

**Why mandatory:**
- Backs up work immediately (prevents data loss)
- Makes progress visible to team
- Enables early feedback
- Creates backup before further changes
- Allows clean PR creation without later changes polluting the PR

**Frequency: After EVERY commit, not just at end of task.**

### Merge Discipline

**CRITICAL: Only merge to main/master after ALL verification passes:**

1. ✅ All tests passing
2. ✅ Code review approved (if applicable)
3. ✅ Build succeeds (if applicable)
4. ✅ Integration tests pass (if applicable)

**Never merge broken code to main/master.**

### PR Workflow

**When creating Pull Requests:**

1. **Always use a separate branch** (already done by worktree)
2. **Push branch before creating PR** (not main/master)
3. **PR from branch → main/master** (never commit directly to main after PR created)
4. **Benefit:** Later changes on main don't pollute the PR's change set

### History Management

**Before starting work:**
```bash
# Check for remote updates
git fetch origin
git status  # Check if branch is behind
```

**For rollbacks:**
```bash
# Preserve history (preferred)
git revert <commit>

# Undo commit but keep changes staged (use cautiously)
git reset --soft HEAD~1
```

**Key principle:** Git history should tell a clear story of what changed and why.

## Integration

**Called by:**
- **brainstorming** (Phase 4) - REQUIRED when design is approved and implementation follows
- **subagent-driven-development** - REQUIRED before executing any tasks
- **executing-plans** - REQUIRED before executing any tasks
- Any skill needing isolated workspace

**Pairs with:**
- **finishing-a-development-branch** - REQUIRED for cleanup after work complete

---
name: finishing-a-development-branch
description: Use when implementation is complete, all tests pass, and you need to decide how to integrate the work - guides completion of development work by presenting structured options for merge, PR, or cleanup
---

# Finishing a Development Branch

## Overview

Guide completion of development work by presenting clear options and handling chosen workflow.

**Core principle:** Verify tests → Present options → Execute choice → Clean up.

**Announce at start:** "I'm using the finishing-a-development-branch skill to complete this work."

## The Process

### Step 1: Verify Tests

**Before presenting options, verify tests pass:**

```bash
# Run project's test suite
npm test / cargo test / pytest / go test ./...
```

**If tests fail:**
```
Tests failing (<N> failures). Must fix before completing:

[Show failures]

Cannot proceed with merge/PR until tests pass.
```

Stop. Don't proceed to Step 2.

**If tests pass:** Continue to Step 2.

### Step 2: Determine Base Branch

```bash
# Try common base branches
git merge-base HEAD main 2>/dev/null || git merge-base HEAD master 2>/dev/null
```

Or ask: "This branch split from main - is that correct?"

### Step 3: Present Options

Present exactly these 4 options:

```
Implementation complete. What would you like to do?

1. Merge back to <base-branch> locally
2. Push and create a Pull Request
3. Keep the branch as-is (I'll handle it later)
4. Discard this work

Which option?
```

**Don't add explanation** - keep options concise.

### Step 4: Execute Choice

#### Option 1: Merge Locally

**CRITICAL: Only merge after ALL tests pass. Never merge broken code to main/master.**

```bash
# Switch to base branch
git checkout <base-branch>

# Pull latest
git pull

# Merge feature branch
git merge <feature-branch>

# REQUIRED: Verify tests on merged result
<test command>

# If tests FAIL - DO NOT PROCEED
# Checkout back to feature branch and fix
git checkout <feature-branch>
# Fix issues, commit, push
# Start over from beginning of Option 1

# If tests PASS - safe to finalize
git branch -d <feature-branch>

# Push merged result to remote
git push origin <base-branch>
```

**Red flag:** If tests fail after merge, the merge was premature. Fix on feature branch first.

Then: Cleanup worktree (Step 5)

#### Option 2: Push and Create PR

**CRITICAL: Always create PR from a separate branch, NEVER from main/master.**

```bash
# Verify you're on feature branch (not main/master)
current_branch=$(git branch --show-current)
if [[ "$current_branch" == "main" || "$current_branch" == "master" ]]; then
    echo "ERROR: Cannot create PR from main/master. Create feature branch first."
    exit 1
fi

# Push branch (if not already pushed)
git push -u origin <feature-branch>

# Create PR from feature branch → base branch
gh pr create --title "<title>" --body "$(cat <<'EOF'
## Summary
<2-3 bullets of what changed>

## Test Plan
- [ ] <verification steps>
EOF
)"
```

**Why mandatory separate branch:**
- Later commits to main/master won't pollute this PR's changes
- PR shows only the feature changes, not subsequent work
- Clean separation between work items

**After PR created:**
- Continue work on main/master for other tasks
- This PR remains clean with only feature-related commits
- Merge PR when approved (via GitHub UI or `gh pr merge`)

Then: Cleanup worktree (Step 5)

#### Option 3: Keep As-Is

Report: "Keeping branch <name>. Worktree preserved at <path>."

**Don't cleanup worktree.**

#### Option 4: Discard

**Confirm first:**
```
This will permanently delete:
- Branch <name>
- All commits: <commit-list>
- Worktree at <path>

Type 'discard' to confirm.
```

Wait for exact confirmation.

If confirmed:
```bash
git checkout <base-branch>
git branch -D <feature-branch>
```

Then: Cleanup worktree (Step 5)

### Step 5: Cleanup Worktree

**For Options 1, 2, 4:**

Check if in worktree:
```bash
git worktree list | grep $(git branch --show-current)
```

If yes:
```bash
git worktree remove <worktree-path>
```

**For Option 3:** Keep worktree.

## Quick Reference

| Option | Merge | Push | Keep Worktree | Cleanup Branch | Tests Required |
|--------|-------|------|---------------|----------------|----------------|
| 1. Merge locally | ✓ | ✓ | - | ✓ | ✅ Before & after merge |
| 2. Create PR | - | ✓ | ✓ | - | ✅ Before PR |
| 3. Keep as-is | - | - | ✓ | - | - |
| 4. Discard | - | - | - | ✓ (force) | - |

## Common Mistakes

**Skipping test verification**
- **Problem:** Merge broken code, create failing PR
- **Fix:** Always verify tests before offering options AND after merging

**Merging with failing tests**
- **Problem:** Broken code in main/master breaks production
- **Fix:** If tests fail after merge, checkout feature branch, fix, try again

**Creating PR from main/master branch**
- **Problem:** Later commits to main/master pollute the PR changes
- **Fix:** Always create PR from separate feature branch (never main/master)

**Open-ended questions**
- **Problem:** "What should I do next?" → ambiguous
- **Fix:** Present exactly 4 structured options

**Automatic worktree cleanup**
- **Problem:** Remove worktree when might need it (Option 2, 3)
- **Fix:** Only cleanup for Options 1 and 4

**No confirmation for discard**
- **Problem:** Accidentally delete work
- **Fix:** Require typed "discard" confirmation

## Red Flags

**Never:**
- Proceed with failing tests
- Merge without verifying tests on result
- Delete work without confirmation
- Force-push without explicit request
- **Merge to main/master with failing tests** (fix first, then merge)
- **Create PR from main/master branch** (use feature branch)
- **Skip post-merge test verification** (Option 1 requires tests after merge)

**Always:**
- Verify tests before offering options
- Present exactly 4 options
- Get typed confirmation for Option 4
- Clean up worktree for Options 1 & 4 only
- **Run tests AFTER merging** (Option 1 - verify merge didn't break anything)
- **Verify on feature branch before creating PR** (Option 2 - branch must be used)
- **Push merged main/master to remote** (Option 1 - backup changes)

## Integration

**Called by:**
- **subagent-driven-development** (Step 7) - After all tasks complete
- **executing-plans** (Step 5) - After all batches complete

**Pairs with:**
- **using-git-worktrees** - Cleans up worktree created by that skill

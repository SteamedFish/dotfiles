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

**IMPORTANT: Clean up commit history before merging to keep main/master readable.**

```bash
# Switch to base branch
git checkout <base-branch>

# Pull latest
git pull

# STEP 1: Rebase and clean up commits
# Rebase feature branch onto latest base to create linear history
git checkout <feature-branch>
git rebase <base-branch>

# Interactive rebase to clean up commit history
# This creates atomic, clear commits and hides:
# - Invalid attempts during development
# - Repeated modifications
# - Timeline chaos (commits out of logical order)
git rebase -i <base-branch>

# In the editor:
# - Squash/fixup related commits
# - Reorder commits to be logical
# - Reword commit messages to be clear
# - Result: Clean, atomic commits that tell a story

# STEP 2: Verify tests on rebased branch
<test command>

# If tests FAIL after rebase:
# - Fix issues on feature branch
# - Commit fixes
# - Re-run rebase cleanup if needed
# - Re-test until passing

# STEP 3: Merge rebased feature branch
git checkout <base-branch>
git merge <feature-branch>

# REQUIRED: Verify tests on merged result
<test command>

# If tests FAIL - DO NOT PROCEED
# Checkout back to feature branch and fix
git checkout <feature-branch>
# Fix issues, commit, push
# Start over from STEP 1

# If tests PASS - safe to finalize
# Mark branch as merged but KEEP it (don't delete yet)
# User will clean up old branches periodically based on their own policy
git branch --edit-description <feature-branch> "Merged to <base-branch> on $(date +%Y-%m-%d)"

# Push merged result to remote
git push origin <base-branch>
```

**Why rebase before merging:**
- Creates linear, readable history
- Removes development noise (failed attempts, debug commits)
- Makes commits atomic and logical
- Main/master stays clean and easy to understand

**Red flag:** If tests fail after rebase or merge, fix on feature branch first.

**Branch retention:** Merged branches are kept (not deleted) so user can review and clean up old branches on their own schedule.

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

**PR HYGIENE - CRITICAL:**

While PR is open and awaiting merge:
- **ONLY push commits related to this PR to the feature branch**
- **NEVER push unrelated work** to the feature branch
- If you need to work on other features:
  - Switch to main/master
  - Create a NEW branch for the new feature
  - Keep work isolated

**Why this matters:**
- Unrelated commits pollute the PR diff
- Reviewers see irrelevant changes
- PR loses focus and becomes harder to review
- Merge becomes complicated

**Correct workflow:**
```bash
# PR is open, need to work on something else
git checkout main
git checkout -b feature/other-work

# Work on other feature...
# This keeps PR branch clean
```

**After PR is merged by maintainer:**
- Mark branch as merged but KEEP it (don't delete yet)
- User will clean up old branches periodically based on their own policy
```bash
git branch --edit-description <feature-branch> "PR merged on $(date +%Y-%m-%d)"
```

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

| Option | Rebase | Merge | Push | Keep Worktree | Branch Retention | Tests Required |
|--------|--------|-------|------|---------------|------------------|----------------|
| 1. Merge locally | ✓ | ✓ | ✓ | - | ✓ (keep, mark as merged) | ✅ Before & after rebase & after merge |
| 2. Create PR | - | - | ✓ | ✓ | ✓ (keep, mark when merged) | ✅ Before PR |
| 3. Keep as-is | - | - | - | ✓ | ✓ | - |
| 4. Discard | - | - | - | - | - (delete) | - |

## Common Mistakes

**Skipping test verification**
- **Problem:** Merge broken code, create failing PR
- **Fix:** Always verify tests before offering options AND after rebase AND after merging

**Merging with failing tests**
- **Problem:** Broken code in main/master breaks production
- **Fix:** If tests fail after rebase or merge, checkout feature branch, fix, try again

**Skipping rebase cleanup before merge**
- **Problem:** Main/master history filled with "fix typo", "WIP", "oops" commits
- **Fix:** Always use `git rebase -i` to clean up commits before merging to main/master

**Creating PR from main/master branch**
- **Problem:** Later commits to main/master pollute the PR changes
- **Fix:** Always create PR from separate feature branch (never main/master)

**Pushing unrelated commits to PR branch**
- **Problem:** PR diff includes unrelated changes, confuses reviewers
- **Fix:** While PR is open, ONLY push commits related to that PR. Create new branch for other work.

**Deleting merged branches immediately**
- **Problem:** Can't review what was merged, no record of branch purpose
- **Fix:** Keep merged branches, mark them with description. User decides cleanup schedule.

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
- **Merge to main/master without rebase cleanup** (clean up commits first)
- **Create PR from main/master branch** (use feature branch)
- **Push unrelated commits to PR branch** (keep PR focused on single feature)
- **Delete merged branches immediately** (mark as merged, keep for user to clean up)
- **Skip post-merge test verification** (Option 1 requires tests after merge)
- **Skip post-rebase test verification** (Option 1 requires tests after rebase)

**Always:**
- Verify tests before offering options
- **Rebase and clean commits before merging to main/master** (Option 1)
- Present exactly 4 options
- Get typed confirmation for Option 4
- Clean up worktree for Options 1 & 4 only
- **Run tests AFTER rebasing** (Option 1 - verify rebase didn't break anything)
- **Run tests AFTER merging** (Option 1 - verify merge didn't break anything)
- **Verify on feature branch before creating PR** (Option 2 - branch must be used)
- **Keep PR branch focused** (Option 2 - only PR-related commits)
- **Mark merged branches but keep them** (let user decide cleanup schedule)
- **Push merged main/master to remote** (Option 1 - backup changes)

## Integration

**Called by:**
- **subagent-driven-development** (Step 7) - After all tasks complete
- **executing-plans** (Step 5) - After all batches complete

**Pairs with:**
- **using-git-worktrees** - Cleans up worktree created by that skill

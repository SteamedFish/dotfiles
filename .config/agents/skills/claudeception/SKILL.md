---
name: claudeception
description: Use when a work session produced non-obvious debugging discoveries, reusable workarounds, or project-specific patterns that should be extracted into reusable skills.
---

# Claudeception

## Overview

Claudeception is a meta-skill for continuous learning. After solving real tasks, it extracts reusable knowledge into new skills so future sessions avoid relearning the same things.

## When to Use

Use this skill when any of the following appears:
- A fix required non-obvious investigation or trial-and-error.
- An error message was misleading and the real root cause differed.
- You discovered project-specific conventions not already documented.
- You found a repeatable workflow optimization.
- The user explicitly asks to "save this as a skill" or run a retrospective.

Do not use this for trivial doc lookups or one-off local hacks.

## Extraction Workflow

### 1) Check Existing Skills First

```sh
# Prefer project-first, then user-level paths.
SKILL_DIRS=(
  ".config/agents/skills"
  "$HOME/.config/agents/skills"
  "$HOME/.codex/skills"
)

# List all skills.
rg --files -g 'SKILL.md' "${SKILL_DIRS[@]}" 2>/dev/null

# Search by problem keywords, exact errors, or context markers.
rg -i "keyword1|keyword2" "${SKILL_DIRS[@]}" 2>/dev/null
rg -F "exact error message" "${SKILL_DIRS[@]}" 2>/dev/null
```

Decision rule:
- No related skill: create new.
- Same trigger + same fix: update existing.
- Same trigger + different root cause: create new and cross-link.
- Partial overlap: extend existing with a variant section.

### 2) Capture the Knowledge

Document:
- Problem and trigger conditions.
- Non-obvious root cause.
- Exact fix that worked.
- Verification steps and expected success signal.

### 3) Validate Freshness (When Needed)

For technology-specific patterns, check current primary docs before writing the skill.
Skip web lookup for purely internal project conventions.

### 4) Write the New Skill

Create `.config/agents/skills/<skill-name>/SKILL.md` with:
- `name` and `description` frontmatter.
- Problem, trigger conditions, solution, verification, notes.
- Concrete, searchable wording in description.

### 5) Keep Skill Quality High

Require all of:
- Reusable beyond a single incident.
- Non-trivial (not obvious from first-pass docs).
- Specific triggers and actionable steps.
- Verified in practice.
- No secrets or internal sensitive data.

## Retrospective Mode

At end of meaningful sessions:
1. Review what was learned.
2. List extraction candidates.
3. Prioritize highest reuse value.
4. Extract 1-3 skills max.
5. Summarize what was created and why.

## Quick Checklist

- [ ] Checked for overlapping existing skills.
- [ ] Description includes concrete triggers.
- [ ] Solution verified with evidence.
- [ ] Skill is reusable and non-trivial.
- [ ] No sensitive information included.
- [ ] Optional references added for tech-specific claims.

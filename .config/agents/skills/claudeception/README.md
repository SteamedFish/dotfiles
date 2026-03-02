# Claudeception (Agent-Generic Edition)

Claudeception is a continuous-learning skill for coding agents. It extracts reusable discoveries from completed tasks and stores them as new skills.

## Installed Location

This repository imports Claudeception to:

- `.config/agents/skills/claudeception/`

## Key Adaptations

- Replaced Claude-specific paths with `.config/agents/skills`.
- Rewrote instructions to be agent-generic instead of Claude-only.
- Kept examples/resources for reference while standardizing the main skill to local conventions.

## Manual Usage

- Use this skill at the end of debugging-heavy or discovery-heavy sessions.
- Ask for retrospective extraction, e.g. `save this as a skill`.

## Optional Reminder Script

If your agent runtime supports prompt hooks, you can reuse:

- `scripts/claudeception-activator.sh`

Hook wiring is runtime-specific and intentionally not hardcoded here.

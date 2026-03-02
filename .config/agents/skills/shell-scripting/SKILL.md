---
name: shell-scripting
description: Use when writing bash/shell scripts - covers structured output, dependency management, command variants (GNU vs BSD), and avoiding reinvented wheels
---

# Shell Scripting

## Overview

Write robust, portable, dependency-aware shell scripts. **Core principle:** Prefer structured output over text parsing, check dependencies explicitly, use existing commands over custom implementations.

## When to Use

**Use this skill when:**
- Writing any bash/shell script (automation, system admin, build scripts)
- Parsing command output
- Cross-platform shell scripting (Linux/macOS/BSD)

**Symptoms:**
- About to use `grep`, `sed`, `awk`, `cut` to parse command output
- Adding external command calls without checking existence
- Writing custom logic that might already exist as a command

## Core Principles

### 1. Option Style: Readability First, POSIX When Portable

**MANDATORY: Use long options (`--option`) for readability, EXCEPT when short option is POSIX standard and long option is GNU-only extension.**

**Why long options:**
- Self-documenting: `tar --create --gzip --file` is clearer than `tar -czf`
- Maintenance-friendly: Future readers understand intent immediately
- Less error-prone: No memorizing flag meanings

**Decision tree:**
```
Is there a --long option? 
  NO  → Use short option
  YES → Is short option POSIX standard and long option GNU-only?
    YES → Use POSIX short (portable across Linux/BSD/macOS)
    NO  → Use GNU long (readability wins)
```

**Quick examples:**

| Command | Use This | Not This | Why |
|---------|----------|----------|-----|
| `ls` | `ls -a -l -h` | `ls --all --long` | POSIX short, GNU long |
| `grep` | `grep -r -i` | `grep --recursive --ignore-case` | POSIX short, GNU long |
| `tar` | `tar --create --gzip --file=x.tar.gz` | `tar -czf x.tar.gz` | Both support long, clarity wins |
| `mkdir` | `mkdir --parents` | `mkdir -p` | Widely supported long option |
| `find` | `find --type f --name "*.log"` | `find -type f -name "*.log"` | GNU long preferred |

**Acceptable short options:**
- No long equivalent: `tar -C`, `find -print0`
- Standard idioms: `set -euo pipefail`, `rm -rf`
- POSIX standard when long is GNU-only: `-a`, `-l`, `-h`, `-r`, `-i`, `-E`, `-n`, `-v`

**NOT acceptable excuses:**
- "Just a one-liner" — One-liners need clarity too
- "Urgent/emergency" — Clarity matters MOST in emergencies
- "Editing old code" — New code uses proper style
- "Faster to type" — 2 seconds typing vs hours debugging

**Self-check (4 steps):**
1. Does `--long` version exist? Check `man command`
2. If YES: Is `-short` POSIX and `--long` GNU-only? Check BSD man page
3. If POSIX + GNU-only: Use POSIX short (portability)
4. Otherwise: Use long option (readability)

### 2. Structured Output Over Text Parsing

Commands often provide machine-readable formats. **Always check before parsing.**

| ❌ Text Parsing | ✅ Structured Output |
|----------------|---------------------|
| `lsblk \| tail -n +3 \| cut -d' ' -f1` | `lsblk --json --output name,size` |
| `df -h \| grep pattern \| awk '{print $5}'` | `df --output=source,pcent` |
| `ps aux \| grep pattern \| awk '{print $2}'` | `pgrep -f pattern` |

**Check for these flags:**
- `-J`, `--json`: JSON output (lsblk, ip, systemctl)
- `--output=`: Column selection (df, ps, dpkg)
- `-0`, `-z`: Null-delimited (find, xargs, grep)
- `-n`, `--noheadings`: Remove headers (lsblk, lvs, pvs)

### 3. Use Existing Commands First

Before writing custom logic, search for built-in commands.

| ❌ Custom | ✅ Existing |
|----------|------------|
| `basename "$part" \| sed 's/[0-9]*$//'` | `lsblk --noheadings --output pkname "$part"` |
| Loop to count files | `find . --type f -printf '.' \| wc --bytes` |
| Parse /proc/meminfo | `free --bytes` or `vmstat -s` |

**How to discover:**
```bash
man command                    # Read manual for options
apropos "keyword"              # Search for related tools
command --help | grep json     # Check for structured output
```

### 4. Check Dependencies Explicitly

**Pattern:**
```bash
#!/usr/bin/env bash
set -euo pipefail

# Required: jq, curl, lsblk
# Optional: notify-send (desktop notifications)

for cmd in jq curl lsblk; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd required" >&2
        echo "Install: sudo apt install $cmd" >&2
        exit 1
    fi
done
```

### 5. Handle GNU vs BSD Variants

**Don't restrict to POSIX-only. Use GNU features when beneficial, but handle portability.**

**GNU-required pattern:**
```bash
# This script requires GNU coreutils
# macOS: brew install coreutils

if ! stat --version 2>/dev/null | grep --quiet GNU; then
    echo "Error: GNU stat required" >&2
    echo "macOS: brew install coreutils" >&2
    exit 1
fi
```

**Cross-platform pattern:**
```bash
if stat --version 2>/dev/null | grep --quiet GNU; then
    file_time=$(stat --format='%Y' "$file")  # GNU
else
    file_time=$(stat -f '%m' "$file")         # BSD/macOS
fi
```

## Quick Reference

### POSIX vs GNU Options

**POSIX short options (use these for portability):**

| Command | POSIX Options | Avoid GNU Long |
|---------|--------------|----------------|
| `ls` | `-a -l -h -t -r` | `--all --long --human-readable` |
| `grep` | `-r -i -E -n -v` | `--recursive --ignore-case` |
| `df` | `-h -T` | `--human-readable --type` |
| `ps` | `-e -f -o` | (no long options in POSIX) |

**GNU long options (use these when both support or GNU-only):**

| Command | Preferred | Reason |
|---------|-----------|--------|
| `tar` | `--create --gzip --file` | Both GNU/BSD support, clarity wins |
| `mkdir` | `--parents` | Widely available |
| `find` | `--printf` | GNU-only feature, no POSIX equivalent |

### Structured Output Commands

| Command | Flag | Example |
|---------|------|---------|
| `lsblk` | `-J` (JSON) | `lsblk --json --output name,size` |
| `ip` | `-j` (JSON) | `ip --json addr show` |
| `df` | `--output=` | `df --output=source,pcent` |
| `ps` | `-o` | `ps -eo pid,comm --no-headers` |
| `find` | `-print0` | `find . -print0 \| xargs -0` |

### Dependency Check

```bash
# Single command
command -v jq &>/dev/null || { echo "jq required" >&2; exit 1; }

# Multiple commands
for cmd in jq curl awk; do
    command -v "$cmd" &>/dev/null || { echo "$cmd required" >&2; exit 1; }
done
```

### GNU vs BSD Detection

```bash
# Check for GNU
if cmd --version 2>/dev/null | grep --quiet GNU; then
    # GNU code
else
    # BSD code
fi
```

## Common Mistakes

### ❌ Parsing ls output
```bash
for file in $(ls *.txt); do echo "$file"; done  # BREAKS on spaces
```
**Fix:** Use globs or `find -print0`

### ❌ Assuming command exists
```bash
result=$(echo '{}' | jq -r '.key')  # Fails silently if jq missing
```
**Fix:** Check with `command -v jq` first

### ❌ Text parsing when structured exists
```bash
df -h | grep sda1 | awk '{print $5}'  # Fragile
```
**Fix:** `df --output=pcent /dev/sda1`

### ❌ Reinventing functionality
```bash
for i in {1..5}; do curl "$url" && break; sleep 2; done  # Custom retry
```
**Fix:** `timeout 30 bash -c 'until curl "$1"; do sleep 2; done' _ "$url"`

## Red Flags

**STOP if you're about to:**
- Use short option → Check: POSIX standard? Then OK. Otherwise use `--long`.
- Use `grep`/`sed`/`awk` on output → Check for `--json` or `--output=` first
- Call external command → Add existence check
- Type "urgent" or "simple one-liner" → That's when clarity matters MOST
- Parse device names with regex → Check `lsblk`, `blkid`, `findmnt` first
- Write 10+ lines custom logic → Search `man` and `apropos` first

## Common Rationalizations

| Excuse | Reality |
|--------|---------|
| "Short options faster to type" | 2 sec typing vs hours debugging later |
| "Just a one-liner" | One-liners get read and debugged too |
| "Emergency situation" | Emergencies need clarity MOST |
| "Existing code uses short" | New code uses proper style |
| "Regex simpler than JSON" | Regex breaks across versions, JSON doesn't |
| "Don't need dependency check" | Explicit errors save hours of debugging |
| "Text parsing works in tests" | Production has different locales/versions |
| "Already implemented, testing enough" | Testing proves now, not future-proof |

## Real-World Impact

**Patterns this skill addresses:**
- Text parsing → Structured output (JSON, `--output=`)
- No dependency docs → Explicit checks with install hints
- Platform assumptions → GNU vs BSD detection
- Reinvented wheels → Use existing commands first

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
- Adding script dependencies
- Cross-platform shell scripting (Linux/macOS/BSD)

**Symptoms that trigger this skill:**
- About to use `grep`, `sed`, `awk`, `cut` to parse command output
- Adding external command calls without checking existence
- Manually extracting fields from text output
- Writing custom logic that might already exist as a command

## Core Principles

### 1. Prefer Structured Output Over Text Parsing

Commands often support machine-readable formats (JSON, CSV, null-delimited). **Always check if available.**

| ❌ Text Parsing | ✅ Structured Output |
|----------------|---------------------|
| `lsblk \| tail -n +3 \| cut -d' ' -f1` | `lsblk -no name,size -J` (JSON) |
| `df -h \| grep -v '^Filesystem' \| awk '{print $1,$5}'` | `df --output=source,pcent` |
| `ps aux \| grep pattern \| awk '{print $2}'` | `pgrep -f pattern` or `ps -o pid= -C command` |
| `find . -name "*.sh" \| wc -l` | `find . -name "*.sh" -printf '.' \| wc -c` |

**Why this matters:**
- Text formats change between versions/distros
- Whitespace, locales, and formatting break parsing
- Structured output is version-stable

**Command options to check:**
- `-J` or `--json`: JSON output (lsblk, ip, systemctl, etc.)
- `--output=`: Select specific columns (df, ps, dpkg)
- `-0` or `-z`: Null-delimited output (find, xargs, grep)
- `-n` or `--noheadings`: Remove headers for clean parsing (lsblk, lvs, pvs)
- `-o` or `--format`: Custom format strings (ps, date, git)

### 2. Find Existing Commands Before Implementing

**Before writing custom logic, search for built-in commands or flags.**

| ❌ Custom Implementation | ✅ Existing Command/Flag |
|-------------------------|-------------------------|
| `basename "$partition" \| sed 's/[0-9]*$//'` | `lsblk -no pkname "$partition"` |
| `echo "$string" \| tr '[:upper:]' '[:lower:]'` | `printf '%s\n' "${string,,}"` (bash 4+) |
| Loop to count files | `find . -type f -printf '.' \| wc -c` |
| Parsing /proc/meminfo | `free -b` or `vmstat -s` |
| Custom retry loop | `timeout` + `until`/`while` pattern |

**How to discover:**
```bash
# Read the manual for related commands
man lsblk  # Look for -o, --output options
man find   # Check -printf, -print0 options

# Search for similar tools
apropos "list block"
apropos "partition"

# Check command help for machine-readable options
command --help | grep -E 'json|output|format|print'
```

### 3. Track and Check Dependencies

**ALWAYS document required and optional dependencies.**

**At script beginning:**
```bash
#!/usr/bin/env bash

# Required dependencies: jq, curl, lsblk
# Optional dependencies: notify-send (for desktop notifications)

set -euo pipefail

# Check required dependencies
for cmd in jq curl lsblk; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: Required command '$cmd' not found" >&2
        echo "Install with: sudo apt install $cmd" >&2  # Adjust per distro
        exit 1
    fi
done

# Check optional dependencies
if ! command -v notify-send &>/dev/null; then
    echo "Warning: notify-send not found. Desktop notifications disabled." >&2
fi
```

**In documentation/comments:**
```markdown
## Dependencies

### Required
- `jq` (JSON processor) - Install: `sudo apt install jq` (Debian/Ubuntu), `brew install jq` (macOS)
- `curl` (HTTP client) - Usually pre-installed
- `lsblk` (block device info) - Part of util-linux package

### Optional
- `notify-send` (desktop notifications) - Install: `sudo apt install libnotify-bin`
```

### 4. Handle GNU vs BSD Command Variants

**You don't need POSIX-only scripts. Use GNU features when beneficial, but check and guide installation.**

**GNU-specific features commonly used:**
- `sed -i` (in-place editing)
- `find -printf` (custom output format)
- `grep -P` (Perl regex)
- `date -d` (relative date parsing)
- `stat -c` (custom format)

**Pattern for GNU-required scripts:**

```bash
#!/usr/bin/env bash

# This script requires GNU coreutils (not BSD)
# macOS users: brew install coreutils && export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Verify GNU version
if ! stat --version 2>/dev/null | grep -q GNU; then
    echo "Error: This script requires GNU stat" >&2
    echo "macOS: brew install coreutils" >&2
    echo "Then add to PATH: export PATH=\"/usr/local/opt/coreutils/libexec/gnubin:\$PATH\"" >&2
    exit 1
fi

# Now safe to use GNU-specific features
stat -c '%Y' "$file"  # GNU format specifier
```

**Cross-platform approach (when feasible):**

```bash
# Detect GNU vs BSD and adapt
if stat --version 2>/dev/null | grep -q GNU; then
    # GNU stat
    file_time=$(stat -c '%Y' "$file")
else
    # BSD stat (macOS)
    file_time=$(stat -f '%m' "$file")
fi
```

## Common Rationalizations

**Why agents skip these principles and what the reality is:**

| Excuse | Reality |
|--------|---------|
| "Regex is simpler than checking for JSON flags" | JSON flags are in `--help`. Reading docs takes 10 seconds. Regex breaks across versions. |
| "Not worth adding dependency check for common commands" | "Common" varies by environment. 30-second check prevents cryptic production failures. |
| "User will know to install dependencies" | They won't. Explicit errors with install commands save hours of debugging. |
| "Text parsing works fine in my tests" | Your environment isn't production. Different locale/version breaks it. |
| "GNU vs BSD doesn't matter for this script" | It matters when user is on macOS and script crashes. Check early or fail gracefully. |
| "Don't need lsblk, can parse device names" | NVMe naming (nvme0n1p1), md arrays, LVM break string patterns. lsblk knows all cases. |
| "Already implemented it, testing is enough" | Testing proves it works now. Doesn't prove it won't break. Structured output is future-proof. |
| "Adding jq is overhead" | Shell text parsing is more overhead. jq is robust, widely available, purpose-built. |

## Red Flags - STOP and Reconsider

**If you're about to:**
- Use `grep`/`sed`/`awk`/`cut` on command output → Check if command has `-J`, `--output=`, or `-o` flag first
- Call external command → Add existence check and installation guide
- Parse device names with regex → Check if `lsblk`, `blkid`, or `findmnt` already provides the info
- Assume GNU command exists → Check `--version | grep GNU` or provide BSD alternative
- Write 10+ lines of custom logic → Search `man` and `apropos` for existing command first

**All of these mean: Read the command's manual page before implementing.**

## Common Mistakes

### ❌ Parsing ls output
```bash
# NEVER do this
for file in $(ls *.txt); do
    echo "$file"
done
```

**Why:** Breaks on spaces, newlines, special characters.

**Fix:**
```bash
# Use globs or find
for file in *.txt; do
    echo "$file"
done

# Or find with null-delimiter
find . -name "*.txt" -print0 | while IFS= read -r -d '' file; do
    echo "$file"
done
```

### ❌ Assuming command exists
```bash
# Fails silently if jq not installed
result=$(echo '{"key":"value"}' | jq -r '.key')
```

**Fix:**
```bash
if ! command -v jq &>/dev/null; then
    echo "Error: jq required but not installed" >&2
    exit 1
fi
result=$(echo '{"key":"value"}' | jq -r '.key')
```

### ❌ Regex when structured option exists
```bash
# Fragile - breaks if format changes
df -h | grep '/dev/sda1' | awk '{print $5}' | tr -d '%'
```

**Fix:**
```bash
# Robust - uses structured output
df --output=pcent /dev/sda1 | tail -n1 | tr -d ' %'
```

### ❌ Reinventing existing functionality
```bash
# Custom retry with sleep
for i in {1..5}; do
    if curl "$url"; then
        break
    fi
    sleep 2
done
```

**Fix:**
```bash
# Use timeout + until pattern
timeout 30 bash -c 'until curl "$1"; do sleep 2; done' _ "$url"
```

## Quick Reference

### Structured Output Commands

| Command | Structured Format | Example |
|---------|------------------|---------|
| `lsblk` | `-J` (JSON), `-P` (key=value), `-no` (no headers) | `lsblk -J -o name,size,type` |
| `ip` | `-j` (JSON) | `ip -j addr show` |
| `df` | `--output=` (column selection) | `df --output=source,pcent,target` |
| `ps` | `-o` (custom format) | `ps -eo pid,comm,pcpu --no-headers` |
| `systemctl` | `--output=json` | `systemctl show --output=json unit.service` |
| `find` | `-print0`, `-printf` | `find . -type f -printf '%p\t%s\n'` |
| `jq` | JSON processor | `echo '{"a":1}' \| jq -r '.a'` |

### Dependency Check Pattern

```bash
# Simple check
command -v cmd &>/dev/null || { echo "cmd required" >&2; exit 1; }

# With installation hint
if ! command -v jq &>/dev/null; then
    echo "Error: jq required" >&2
    echo "Install: sudo apt install jq" >&2
    exit 1
fi

# Array-based for multiple deps
REQUIRED_DEPS=(jq curl awk)
for dep in "${REQUIRED_DEPS[@]}"; do
    command -v "$dep" &>/dev/null || {
        echo "Error: $dep required" >&2
        exit 1
    }
done
```

### GNU vs BSD Detection

```bash
# Check for GNU version
if cmd --version 2>/dev/null | grep -q GNU; then
    # GNU-specific code
else
    # BSD-specific code
fi

# Or fail early if GNU required
if ! stat --version 2>/dev/null | grep -q GNU; then
    echo "GNU stat required" >&2
    echo "macOS: brew install coreutils" >&2
    exit 1
fi
```

## Real-World Impact

**Observed anti-patterns this skill addresses:**
- Text parsing (`lsblk | tail | cut`) → `lsblk -J` (JSON)
- No dependency docs → Explicit checks with install commands
- Platform assumptions → GNU vs BSD detection
- Reinvented wheels → Use `lsblk -no pkname` over regex

# DOTFILES KNOWLEDGE BASE

**Generated:** 2026-02-06 11:02 UTC  
**Commit:** (will be updated after final commit)  
**Branch:** master

## OVERVIEW
Personal Unix/Linux dotfiles. Shell (bash/zsh), Emacs, Neovim, ~40 XDG app configs.

**Recent Improvements:**

*2026-02-07:*
- **Reorganized all Emacs leaf statements**: Builtin packages now consistently ordered first, followed by third-party packages grouped by category
- **Added leaf-convert.el**: Tool for converting use-package declarations to leaf (https://github.com/conao3/leaf-convert.el)
- **Fixed deprecated URLs**: Updated raxod502 → radian-software for straight.el, blackout, and prescient packages
- **Removed company-lsp**: Cleaned up deprecated package (LSP no longer supports company-lsp)
- **Improved organization**: Applied consistent categorization across init-packages.el, init-setups.el, init-keybindings.el, init-company.el, init-ivy.el, init-edittools.el, init-helpers.el, and all language-specific files

*2026-02-06:*
- Fixed critical bugs in shell scripts (duplicate declarations, PATH issues)
- Refactored duplicate code for better maintainability
- Added shellcheck compliance across all bash scripts
- Verified all 42 elisp files pass byte-compilation
- Improved code quality and readability throughout

## STRUCTURE
```
./
├── .bashrc.d/       # Modular shell config (cleaned & optimized)
├── .config/         # XDG configs
│   └── emacs/       # Active Emacs config (42 files, all verified)
├── .local/bin/      # Scripts (mostly deprecated - see ANTI-PATTERNS)
│   └── update       # System update script (recently fixed)
├── .Brewfile        # macOS package manager
├── .bashrc/.zshrc   # Shell entry points (source .bashrc.d/*)
└── .[tool]rc        # Tool configs (tmux, git, vim fallback)
```

## CODE QUALITY

### Shell Scripts - All Verified
- **Shellcheck compliant**: All scripts pass shellcheck validation
- **No critical issues**: Fixed all SC2xxx errors
- **Consistent patterns**: Unified coding style across all scripts
- **Proper scoping**: All function variables properly declared as `local`

### Elisp Files - All Verified & Organized
- **Byte-compilation**: All 42 files pass without errors/warnings
- **Lexical binding**: All files use `lexical-binding: t`
- **Consistent organization**: Builtin packages first (`:tag "builtin"`), then third-party grouped by purpose
- **Modern practices**: Proper use of leaf macros (NOT use-package)
- **No deprecated code**: Updated all URLs, removed company-lsp
- **leaf-convert.el added**: Tool available for converting use-package → leaf

## WHERE TO LOOK

| Task | Location | Notes |
|------|----------|-------|
| Shell customization | `.bashrc.d/` | Modular: aliases, colors, prompts, OS-specific |
| Emacs config | `.config/emacs/` | Custom config, NOT doom/spacemacs |
| Neovim config | `.config/nvim/` | LazyVim-based |
| App configs | `.config/{app}/` | Per-app: alacritty, git, tmux, btop, etc. |
| Bootstrap | `.bashrc`, `.zshrc` | Entry points source `.bashrc.d/*` |
| Packages | `.Brewfile` | macOS dependencies |
| Custom scripts | `.local/bin/` | **Mostly deprecated** - see ANTI-PATTERNS |

## CONVENTIONS

### Shell Initialization
- `.zshrc` sources `.bashrc` (unified config)
- `.bashrc` sources all `.bashrc.d/*.sh` (modular)
- Editor priority: `nvim > vim`
- PATH additions: `~/.go/bin`, `~/.cargo/bin`, `~/.local/bin`

### Prompt
- bash: powerline or starship (`.bashrc.d/powerline.sh`, `.bashrc.d/starship.sh`)
- zsh: p10k or starship

### File Organization
- **Active configs**: Check modification timestamp when conflicts exist
- **XDG compliance**: Apps use `.config/` when possible

## RECENT FIXES (2026-02-06)

### Bash Scripts
1. **.local/bin/update**: Removed duplicate `set -e`, added `local` declarations for PACMAN_CMD
2. **.bashrc.d/aliases.sh**: Refactored bat/batcat setup into reusable `_setup_bat_aliases()` function (30 lines → 15 lines)
3. **.bashrc.d/bash.sh**: Removed duplicate HISTCONTROL and HISTTIMEFORMAT declarations
4. **.bashrc.d/linux.sh**: Fixed PATH concatenation missing colon, fixed MAKEFLAGS export pattern
5. **.bashrc.d/emacs.sh**: Replaced subshell with brace grouping in vterm_printf for performance
6. **All scripts**: Added shellcheck directives for intentional patterns, verified syntax

### Elisp Files (2026-02-07 Update)
- **Reorganized all leaf statements**: Consistent ordering across all 42 files
  - Pattern: Builtin packages (`:tag "builtin"`) → Third-party (grouped by category)
  - Files reorganized: init-packages.el, init-setups.el, init-keybindings.el, init-company.el, init-ivy.el, init-edittools.el, init-helpers.el, plus all language files
- **Added leaf-convert.el** to init-packages.el for use-package → leaf conversion
- **Fixed deprecated URLs**: raxod502 → radian-software for straight.el, blackout, prescient
- **Removed company-lsp**: Deprecated package completely removed from init-lsp.el
- **Verification**: All files maintain 100% clean byte-compilation status

## ANTI-PATTERNS (THIS REPO)

### Deprecated Directories
**DO NOT reference or modify:**
- `.local/bin/{gssh,macflush,netease-*,set-proxy.sh}` - Old, unused for years
- `.local/lib/ansible` - Experimental, never used

### Duplicate Configs
**Multiple Emacs configs exist:**
- ✅ **ACTIVE**: `.config/emacs/` (42 files, custom config, fully verified)
- ❌ **DEPRECATED**: `.config/doom/` (50 files, old doom-emacs)
- ❌ **DEPRECATED**: `.spacemacs.d/` (5 files, old spacemacs)

**Rule**: When conflicts exist, **recently modified = active**.

## ACTIVE EMACS CONFIG
`.config/emacs/` is the ONLY active Emacs configuration. Ignore doom/spacemacs dirs.

## COMMANDS

### Bootstrap (if applicable)
```bash
# Usually manual symlinking or stow
# Check for install.sh/Makefile if present
```

### Shell reload
```bash
source ~/.bashrc  # or
source ~/.zshrc
```

### Verify code quality
```bash
# Check bash scripts
shellcheck -x .bashrc .zshrc .bashrc.d/*.sh .local/bin/update

# Check elisp files
cd .config/emacs && emacs --batch --eval "(byte-recompile-directory \"$(pwd)\" 0 t)"
```

## NOTES

### OS-Specific
- `.bashrc.d/mac.sh` - macOS-specific (Homebrew, etc.)
- `.bashrc.d/linux.sh` - Linux-specific
- `.bashrc.d/wsl.sh` - WSL-specific

### Depth Warning
- 9 levels deep in places
- 58K+ LOC total

### Symlinks
- Some configs may be symlinked from this repo to `~/`
- Check actual paths if editing fails


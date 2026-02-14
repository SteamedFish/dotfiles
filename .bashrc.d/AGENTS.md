# .bashrc.d KNOWLEDGE BASE

**Generated:** 2026-02-06 03:09 UTC

## OVERVIEW
Modular bash/zsh configuration system. 14 modules covering aliases, colors, OS-specific, prompts.

## STRUCTURE
```
.bashrc.d/
├── aliases.sh      # Command aliases (~140 aliases)
├── android.sh      # Android SDK paths
├── bash.sh         # Bash-specific (history, completion)
├── color.sh        # Color definitions, ls colors
├── context.sh      # Context detection (OS, terminal)
├── emacs.sh        # Emacs integration, daemon management
├── linux.sh        # Linux-specific (clipboard, display)
├── mac.sh          # macOS-specific (Homebrew, iTerm2)
├── powerline.sh    # Powerline-go prompt
├── starship.sh     # Starship prompt
├── wsl.sh          # WSL-specific
└── zsh.sh          # Zsh-specific (oh-my-zsh, completion)
```

## WHERE TO LOOK

| Task | File | Notes |
|------|------|-------|
| Add alias | `aliases.sh` | Organized by category (git, docker, etc.) |
| OS detection | `context.sh` | Sets IS_MAC, IS_LINUX, IS_WSL |
| Prompt config | `powerline.sh` / `starship.sh` | Mutually exclusive |
| Emacs integration | `emacs.sh` | Daemon start, aliases (e, ec, etc.) |
| macOS settings | `mac.sh` | Homebrew, iTerm2 shell integration |
| Linux settings | `linux.sh` | Display, clipboard, package managers |

## CONVENTIONS

### Loading Order
- Sourced alphabetically by `.bashrc`
- `context.sh` should run early (sets OS vars)
- OS-specific files check context vars

### Alias Style
- Short mnemonics: `g` = git, `d` = docker
- Safety aliases: `rm -i`, `cp -i`, `mv -i`
- Colorized output: `ls --color=auto`, `grep --color=auto`

### OS Detection
```bash
# Set in context.sh, used by other modules
IS_MAC, IS_LINUX, IS_WSL, IN_SSH, IN_DOCKER
```

## ANTI-PATTERNS

### Do Not
- Add global exports here (use `.bashrc` or `.profile`)
- Source external files without existence checks
- Assume GNU tools on macOS (use `g` prefix or check)

## NOTES

### Emacs Integration
- `emacs.sh` manages emacsclient daemon
- Aliases: `e` (editor), `ec` (emacsclient), `ef` (find-file)
- Daemon auto-starts if not running

### Prompt Priority
- Starship takes precedence if installed
- Powerline-go fallback
- Both modules check for binary existence

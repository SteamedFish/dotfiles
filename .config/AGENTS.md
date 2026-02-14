# .config KNOWLEDGE BASE

**Generated:** 2026-02-06 03:09 UTC

## OVERVIEW
XDG config directory. ~40 application configs: terminal, editor, shell, system tools.

## STRUCTURE
Notable apps (not exhaustive):
```
.config/
├── alacritty/       # Terminal emulator
├── atuin/           # Shell history
├── btop/            # System monitor
├── doom/            # DEPRECATED Doom Emacs
├── doom-nvim/       # Neovim config
├── emacs/           # ACTIVE Emacs (see AGENTS.md there)
├── fastfetch/       # System info
├── fcitx5/          # Input method
├── git/             # Git config
├── nvim/            # Neovim (LazyVim)
├── starship/        # Prompt
├── systemd/         # User services
└── tmux/            # Terminal multiplexer
```

## WHERE TO LOOK

| Task | Location | Notes |
|------|----------|-------|
| Terminal config | `alacritty/`, `wezterm/` | Alacritty active |
| Shell prompt | `starship/` | Used by both bash/zsh |
| Editor configs | `emacs/`, `nvim/` | Emacs active, nvim fallback |
| Git settings | `git/` | User config, aliases |
| System monitor | `btop/`, `bashtop/` | btop newer |
| Input method | `fcitx5/` | Chinese/multilingual |

## CONVENTIONS

### XDG Compliance
- Apps follow `$XDG_CONFIG_HOME` standard
- Most configs are symlinked from dotfiles repo to `~/.config/`

### Active vs Deprecated
- **Emacs**: `.config/emacs/` ACTIVE, `doom/` deprecated
- **System monitor**: `btop/` active, `bashtop/` older
- Check file modification timestamps when uncertain

## ANTI-PATTERNS

### Do Not
- Modify `.config/doom/` - DEPRECATED (old Doom Emacs)
- Edit configs assuming GNU tools on macOS (may need Homebrew versions)

## NOTES

### Input Method
- fcitx5 for Chinese/multilingual input
- Both fcitx and fcitx5 dirs exist (fcitx5 active)

### Systemd User Services
- `systemd/user/` for user-level services
- Some symlinks may break if services disabled

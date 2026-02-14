# .config/emacs KNOWLEDGE BASE

**Generated:** 2026-02-06 03:09 UTC

## OVERVIEW
Custom Emacs configuration. Modular, NOT doom/spacemacs. 44 files across 4 lisp modules.

## STRUCTURE
```
.config/emacs/
├── init.el              # Bootstrap (loads lisp-*)
├── early-init.el        # Pre-startup optimization
├── lisp-core/           # Core: packages, keybindings, setup
├── lisp-lang/           # Language modes (12 langs)
├── lisp-tools/          # Tools: dired, company, checkers, etc.
└── lisp-theme/          # Theming
```

## WHERE TO LOOK

| Task | Location | Notes |
|------|----------|-------|
| Add package | `lisp-core/init-packages.el` | Package management |
| Language support | `lisp-lang/init-{lang}.el` | Go, Rust, Python, Elisp, etc. |
| Tool integration | `lisp-tools/init-{tool}.el` | Company, LSP, git, etc. |
| Keybindings | `lisp-core/init-keybindings.el` | Global bindings |
| Startup optimization | `early-init.el`, `lisp-core/init-gcmh.el` | GC, package init |

## CONVENTIONS

### Loading System
- `init.el` adds lisp-* dirs to load-path
- Modules loaded via `(require 'init-*)` pattern
- OS detection constants: `IS-MAC`, `IS-LINUX`, `IS-ANDROID`

### Module Organization
- **lisp-core**: Foundation (packages, keybindings, setup)
- **lisp-lang**: One file per language/major mode
- **lisp-tools**: One file per tool category (dired, completion, etc.)
- **lisp-theme**: Appearance only

### Language Modules (lisp-lang)
- Go, Rust, Python, Elisp, Shell, Web (JS/CSS/HTML)
- YAML, Puppet, Debian (control files), PlantUML, Org-mode

### Tool Modules (lisp-tools) 
- Company (completion), Checkers (flycheck), LSP
- Dired, Git (magit), Projectile, Yasnippet
- Calendar, Chinese input, EAF, Ement (Matrix)

## ANTI-PATTERNS

### Do Not
- Modify `.config/doom/` or `.spacemacs.d/` - both DEPRECATED
- Add to load-path without directory existence check
- Use `package-initialize` in init.el (handled in early-init)

## NOTES

### Android/Termux Support
- `IS-ANDROID`, `IS-TERMUX` constants detect environment
- Termux paths added to exec-path automatically
- TLS program adjusted for Android gnutls-cli

### Data Directory
- `.local/` (my-data-dir) for persistent data
- Created automatically if missing

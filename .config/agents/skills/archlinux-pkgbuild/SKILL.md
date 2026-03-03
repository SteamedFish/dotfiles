---
name: archlinux-pkgbuild
description: Use when creating or reviewing Arch Linux PKGBUILD files, preparing AUR submissions, or fixing namcap/FHS/dependency issues in package metadata and build scripts.
---

# Arch Linux PKGBUILD Creation

## Overview

Use this skill as the default workflow for PKGBUILD work.  
Core rule: do not claim packaging is complete until `namcap` passes for both `PKGBUILD` and built package artifacts.

This skill is intentionally concise. Load sub-skills and reference docs only when needed.

## When to Use

- Writing a new PKGBUILD from upstream source or binary release.
- Updating package version, dependencies, install behavior, or architecture metadata.
- Debugging `namcap` warnings/errors.
- Preparing `.SRCINFO` and AUR push.

## Sub-Skills

| Sub-skill | Trigger |
|---|---|
| `archlinux-pkgbuild/vcs-packages` | `-git/-svn/-hg/-bzr/-darcs` packages, `pkgver()` logic |
| `archlinux-pkgbuild/systemd-services` | Services, `sysusers.d`, `tmpfiles.d`, sandboxing |
| `archlinux-pkgbuild/compiled-languages` | Go/Rust/Java/Haskell/OCaml/Pascal builds |
| `archlinux-pkgbuild/interpreted-languages` | Python/Node/Ruby/PHP/Perl/R/Shell/Lisp packages |
| `archlinux-pkgbuild/build-systems` | CMake or Meson projects |
| `archlinux-pkgbuild/cross-platform` | Wine/MinGW/Electron/.NET/Mono packaging |
| `archlinux-pkgbuild/desktop-integration` | GNOME/KDE/Eclipse/font integration |
| `archlinux-pkgbuild/system-packages` | DKMS/kernel/lib32/nonfree/webapp/split packages |

## Mandatory Workflow

```mermaid
flowchart TD
    A[Check existing packages] --> B[Write or update PKGBUILD]
    B --> C[Run namcap on PKGBUILD]
    C --> D{Passes?}
    D -->|No| B
    D -->|Yes| E{devtools available?}
    E -->|Yes| F[Build in clean chroot]
    E -->|No| G[Build with makepkg and warn]
    F --> H[Run namcap on built package]
    G --> H
    H --> I{Passes?}
    I -->|No| B
    I -->|Yes| J[Test install]
    J --> K[Generate .SRCINFO]
```

## Quick Reference

### 1. Existence check

```bash
pacman -Ss '^pkgname$'
yay -Ss '^pkgname$'   # or paru -Ss
```

If official repo has it, do not create duplicate AUR package.

### 2. Build method

| Method | Use case |
|---|---|
| `extra-x86_64-build` (preferred) | AUR submission, dependency correctness |
| `makepkg -f` (fallback) | Local prototyping only |

If fallback is used, explicitly warn that dependencies may be incomplete due to non-chroot build.

### 3. Validation (required)

```bash
namcap PKGBUILD
namcap *.pkg.tar.zst
```

Recommended deep checks:

```bash
namcap -i PKGBUILD
namcap -i *.pkg.tar.zst
```

### 4. Minimum PKGBUILD quality gates

| Item | Rule |
|---|---|
| Paths | Never install to `/usr/local`; package files go under `/usr` |
| Vendor config | Put package-owned defaults in `/usr/lib`, not `/etc` |
| Dependencies | List direct runtime/build deps only; no transitive padding. Do not include packages from `base` in `depends`/`optdepends` or `base-devel` in `makedepends` — both groups are assumed present |
| Checksums | Use `sha256sums`/`sha512sums`/`b2sums`; `SKIP` only for VCS sources |
| Arch field | `any` only for architecture-independent packages |
| Shell vars | Quote `"$pkgdir"` and `"$srcdir"` |
| Config files | User-editable `/etc` files must be in `backup=()` |

### 5. Architecture policy

- Source-built packages: prefer `x86_64` + `aarch64` when both can be validated.
- Binary packages: only declare architectures that upstream actually publishes.
- If only one architecture is tested, keep `arch=()` limited to tested architectures.

### 6. AUR finalization

```bash
makepkg --printsrcinfo > .SRCINFO
```

Track: `PKGBUILD`, `.SRCINFO`, patches, `.install`, service/tmpfiles/sysusers files.  
Ignore build artifacts (`pkg/`, `src/`, `*.pkg.tar.*`, `.BUILDINFO`, `.PKGINFO`, `.MTREE`, `.INSTALL`).

## Supporting References

- `advanced-workflow-and-build.md`: detailed workflow, dependency, architecture, build, and validation execution
- `aur-publishing-and-release-tracking.md`: detailed AUR push workflow, `.SRCINFO`/`.gitignore`, and nvchecker/pkgctl usage
- `policy-quality-and-install-scripts.md`: red flags, quality gates, configuration handling, and `.install` script patterns
- `pkgbuild-template.sh`: annotated baseline template
- `validation-guide.md`: `namcap` interpretation and fix patterns
- `fhs-and-vendor-config.md`: correct install destinations
- `config-file-handling.md`: `backup=()` and sensitive file permissions
- `clean-chroot-reference.md`: advanced `devtools`/chroot workflows

## Common Mistakes

- Submitting packages built only with local `makepkg` without warning.
- Using `/etc` for vendor defaults that belong in `/usr/lib`.
- Forgetting to regenerate `.SRCINFO` after dependency/version updates.
- Declaring unsupported architectures in binary packages.

## Resources

- https://wiki.archlinux.org/title/Arch_package_guidelines
- https://man.archlinux.org/man/PKGBUILD.5
- https://wiki.archlinux.org/title/Namcap
- https://wiki.archlinux.org/title/AUR_submission_guidelines
- https://wiki.archlinux.org/title/DeveloperWiki:Building_in_a_clean_chroot

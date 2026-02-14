---
name: archlinux-pkgbuild/language-ecosystems
description: DEPRECATED - Split into compiled-languages and interpreted-languages sub-skills. Use those instead.
---

# Language Ecosystem Packaging (DEPRECATED)

**This skill has been split into more focused sub-skills:**

- **archlinux-pkgbuild/compiled-languages** - For Go, Rust, Haskell, OCaml, Free Pascal, Java packages
- **archlinux-pkgbuild/interpreted-languages** - For Node.js, Python, Ruby, PHP, Perl, R, Shell, Lisp packages

**Please load the appropriate sub-skill instead of this one.**

## Migration Guide

| Old Reference | New Sub-Skill |
|--------------|---------------|
| Node.js, npm packages | **archlinux-pkgbuild/interpreted-languages** |
| Python, pip packages | **archlinux-pkgbuild/interpreted-languages** |
| Ruby, gem packages | **archlinux-pkgbuild/interpreted-languages** |
| PHP packages | **archlinux-pkgbuild/interpreted-languages** |
| Perl, CPAN modules | **archlinux-pkgbuild/interpreted-languages** |
| R packages | **archlinux-pkgbuild/interpreted-languages** |
| Shell scripts | **archlinux-pkgbuild/interpreted-languages** |
| Lisp packages | **archlinux-pkgbuild/interpreted-languages** |
| Go packages | **archlinux-pkgbuild/compiled-languages** |
| Rust, Cargo packages | **archlinux-pkgbuild/compiled-languages** |
| Haskell packages | **archlinux-pkgbuild/compiled-languages** |
| OCaml packages | **archlinux-pkgbuild/compiled-languages** |
| Free Pascal packages | **archlinux-pkgbuild/compiled-languages** |
| Java, JAR packages | **archlinux-pkgbuild/compiled-languages** |

This file is kept for backward compatibility but contains no implementation details. All content has been moved to the new sub-skills.

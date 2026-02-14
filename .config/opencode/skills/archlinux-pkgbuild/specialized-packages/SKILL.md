---
name: archlinux-pkgbuild/specialized-packages
description: DEPRECATED - Split into build-systems, cross-platform, desktop-integration, and system-packages sub-skills. Use those instead.
---

# Specialized Package Types (DEPRECATED)

**This skill has been split into more focused sub-skills:**

- **archlinux-pkgbuild/build-systems** - For CMake and Meson build systems
- **archlinux-pkgbuild/cross-platform** - For Wine, MinGW, Electron, CLR/.NET compatibility layers
- **archlinux-pkgbuild/desktop-integration** - For GNOME, KDE, Eclipse, Font packages
- **archlinux-pkgbuild/system-packages** - For DKMS, kernel modules, lib32, nonfree, web apps, split packages

**Please load the appropriate sub-skill instead of this one.**

## Migration Guide

| Old Reference | New Sub-Skill |
|--------------|---------------|
| CMake packages | **archlinux-pkgbuild/build-systems** |
| Meson packages | **archlinux-pkgbuild/build-systems** |
| Wine packages | **archlinux-pkgbuild/cross-platform** |
| MinGW packages | **archlinux-pkgbuild/cross-platform** |
| Electron apps | **archlinux-pkgbuild/cross-platform** |
| CLR/.NET/Mono | **archlinux-pkgbuild/cross-platform** |
| GNOME apps | **archlinux-pkgbuild/desktop-integration** |
| KDE apps | **archlinux-pkgbuild/desktop-integration** |
| Eclipse plugins | **archlinux-pkgbuild/desktop-integration** |
| Font packages | **archlinux-pkgbuild/desktop-integration** |
| DKMS modules | **archlinux-pkgbuild/system-packages** |
| Kernel modules | **archlinux-pkgbuild/system-packages** |
| lib32 multilib | **archlinux-pkgbuild/system-packages** |
| Nonfree software | **archlinux-pkgbuild/system-packages** |
| Web applications | **archlinux-pkgbuild/system-packages** |
| Split packages | **archlinux-pkgbuild/system-packages** |

This file is kept for backward compatibility but contains no implementation details. All content has been moved to the new sub-skills.

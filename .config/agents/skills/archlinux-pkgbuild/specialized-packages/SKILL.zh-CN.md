---
name: archlinux-pkgbuild/specialized-packages
description: DEPRECATED - Split into build-systems, cross-platform, desktop-integration, and system-packages sub-skills. Use those instead.
---

# 专门软件包类型（已弃用）

**本技能已拆分为更专注的子技能：**

- **archlinux-pkgbuild/build-systems** - 用于 CMake 和 Meson 构建系统
- **archlinux-pkgbuild/cross-platform** - 用于 Wine、MinGW、Electron、CLR/.NET 兼容层
- **archlinux-pkgbuild/desktop-integration** - 用于 GNOME、KDE、Eclipse、字体软件包
- **archlinux-pkgbuild/system-packages** - 用于 DKMS、内核模块、lib32、非自由软件、Web 应用、分割软件包

**请加载适当的子技能而不是本技能。**

## 迁移指南

| 旧参考 | 新子技能 |
|--------------|---------------|
| CMake 软件包 | **archlinux-pkgbuild/build-systems** |
| Meson 软件包 | **archlinux-pkgbuild/build-systems** |
| Wine 软件包 | **archlinux-pkgbuild/cross-platform** |
| MinGW 软件包 | **archlinux-pkgbuild/cross-platform** |
| Electron 应用 | **archlinux-pkgbuild/cross-platform** |
| CLR/.NET/Mono | **archlinux-pkgbuild/cross-platform** |
| GNOME 应用 | **archlinux-pkgbuild/desktop-integration** |
| KDE 应用 | **archlinux-pkgbuild/desktop-integration** |
| Eclipse 插件 | **archlinux-pkgbuild/desktop-integration** |
| 字体软件包 | **archlinux-pkgbuild/desktop-integration** |
| DKMS 模块 | **archlinux-pkgbuild/system-packages** |
| 内核模块 | **archlinux-pkgbuild/system-packages** |
| lib32 multilib | **archlinux-pkgbuild/system-packages** |
| 非自由软件 | **archlinux-pkgbuild/system-packages** |
| Web 应用 | **archlinux-pkgbuild/system-packages** |
| 分割软件包 | **archlinux-pkgbuild/system-packages** |

本文件保留用于向后兼容，但不包含实现细节。所有内容已移至新的子技能。

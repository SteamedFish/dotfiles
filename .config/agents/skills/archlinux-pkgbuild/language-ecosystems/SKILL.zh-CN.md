---
name: archlinux-pkgbuild/language-ecosystems
description: DEPRECATED - Split into compiled-languages and interpreted-languages sub-skills. Use those instead.
---

# 语言生态系统打包（已弃用）

**本技能已拆分为更专注的子技能：**

- **archlinux-pkgbuild/compiled-languages** - 用于 Go、Rust、Haskell、OCaml、Free Pascal、Java 软件包
- **archlinux-pkgbuild/interpreted-languages** - 用于 Node.js、Python、Ruby、PHP、Perl、R、Shell、Lisp 软件包

**请加载适当的子技能而不是本技能。**

## 迁移指南

| 旧参考 | 新子技能 |
|--------------|---------------|
| Node.js、npm 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| Python、pip 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| Ruby、gem 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| PHP 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| Perl、CPAN 模块 | **archlinux-pkgbuild/interpreted-languages** |
| R 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| Shell 脚本 | **archlinux-pkgbuild/interpreted-languages** |
| Lisp 软件包 | **archlinux-pkgbuild/interpreted-languages** |
| Go 软件包 | **archlinux-pkgbuild/compiled-languages** |
| Rust、Cargo 软件包 | **archlinux-pkgbuild/compiled-languages** |
| Haskell 软件包 | **archlinux-pkgbuild/compiled-languages** |
| OCaml 软件包 | **archlinux-pkgbuild/compiled-languages** |
| Free Pascal 软件包 | **archlinux-pkgbuild/compiled-languages** |
| Java、JAR 软件包 | **archlinux-pkgbuild/compiled-languages** |

本文件保留用于向后兼容，但不包含实现细节。所有内容已移至新的子技能。

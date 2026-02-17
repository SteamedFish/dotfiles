---
name: archlinux-pkgbuild/build-systems
description: Use when packaging software using CMake or Meson build systems - CMakeLists.txt or meson.build present in upstream source
---

# 构建系统软件包（CMake 和 Meson）

**有关核心 PKGBUILD 工作流程，请使用 archlinux-pkgbuild**

本子技能涵盖 CMake 和 Meson 构建系统以及 Arch 特定的配置要求。

## 快速参考

| 构建系统 | 关键工具 | 前缀标志 | 构建类型 | 常见问题 |
|--------------|----------|-------------|------------|---------------|
| CMake | cmake | CMAKE_INSTALL_PREFIX=/usr | CMAKE_BUILD_TYPE=None | lib64、RPATH、/usr/local |
| Meson | meson/arch-meson | --prefix=/usr | --buildtype=plain | 错误的前缀 |

---

## CMake 软件包

**CMake 是一个跨平台构建系统。** 常用于 C/C++ 项目。

### CMake 关键规则

| 规则 | 描述 |
|------|-------------|
| **CMAKE_BUILD_TYPE** | 使用 `None`（不是 `Release`）以避免自动 `-O3` 标志覆盖 |
| **CMAKE_INSTALL_PREFIX** | MUST（必须）是 `/usr`（不是 `/usr/local`） |
| **CMAKE_INSTALL_LIBDIR** | 如果项目默认为 `lib64`，则 MUST（必须）是 `lib` |
| **Verbose builds** | 使用 `VERBOSE=1` 验证编译器标志 |
| **RPATH 安全** | 使用 `CMAKE_SKIP_INSTALL_RPATH=YES` 或 `CMAKE_SKIP_RPATH=YES` |
| **cmake in makedepends** | 始终必需 |

### CMake 模板

```bash
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="CMake-based package"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('lib1' 'lib2')
makedepends=('cmake')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    local cmake_options=(
        -B build
        -S "$pkgname-$pkgver"
        -W no-dev
        -D CMAKE_BUILD_TYPE=None
        -D CMAKE_INSTALL_PREFIX=/usr
        -D CMAKE_INSTALL_LIBDIR=lib
        -D CMAKE_SKIP_INSTALL_RPATH=YES
    )
    cmake "${cmake_options[@]}"
    cmake --build build
}

check() {
    ctest --test-dir build --output-on-failure
}

package() {
    DESTDIR="$pkgdir" cmake --install build
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### CMake 常见问题

| 问题 | 错误 | 修复 |
|-------|-------|-----|
| 错误的前缀 | 文件在 /usr/local 中 | `-DCMAKE_INSTALL_PREFIX=/usr` |
| lib64 目录 | namcap：file-in-non-standard-dir | `-DCMAKE_INSTALL_LIBDIR=lib` |
| 不安全的 RPATH | namcap：insecure-rpath | `-DCMAKE_SKIP_INSTALL_RPATH=YES` |
| -O3 覆盖 | 错误的优化级别 | `-DCMAKE_BUILD_TYPE=None` |

---

## Meson 软件包

**Meson 是一个快速、用户友好的构建系统，为 Ninja 等后端生成构建文件。**

### Meson 关键规则

| 规则 | 描述 |
|------|-------------|
| **meson in makedepends** | 始终必需 |
| **Build method** | 直接：`meson setup` + `meson compile`，或 `arch-meson` 包装器 |
| **Build directory** | 源码外构建：通常命名为 `build` 或 `_build` |
| **Prefix** | 始终 `--prefix=/usr`（强制） |
| **Build type** | 使用 `--buildtype=plain` 或与 arch-meson 一起省略 |

### Meson 模板

```bash
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Meson-based application"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('lib1' 'lib2')
makedepends=('meson')
source=("https://example.com/$pkgname-$pkgver.tar.xz")
sha256sums=('...')

build() {
    arch-meson $pkgname-$pkgver build
    meson compile -C build
}

check() {
    meson test -C build --print-errorlogs
}

package() {
    meson install -C build --destdir "$pkgdir"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Meson 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 文件在 /usr/local 中 | 错误的前缀 | 使用 --prefix=/usr 或 arch-meson |
| 构建类型覆盖 | 优化标志未生效 | 使用 --buildtype=plain |
| 源码内构建 | 源中的构建产物 | 使用源码外构建目录 |
| 缺少测试 | 测试未运行 | 使用 meson test 添加 check() |

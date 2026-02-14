---
name: archlinux-pkgbuild/build-systems
description: Use when packaging software using CMake or Meson build systems - CMakeLists.txt or meson.build present in upstream source
---

# Build System Packages (CMake & Meson)

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers CMake and Meson build systems with Arch-specific configuration requirements.

## Quick Reference

| Build System | Key Tool | Prefix Flag | Build Type | Common Issues |
|--------------|----------|-------------|------------|---------------|
| CMake | cmake | CMAKE_INSTALL_PREFIX=/usr | CMAKE_BUILD_TYPE=None | lib64, RPATH, /usr/local |
| Meson | meson/arch-meson | --prefix=/usr | --buildtype=plain | Wrong prefix |

---

## CMake Packages

**CMake is a cross-platform build system.** Common for C/C++ projects.

### CMake Key Rules

| Rule | Description |
|------|-------------|
| **CMAKE_BUILD_TYPE** | Use `None` (not `Release`) to avoid automatic `-O3` flag override |
| **CMAKE_INSTALL_PREFIX** | Must be `/usr` (not `/usr/local`) |
| **CMAKE_INSTALL_LIBDIR** | Must be `lib` if project defaults to `lib64` |
| **Verbose builds** | Use `VERBOSE=1` to verify compiler flags |
| **RPATH security** | Use `CMAKE_SKIP_INSTALL_RPATH=YES` or `CMAKE_SKIP_RPATH=YES` |
| **cmake** in makedepends | Always required |

### CMake Template

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

### CMake Common Issues

| Issue | Error | Fix |
|-------|-------|-----|
| Wrong prefix | Files in /usr/local | `-DCMAKE_INSTALL_PREFIX=/usr` |
| lib64 directory | namcap: file-in-non-standard-dir | `-DCMAKE_INSTALL_LIBDIR=lib` |
| Insecure RPATH | namcap: insecure-rpath | `-DCMAKE_SKIP_INSTALL_RPATH=YES` |
| -O3 override | Wrong optimization level | `-DCMAKE_BUILD_TYPE=None` |

---

## Meson Packages

**Meson is a fast, user-friendly build system that generates build files for backends like Ninja.**

### Meson Key Rules

| Rule | Description |
|------|-------------|
| **meson in makedepends** | Always required |
| **Build method** | Direct: `meson setup` + `meson compile`, or `arch-meson` wrapper |
| **Build directory** | Out-of-source: commonly named `build` or `_build` |
| **Prefix** | Always `--prefix=/usr` (MANDATORY) |
| **Build type** | Use `--buildtype=plain` or omit with arch-meson |

### Meson Template

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

### Meson Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Files in /usr/local | Wrong prefix | Use --prefix=/usr or arch-meson |
| Build type override | Optimization flags not honored | Use --buildtype=plain |
| In-source build | Build artifacts in source | Use out-of-source build directory |
| Missing tests | Tests not run | Add check() with meson test |

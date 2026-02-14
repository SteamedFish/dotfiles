---
name: archlinux-pkgbuild/cross-platform
description: Use when packaging Windows applications under Wine, cross-compiling for Windows with MinGW, packaging Electron-based applications, or .NET/Mono applications
---

# Cross-Platform Compatibility Packages

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers packaging software that bridges platforms: Windows apps via Wine, Windows binaries via MinGW, web apps via Electron, and .NET apps via Mono/CLR.

## Quick Reference

| Platform | Use Case | Key Dependency | Install Location | Architecture |
|----------|----------|----------------|------------------|--------------|
| Wine | Windows apps on Linux | wine | /opt/appname/ | x86_64 |
| MinGW | Cross-compile for Windows | mingw-w64-gcc | /usr/{i686,x86_64}-w64-mingw32/ | any |
| Electron | Web apps as desktop apps | electron* | /usr/lib/appname/ | x86_64/any |
| CLR/Mono | .NET apps on Linux | mono | /usr/lib/appname/ | any |

---

## Wine Packages

**Wine packages provide Windows software that runs via Wine compatibility layer.**

### Wine Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Use application name (no wine- prefix unless conflicts) |
| **Architecture** | `x86_64` (even if 32-bit Windows app - wine provides multilib) |
| **Wine dependency** | Depend on `wine` or `wine-staging` |
| **Install location** | `/opt/appname/` or `/usr/share/appname/` |
| **Wrapper script** | Required in `/usr/bin/` to launch via wine |
| **WINEPREFIX** | Use per-application prefix in user's home |
| **Desktop integration** | Provide .desktop file with correct Exec line |

### Wine Template

```bash
pkgname=example-app
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Windows application (Wine)"
arch=('x86_64')
url="https://example.com"
license=('custom')
depends=('wine')
optdepends=('wine-gecko: HTML rendering'
            'wine-mono: .NET support')
source=("https://example.com/download/Example-$pkgver.exe")
sha256sums=('...')

package() {
    # Install Windows executable and files
    install -dm755 "$pkgdir/opt/$pkgname"
    install -Dm755 Example-$pkgver.exe "$pkgdir/opt/$pkgname/example.exe"
    
    # Install wrapper script
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << 'EOF'
#!/bin/bash
export WINEPREFIX="$HOME/.wine-example"
export WINEARCH=win32  # or win64

# Initialize prefix if needed
if [ ! -d "$WINEPREFIX" ]; then
    wineboot -u
fi

# Launch application
exec wine /opt/example-app/example.exe "$@"
EOF
    
    # Desktop file
    install -Dm644 /dev/stdin "$pkgdir/usr/share/applications/$pkgname.desktop" << EOF
[Desktop Entry]
Name=Example Application
Comment=Example Windows app via Wine
Exec=/usr/bin/$pkgname
Icon=$pkgname
Type=Application
Categories=Utility;
EOF
    
    # Install icon
    install -Dm644 icon.png "$pkgdir/usr/share/pixmaps/$pkgname.png"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Wine Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Shared WINEPREFIX | Multiple apps using default prefix | Use dedicated WINEPREFIX per app |
| Wrong architecture | 32-bit app with win64 prefix | Set WINEARCH=win32 |
| Missing DLLs | Native Windows DLLs needed | Bundle DLLs or use winetricks |
| Installer won't extract | Wrong extraction method | Use cabextract or innoextract |
| Desktop file won't launch | Wrong Exec path | Use absolute path to wrapper script |
| Icon not showing | Wrong icon format | Extract .ico with wrestool/icotool |

---

## MinGW Packages

**MinGW packages cross-compile software to run on Windows using mingw-w64 toolchain.**

### MinGW Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `mingw-w64-pkgname` (static: `mingw-w64-pkgname-static`) |
| **Architecture** | Always `any` (unless executables run on build system) |
| **Prefixes** | `/usr/i686-w64-mingw32/` and `/usr/x86_64-w64-mingw32/` |
| **Dependencies** | Always depend on `mingw-w64-crt` |
| **makedepends** | Always include `mingw-w64-gcc` |
| **options** | Always: `!strip`, `staticlibs`, `!buildflags` |
| **Build both** | 32-bit (i686) AND 64-bit (x86_64) versions |

### MinGW Template

```bash
pkgname=mingw-w64-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example library (mingw-w64)"
arch=('any')
url="https://example.com"
license=('GPL')
makedepends=('mingw-w64-configure')
depends=('mingw-w64-crt')
options=('!strip' '!buildflags' 'staticlibs')
source=("https://example.com/example-$pkgver.tar.gz")
sha256sums=('...')

_architectures="i686-w64-mingw32 x86_64-w64-mingw32"

build() {
  cd example-$pkgver
  for _arch in ${_architectures}; do
    mkdir -p build-${_arch} && pushd build-${_arch}
    ${_arch}-configure ..
    make
    popd
  done
}

package() {
  for _arch in ${_architectures}; do
    cd "example-$pkgver/build-${_arch}"
    make DESTDIR="${pkgdir}" install
    ${_arch}-strip --strip-unneeded "$pkgdir"/usr/${_arch}/bin/*.dll
    ${_arch}-strip -g "$pkgdir"/usr/${_arch}/lib/*.a
    rm -r "$pkgdir"/usr/${_arch}/share/{doc,man,info}
  done
}
```

### MinGW Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Files in /usr/local | Wrong prefix | Set --prefix=/usr/${_arch} |
| Missing DLLs | Not in bin/ directory | Install DLLs to /usr/${_arch}/bin/ |
| Only 32-bit or 64-bit | Missing architecture | Build for both i686 and x86_64 |
| No static libraries | Only shared built | Build both shared and static |

---

## Electron Packages

**Electron packages bundle web applications with Chromium and Node.js runtime.** Use system electron when possible.

### Electron Key Rules

| Rule | Description |
|------|-------------|
| **Use system electron** | Prefer `electron` package over bundled runtime |
| **Package naming** | No special suffix (use upstream name) |
| **electron dependency** | Add versioned `electron*` (e.g., `electron34`) to `depends` |
| **Architecture** | `any` if no native extensions, `x86_64` if compiled modules |
| **Wrapper script** | Required to launch app with system electron |
| **Resources location** | `/usr/lib/appname/` (arch-dependent) or `/usr/share/appname/` (any) |

### Electron Template

```bash
pkgname=example-app
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Electron application"
arch=('x86_64')
url="https://example.com"
license=('MIT')
depends=('electron34')
makedepends=('npm' 'nodejs')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    
    # Update electron version
    npm pkg set devDependencies.electron=$(cat /usr/lib/electron34/version)
    
    # Install dependencies
    npm install
    
    # Build with electron-builder
    ./node_modules/.bin/electron-builder --linux --x64 --dir \
        -c.electronDist=/usr/lib/electron34 \
        -c.electronVersion=$(cat /usr/lib/electron34/version)
}

package() {
    cd "$pkgname-$pkgver"
    
    # Install app resources (not electron runtime)
    install -dm755 "$pkgdir/usr/lib/$pkgname"
    cp -r dist/linux-unpacked/resources/app "$pkgdir/usr/lib/$pkgname/"
    
    # Create wrapper script
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << EOF
#!/bin/sh
exec /usr/bin/electron34 /usr/lib/$pkgname/app "\$@"
EOF
    
    # Desktop file
    install -Dm644 "$pkgname.desktop" "$pkgdir/usr/share/applications/$pkgname.desktop"
    
    # Icon
    install -Dm644 icon.png "$pkgdir/usr/share/pixmaps/$pkgname.png"
    
    # License
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Electron Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Bundled electron | Not using system electron | Use electron dependency + wrapper |
| Wrong electron version | Version mismatch | Match electron version in package.json |
| Missing app.asar | Wrong build output | Check electron-builder config |
| Wrapper doesn't work | Wrong path to app | Verify /usr/lib/$pkgname/app exists |

---

## CLR Packages (.NET/Mono)

**CLR packages use Mono runtime for .NET applications.** Mono provides CLR runtime on Linux.

### CLR Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Append `-mono` suffix (e.g., `example-mono`) |
| **arch** | Always `any` (Mono doesn't support 64-bit compilation yet) |
| **options** | Always include `!strip` |
| **mono dependency** | Always add `mono` to `depends` |
| **Signed assemblies** | GAC installation requires signed DLLs |
| **Debug database** | Convert .pdb to .mdb: `pdb2mdb Foo.dll` |
| **Executables** | Install shell wrapper to `/usr/bin` |

### CLR Template

```bash
pkgname=foo-mono
_pkgname=foo
pkgver=1.0.0
pkgrel=1
pkgdesc=".NET library for Foo"
arch=('any')
url="https://example.com"
license=('GPL')
depends=('mono')
makedepends=('mono')
options=('!strip')
source=("https://example.com/$_pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_pkgname-$pkgver"
    xbuild Foo.sln
}

package() {
    cd "$_pkgname-$pkgver/bin/Release"
    
    # For libraries: install DLL
    install -Dm644 Foo.dll "$pkgdir/usr/lib/$_pkgname/Foo.dll"
    
    # For executables: install wrapper script
    install -dm755 "$pkgdir/usr/bin"
    cat > "$pkgdir/usr/bin/$_pkgname" << 'EOF'
#!/bin/sh
exec mono /usr/lib/$_pkgname/Foo.exe "$@"
EOF
    chmod 755 "$pkgdir/usr/bin/$_pkgname"
}
```

### CLR Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Assembly not found | Wrong DLL location | Install to /usr/lib/appname/ |
| Strip breaks DLL | makepkg strips .NET assemblies | Add !strip to options |
| Missing mono runtime | Not in depends | Add mono to depends |
| GAC install fails | Unsigned assembly | Sign assembly or skip GAC |

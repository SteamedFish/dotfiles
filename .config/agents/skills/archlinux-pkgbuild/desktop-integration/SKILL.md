---
name: archlinux-pkgbuild/desktop-integration
description: Use when packaging GNOME applications, KDE Plasma widgets/themes, Eclipse IDE plugins, or font packages requiring desktop environment integration
---

# Desktop Integration Packages

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers desktop-specific packaging requirements for GNOME, KDE, Eclipse, and Font packages.

## Quick Reference

| Type | Key Tool | Install Location | Hook Handling |
|------|----------|------------------|---------------|
| GNOME | meson/arch-meson | /usr/bin, /usr/share | GSettings/icons auto |
| KDE | cmake + extra-cmake-modules | /usr/lib/qt/plugins | Auto-discovery |
| Eclipse | N/A (Java) | /usr/lib/eclipse/dropins | Auto-discovery |
| Fonts | N/A | /usr/share/fonts/$pkgname | fc-cache auto |

---

## Font Packages

**Font packages install TrueType, OpenType, or Variable fonts system-wide.**

### Font Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `ttf-fontname` (TrueType) or `otf-fontname` (OpenType) |
| **Variable fonts suffix** | Add `-variable` suffix for variable fonts |
| **Architecture** | Always `any` |
| **Dependencies** | None (fontconfig handled by hooks) |
| **Installation path** | `/usr/share/fonts/$pkgname/` |
| **License installation** | `/usr/share/licenses/$pkgname/` (especially OFL) |
| **Provides** | Add `provides=('ttf-font')` if meets criteria |

### Font Template

```bash
pkgname=ttf-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example TrueType font family"
arch=('any')
url="https://example.com"
license=('OFL-1.1-RFN')
provides=('ttf-font')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

package() {
    cd "$pkgname-$pkgver"
    
    # Install fonts
    install -Dm644 -t "$pkgdir/usr/share/fonts/$pkgname" fonts/*.ttf
    
    # Install license (required for OFL)
    install -Dm644 OFL.txt "$pkgdir/usr/share/licenses/$pkgname/OFL.txt"
}
```

### Font Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Missing license | OFL not installed | Install to /usr/share/licenses/$pkgname/ |
| Wrong directory | Font in wrong location | Use /usr/share/fonts/$pkgname/ |
| Uppercase in name | Package naming violation | Use all lowercase |
| Font cache not updated | Old packages used .install | Remove .install (hooks handle this) |

---

## GNOME Packages

**GNOME applications follow GNOME's build conventions and use common GNOME infrastructure.**

### GNOME Key Rules

| Rule | Description |
|------|-------------|
| **Build system** | Most use Meson, some older use GNU Autotools |
| **Source URL** | download.gnome.org for releases, gitlab.gnome.org for Git |
| **GSettings** | No manual intervention (pacman hooks handle updates) |
| **Icons/desktop** | No .install files (pacman hooks handle caching) |
| **Metainfo validation** | Add appstream-glib to checkdepends |

### GNOME Template (Meson)

```bash
pkgname=example-gnome-app
pkgver=45.0
pkgrel=1
pkgdesc="Example GNOME application"
arch=('x86_64')
url="https://gitlab.gnome.org/GNOME/$pkgname"
license=('GPL-2.0-or-later')
depends=('gtk4' 'libadwaita' 'glib2')
makedepends=('meson' 'git' 'appstream-glib')
checkdepends=('appstream-glib')
source=("https://download.gnome.org/sources/$pkgname/${pkgver%.*}/$pkgname-$pkgver.tar.xz")
sha256sums=('...')

build() {
    arch-meson "$pkgname-$pkgver" build
    meson compile -C build
}

check() {
    meson test -C build --print-errorlogs
}

package() {
    meson install -C build --destdir "$pkgdir"
}
```

### GNOME Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Manual schema compile | Old habit from pre-hook era | Remove from .install |
| Icon cache in .install | Unnecessary with hooks | Remove from .install |
| Desktop database in .install | Unnecessary with hooks | Remove from .install |
| Wrong source URL | Old git.gnome.org | Use gitlab.gnome.org |

---

## KDE Packages

**KDE packages use CMake with specific conventions for Plasma widgets, themes, and applications.**

### KDE Key Rules

| Rule | Description |
|------|-------------|
| **Build directory** | Out-of-source build required (`mkdir -p build`) |
| **Install prefix** | Always `-DCMAKE_INSTALL_PREFIX=/usr` |
| **Build type** | Generally omit (honors CFLAGS/CPPFLAGS) |
| **Plasma widgets** | Name: `plasma6-applets-widgetname` |
| **Plasma runners** | Name: `plasma6-runners-runnername` |
| **Service menus** | Name: `kf6-servicemenus-servicename` |
| **Themes** | Name: `plasma6-themes-themename` |

### KDE Template

```bash
pkgname=plasma6-applets-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Plasma 6 widget"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('plasma-workspace')
makedepends=('extra-cmake-modules' 'plasma-framework')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

prepare() {
    mkdir -p build
}

build() {
    cd build
    cmake ../$pkgname-$pkgver \
        -DCMAKE_INSTALL_PREFIX=/usr
    make
}

package() {
    cd build
    make DESTDIR="$pkgdir" install
}
```

### KDE Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Wrong naming convention | Inconsistent with Arch standards | Use plasma6-*, kf6-* prefixes |
| In-source build | Build artifacts in source | Use out-of-source build directory |
| Wrong CMake prefix | Files in wrong location | Set CMAKE_INSTALL_PREFIX=/usr |
| Missing extra-cmake-modules | KDE macros not available | Add to makedepends |

---

## Eclipse Packages

**Eclipse packages provide plugins and features for the Eclipse IDE.**

### Eclipse Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `eclipse-pluginname` (use plugin's common name, not internal ID) |
| **Architecture** | Usually `any` (pure Java), `x86_64` if contains native libraries |
| **Dependencies** | Must depend on `eclipse` or specific Eclipse base |
| **Install location** | `/usr/lib/eclipse/dropins/pluginname/` |
| **No manual registration** | Eclipse auto-discovers plugins in dropins/ |
| **Feature-based** | Install complete features, not individual JARs |

### Eclipse Template

```bash
pkgname=eclipse-example
_pkgname=com.example.eclipse.plugin
pkgver=1.0.0
pkgrel=1
pkgdesc="Example plugin for Eclipse IDE"
arch=('any')
url="https://example.com"
license=('EPL')
depends=('eclipse')
source=("https://example.com/eclipse-plugin-$pkgver.zip")
sha256sums=('...')

package() {
    cd "$srcdir"
    
    # Install to dropins directory
    install -dm755 "$pkgdir/usr/lib/eclipse/dropins/$pkgname"
    
    # Copy features and plugins
    if [ -d features ]; then
        cp -r features "$pkgdir/usr/lib/eclipse/dropins/$pkgname/"
    fi
    
    if [ -d plugins ]; then
        cp -r plugins "$pkgdir/usr/lib/eclipse/dropins/$pkgname/"
    fi
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Eclipse Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Plugin not detected | Wrong install location | Use /usr/lib/eclipse/dropins/ |
| Manual registration attempted | Old Eclipse versions | Remove registration (auto-discovery works) |
| Missing features/ directory | Only plugins/ installed | Install complete feature structure |
| Internal ID used | Package naming error | Use common name, not com.example.id |

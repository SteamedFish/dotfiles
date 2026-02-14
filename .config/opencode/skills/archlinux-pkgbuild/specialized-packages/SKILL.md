---
name: archlinux-pkgbuild/specialized-packages
description: Use when creating Arch Linux PKGBUILDs for specialized package types (nonfree, web apps, Wine, fonts, DKMS, Electron, Eclipse, CMake, GNOME, KDE, kernel modules, lib32, MinGW, split packages, or CLR/.NET)
---

# Specialized Package Types

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This skill covers specialized package types with unique requirements beyond standard PKGBUILD creation. Each section provides specific rules, templates, and common issues for that package type.

---

## Split Packages

**For multiple packages from one source:**

```bash
pkgbase=example-base
pkgname=('example-cli' 'example-gui')
pkgver=1.0.0
pkgrel=1

build() {
    cd "$srcdir/$pkgbase-$pkgver"
    make all
}

package_example-cli() {
    pkgdesc="CLI tool"
    depends=('lib1')
    
    cd "$srcdir/$pkgbase-$pkgver"
    make DESTDIR="$pkgdir" install-cli
}

package_example-gui() {
    pkgdesc="GUI application"
    depends=('lib1' 'gtk3')
    
    cd "$srcdir/$pkgbase-$pkgver"
    make DESTDIR="$pkgdir" install-gui
}
```

---

## Nonfree Packages

**Nonfree packages distribute proprietary or non-free software with licensing restrictions.** Special handling required for licensing, distribution, and user agreements.

### Nonfree Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Append `-nonfree` suffix only if conflicts with free version |
| **License handling** | Use `custom` license + install EULA/license file |
| **Source restrictions** | May require user to download separately (can't redistribute) |
| **EULA acceptance** | Include in `.install` file with post_install message |
| **NO AUR submission** | Proprietary software discouraged in AUR (case-by-case) |
| **Architecture** | Usually `x86_64` only (vendors rarely provide multi-arch) |
| **Binary distribution** | Use `-bin` suffix if not built from source |

### Nonfree License Handling

**License field for proprietary software:**

```bash
license=('custom')  # Or 'custom:VendorName'
```

**MANDATORY: Install license/EULA:**

```bash
package() {
    # ... install files ...
    
    # Install EULA
    install -Dm644 EULA.txt "$pkgdir/usr/share/licenses/$pkgname/EULA"
    
    # Or proprietary license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Nonfree Template (Redistributable Binary)

```bash
pkgname=example-proprietary
pkgver=1.0.0
pkgrel=1
pkgdesc="Example proprietary application"
arch=('x86_64')
url="https://example.com"
license=('custom')
depends=('glibc')
options=('!strip')  # Don't strip proprietary binaries
source=("https://example.com/downloads/example-$pkgver-linux-x86_64.tar.gz")
sha256sums=('...')

package() {
    cd "$srcdir/example-$pkgver"
    
    # Install to /opt for proprietary software
    install -dm755 "$pkgdir/opt/$pkgname"
    cp -r * "$pkgdir/opt/$pkgname/"
    
    # Create wrapper in /usr/bin
    install -dm755 "$pkgdir/usr/bin"
    ln -s "/opt/$pkgname/example" "$pkgdir/usr/bin/example"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Nonfree Template (Manual Download Required)

```bash
pkgname=example-restricted
pkgver=1.0.0
pkgrel=1
pkgdesc="Example software with download restrictions"
arch=('x86_64')
url="https://example.com"
license=('custom:Example')
depends=('glibc')
# NO source= - user must download
DLAGENTS=('http::/usr/bin/curl -fLC - --retry 3 --retry-delay 3 -o %o %u')

# PKGBUILD should error with download instructions
if [ ! -f "$startdir/example-$pkgver.tar.gz" ]; then
    error "Please download example-$pkgver.tar.gz manually from:"
    error "  https://example.com/download"
    error "Place the file in the same directory as this PKGBUILD."
    exit 1
fi

source=("file://example-$pkgver.tar.gz")
sha256sums=('...')  # Checksum of manual download

package() {
    cd "$srcdir/example-$pkgver"
    
    # Install application
    install -dm755 "$pkgdir/opt/$pkgname"
    cp -r * "$pkgdir/opt/$pkgname/"
    
    # Install license
    install -Dm644 EULA.txt "$pkgdir/usr/share/licenses/$pkgname/EULA"
}
```

### Nonfree .install File (EULA)

**Display EULA and require acceptance:**

```bash
post_install() {
    cat << EOF

==> EULA ACCEPTANCE REQUIRED
    
    By installing $pkgname, you agree to the End User License Agreement.
    License text: /usr/share/licenses/$pkgname/EULA
    
    Key restrictions:
    - Non-commercial use only
    - No redistribution
    - No reverse engineering
    
    If you do not agree, remove this package: pacman -R $pkgname

EOF
}

post_upgrade() {
    post_install
}
```

### Nonfree Software Locations

**Installation path guidelines:**

| Software Type | Install Location | Rationale |
|---------------|------------------|-----------|
| Self-contained proprietary app | `/opt/appname/` | FHS: /opt for "add-on software packages" |
| Proprietary libraries | `/usr/lib/` (with care) | Only if designed for system-wide use |
| Proprietary binaries | `/usr/bin/` (symlink to /opt) | Maintain PATH convenience |
| Proprietary games | `/opt/gamename/` | Keep game assets self-contained |

### Nonfree Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Strip breaks binary | makepkg strips proprietary code | Add `!strip` to options |
| Missing libraries | Proprietary binary needs specific .so versions | Bundle libs in /opt or use lib32 deps |
| EULA not displayed | No .install file | Create .install with post_install |
| License violation | Redistributing non-redistributable files | Use manual download method |
| Source unavailable | Binary-only distribution | Use `-bin` suffix in package name |
| Wrong location | Installed to /usr | Use /opt for self-contained proprietary apps |

---

## Web Application Packages

**Web application packages install server-side web applications with proper configuration and security.**

### Web Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Use application name (no `webapp-` prefix) |
| **Architecture** | Usually `any` (interpreted languages) |
| **Install location** | `/usr/share/webapps/appname/` |
| **Configuration** | `/etc/webapps/appname/` (separate from code) |
| **Web server config** | Provide example configs for Apache/Nginx |
| **Permissions** | Careful with writable directories (use http user) |
| **Dependencies** | Include web server as optdepends (not depends) |

### Web Application Directory Structure

| Content Type | Install Location |
|--------------|------------------|
| Application code | `/usr/share/webapps/appname/` |
| Configuration files | `/etc/webapps/appname/` |
| Writable data | `/var/lib/appname/` |
| Logs | `/var/log/appname/` |
| Cache | `/var/cache/appname/` |
| Web server configs | `/etc/webapps/appname/apache.example.conf` |

### Web Application Template

```bash
pkgname=example-webapp
pkgver=1.0.0
pkgrel=1
pkgdesc="Example web application"
arch=('any')
url="https://example.com"
license=('GPL')
depends=('php' 'php-sqlite')
optdepends=(
    'apache: Web server'
    'nginx: Web server'
    'mysql: Database backend'
)
backup=('etc/webapps/example/config.php')
source=("https://example.com/releases/$pkgname-$pkgver.tar.gz"
        'apache.example.conf'
        'nginx.example.conf')
sha256sums=('...')

package() {
    cd "$pkgname-$pkgver"
    
    # Install application
    install -dm755 "$pkgdir/usr/share/webapps/$pkgname"
    cp -r * "$pkgdir/usr/share/webapps/$pkgname/"
    
    # Move config to /etc
    install -dm755 "$pkgdir/etc/webapps/$pkgname"
    mv "$pkgdir/usr/share/webapps/$pkgname/config.php" \
        "$pkgdir/etc/webapps/$pkgname/"
    ln -s "/etc/webapps/$pkgname/config.php" \
        "$pkgdir/usr/share/webapps/$pkgname/config.php"
    
    # Create writable directories
    install -dm755 -o http -g http "$pkgdir/var/lib/$pkgname"
    install -dm755 -o http -g http "$pkgdir/var/log/$pkgname"
    
    # Install example web server configs
    install -Dm644 "$srcdir/apache.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/apache.example.conf"
    install -Dm644 "$srcdir/nginx.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/nginx.example.conf"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Web Application Security

**Configuration separation:**
- Code: `/usr/share/webapps/` (root:root, 644/755)
- Config: `/etc/webapps/` (root:root, 640/750 or http:http for writable)
- Data: `/var/lib/appname/` (http:http, 755/700)

**PHP open_basedir:**
```apache
php_admin_value open_basedir "/tmp:/usr/share/webapps/example:/etc/webapps/example:/var/lib/example"
```

### Web Application Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Writable /usr/share | Wrong permissions | Move writable dirs to /var/lib/ |
| Config in /usr/share | Not separated | Move to /etc/webapps/, symlink back |
| Web server required | In depends | Move to optdepends |
| No example configs | Not provided | Create apache/nginx example configs |
| Wrong ownership | Not using http:http | Set ownership for writable dirs |
| open_basedir errors | Paths not whitelisted | Add all needed paths to open_basedir |

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

---

## DKMS Packages (Kernel Modules)

**DKMS packages provide kernel modules that automatically rebuild on kernel updates.**

### DKMS Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Append `-dkms` suffix (e.g., `example-dkms`) |
| **dkms dependency** | Always add `dkms` to `depends` |
| **NO linux-headers** | Don't add to depends (dkms handles this) |
| **Source location** | Install to `/usr/src/PACKAGE_NAME-PACKAGE_VERSION/` |
| **dkms.conf** | Required configuration file |
| **NO manual dkms calls** | Pacman hooks handle dkms add/build/install/remove |
| **NO autoload in .install** | Let user load modules manually |

### DKMS Template

```bash
_pkgbase=example
pkgname=example-dkms
pkgver=1.0.0
pkgrel=1
pkgdesc="Example kernel module (DKMS)"
arch=('x86_64')
url="https://example.com"
license=('GPL2')
depends=('dkms')
conflicts=("$_pkgbase")
install=${pkgname}.install
source=("https://example.com/tarball.tar.gz"
        'dkms.conf'
        "${pkgname}.conf")
sha256sums=('...')

package() {
    # Install dkms.conf
    install -Dm644 dkms.conf "$pkgdir/usr/src/$_pkgbase-$pkgver/dkms.conf"
    
    # Set name and version in dkms.conf
    sed -e "s/@_PKGBASE@/$_pkgbase/" \
        -e "s/@PKGVER@/$pkgver/" \
        -i "$pkgdir/usr/src/$_pkgbase-$pkgver/dkms.conf"
    
    # Copy sources (including Makefile)
    cp -r "$_pkgbase"/* "$pkgdir/usr/src/$_pkgbase-$pkgver/"
    
    # Blacklist conflicting modules
    install -Dm644 ${pkgname}.conf "$pkgdir/usr/lib/modprobe.d/${pkgname}.conf"
}
```

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

---

## Kernel Module Packages

**Kernel module packages must support multiple kernel installations and separate modules from utilities.**

### Kernel Module Key Rules

| Rule | Description |
|------|-------------|
| **Package separation** | Modules in `pkgname`, utilities in `pkgname-utils` |
| **Dependencies** | Module package depends on specific kernel + utils package |
| **Provides** | Alternative kernel modules provide base package name |
| **File placement** | Override modules in `/lib/modules/X.Y.Z-arch1-1/updates/` |
| **Multiple kernels** | Each kernel needs separate module package |

### Kernel Module Package Separation

**MANDATORY separation pattern:**

| Package | Contains | Dependencies |
|---------|----------|--------------|
| `nvidia` | Modules for linux kernel | `linux`, `nvidia-utils` |
| `nvidia-utils` | Supporting files, utilities | (base dependencies) |
| `nvidia-lts` | Modules for linux-lts kernel | `linux-lts`, `nvidia-utils` |

### Kernel Module Template

```bash
pkgname=nvidia
pkgver=550.54.14
pkgrel=1
pkgdesc="NVIDIA drivers for linux kernel"
arch=('x86_64')
url="https://www.nvidia.com"
license=('custom')
depends=('linux' 'nvidia-utils')
makedepends=('linux-headers')
source=("https://example.com/nvidia-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    
    # Get kernel version
    _kernver=$(cat /usr/lib/modules/extramodules-*/version)
    
    # Build module
    make SYSSRC=/usr/lib/modules/$_kernver/build module
}

package() {
    cd "$pkgname-$pkgver"
    
    # Get kernel version
    _kernver=$(cat /usr/lib/modules/extramodules-*/version)
    
    # Install kernel module
    install -Dm644 nvidia.ko \
        "$pkgdir/usr/lib/modules/$_kernver/extramodules/nvidia.ko"
}
```

---

## lib32 Packages (32-bit)

**lib32 packages provide 32-bit libraries on x86_64 systems for multilib support.**

### lib32 Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `lib32-pkgname` (matches 64-bit package name) |
| **Architecture** | Always `x86_64` (multilib is x86_64-only) |
| **Dependencies** | Often depend on 64-bit version for documentation/binaries |
| **Library location** | `/usr/lib32/` (NOT /usr/lib/) |
| **Compiler flags** | Use `-m32` and `lib32-gcc-libs` |
| **Provides/conflicts** | No provides/conflicts with 64-bit version |

### lib32 Template

```bash
_pkgbasename=example
pkgname=lib32-$_pkgbasename
pkgver=1.0.0
pkgrel=1
pkgdesc="Example library (32-bit)"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('lib32-glibc' "$_pkgbasename")
makedepends=('gcc-multilib')
source=("https://example.com/$_pkgbasename-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_pkgbasename-$pkgver"
    
    export CC='gcc -m32'
    export CXX='g++ -m32'
    export PKG_CONFIG_PATH='/usr/lib32/pkgconfig'
    
    ./configure \
        --prefix=/usr \
        --libdir=/usr/lib32 \
        --disable-static
    make
}

package() {
    cd "$_pkgbasename-$pkgver"
    make DESTDIR="$pkgdir" install
    
    # Remove everything except libraries
    rm -rf "$pkgdir"/usr/{bin,include,share}
}
```

### lib32 Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Libraries in /usr/lib | Wrong libdir | Set --libdir=/usr/lib32 |
| Missing -m32 flag | No multilib compilation | Export CC='gcc -m32' |
| Headers installed | Not removed in package() | rm -rf "$pkgdir"/usr/include |
| Wrong architecture | Set to 'any' or 'i686' | Always use arch=('x86_64') |

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

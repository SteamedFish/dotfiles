---
name: archlinux-pkgbuild/system-packages
description: Use when packaging DKMS kernel modules, out-of-tree kernel modules, 32-bit multilib libraries, proprietary/nonfree software, web applications, or creating split packages
---

# System Packages

**System packages handle specialized system-level components: kernel modules (DKMS and manual), multilib 32-bit libraries, proprietary/nonfree software, web applications, and split packages.**

## When to Use This Skill

Use when:
- Packaging kernel modules (DKMS or traditional)
- Creating 32-bit libraries for multilib (lib32-*)
- Packaging proprietary/nonfree software with licensing restrictions
- Installing web applications with proper configuration separation
- Creating split packages (multiple packages from one source)

Trigger phrases:
- "kernel module", "DKMS", "dkms.conf"
- "lib32-", "32-bit", "multilib"
- "proprietary", "nonfree", "EULA", "/opt"
- "web app", "webapp", "http user"
- "split package", "pkgbase"

## Quick Reference

| Package Type | Key Pattern | Critical Files/Paths |
|--------------|-------------|----------------------|
| **DKMS** | `example-dkms`, depends=(dkms) | /usr/src/$_pkgbase-$pkgver/dkms.conf |
| **Kernel Module** | Per-kernel packages, `-utils` separation | /usr/lib/modules/extramodules-*/module.ko |
| **lib32** | `lib32-pkgname`, arch=('x86_64') | --libdir=/usr/lib32, CC='gcc -m32' |
| **Nonfree** | license=('custom'), options=('!strip') | /opt/appname/, /usr/share/licenses/ |
| **Web App** | arch=('any'), http:http ownership | /usr/share/webapps/, /etc/webapps/, /var/lib/ |
| **Split** | pkgbase + multiple package_name() | Shared build(), separate package_*() |

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

**Key points:**
- `pkgbase=` defines base name, `pkgname=()` array lists all packages
- Single `build()`, separate `package_name()` for each package
- Each package has own `pkgdesc`, `depends`, `optdepends`, etc.
- Common pattern: utilities package + module/plugin packages

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

**dkms.conf example:**
```ini
PACKAGE_NAME="@_PKGBASE@"
PACKAGE_VERSION="@PKGVER@"
BUILT_MODULE_NAME[0]="example"
DEST_MODULE_LOCATION[0]="/kernel/drivers/misc"
AUTOINSTALL="yes"
MAKE[0]="make KERNELRELEASE=${kernelver}"
CLEAN="make clean"
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
| Writable data | `/var/lib/appname/` (use tmpfiles.d) |
| Logs | `/var/log/appname/` (use tmpfiles.d) |
| Cache | `/var/cache/appname/` (use tmpfiles.d) |
| Web server configs | `/etc/webapps/appname/apache.example.conf` |
| tmpfiles.d config | `/usr/lib/tmpfiles.d/appname.conf` |

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
install=$pkgname.install
source=("https://example.com/releases/$pkgname-$pkgver.tar.gz"
        'apache.example.conf'
        'nginx.example.conf'
        "$pkgname.tmpfiles")
sha256sums=('...')

package() {
    cd "$pkgname-$pkgver"
    
    # Install application
    install -dm755 "$pkgdir/usr/share/webapps/$pkgname"
    cp -r * "$pkgdir/usr/share/webapps/$pkgname/"
    
    # Remove writable directories from application (will be created by tmpfiles.d)
    rm -rf "$pkgdir/usr/share/webapps/$pkgname"/{uploads,cache,logs,data}
    
    # Move config to /etc
    install -dm755 "$pkgdir/etc/webapps/$pkgname"
    mv "$pkgdir/usr/share/webapps/$pkgname/config.php" \
        "$pkgdir/etc/webapps/$pkgname/"
    ln -s "/etc/webapps/$pkgname/config.php" \
        "$pkgdir/usr/share/webapps/$pkgname/config.php"
    
    # Create symlinks to /var/lib (actual dirs created by tmpfiles.d at runtime)
    ln -s /var/lib/$pkgname/uploads "$pkgdir/usr/share/webapps/$pkgname/uploads"
    ln -s /var/lib/$pkgname/cache "$pkgdir/usr/share/webapps/$pkgname/cache"
    
    # Install systemd-tmpfiles.d configuration
    install -Dm644 "$srcdir/$pkgname.tmpfiles" \
        "$pkgdir/usr/lib/tmpfiles.d/$pkgname.conf"
    
    # Install example web server configs
    install -Dm644 "$srcdir/apache.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/apache.example.conf"
    install -Dm644 "$srcdir/nginx.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/nginx.example.conf"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Web Application systemd-tmpfiles.d

**MANDATORY for writable directories in /var:** Use systemd-tmpfiles.d instead of creating directories in package().

**Why tmpfiles.d:**
- User-configurable cleanup policies (Age field)
- Proper ownership handling (User/Group fields)
- Runtime directory creation (survives system updates)
- Standard Arch Linux practice

**tmpfiles.d template (appname.tmpfiles):**
```conf
# systemd-tmpfiles configuration for appname
# See tmpfiles.d(5) for details

# Type Path                    Mode User  Group Age Argument
d     /var/lib/appname          0755 http  http  -   -
d     /var/lib/appname/uploads  0755 http  http  -   -
d     /var/lib/appname/cache    0755 http  http  30d -
d     /var/log/appname          0755 http  http  -   -
```

**Field explanations:**
- `Type`: `d` = Create directory if not exists, `z` = Adjust ownership/permissions on existing paths
- `Path`: Absolute path to directory/file
- `Mode`: Permissions (0755, 0700, etc.)
- `User/Group`: Ownership (http for web applications)
- `Age`: Cleanup policy (`-` = never, `30d` = 30 days, `7d` = 7 days)
- `Argument`: Additional args (usually `-`)

**For sensitive config files (credentials, secrets):**
```conf
# If application writes to config (e.g., web installers):
# Files are root:http 0660 (root owns, http group can write)
z     /etc/webapps/appname/config.php      0660 root  http  -   -
z     /etc/webapps/appname/database.php    0660 root  http  -   -  # Contains DB password!

# Type 'z' adjusts ownership/permissions on files installed by the package
# root:http 0660 = root owns file (secure), http group can write, NOT world-readable
# Config directory /etc/webapps/appname remains root:root (default from install)
```

**Why use type 'z' with root:http for sensitive configs:**
1. Package installs files as root:root 0644 (normal)
2. tmpfiles.d changes group to http with 0660 permissions
3. Root owns the file (safer - only root can delete/rename)
4. http group can read/write (application can modify config)
5. Files are NOT world-readable (0660) - protects credentials

**See config-file-handling.md for detailed security patterns.**

**PKGBUILD integration:**
```bash
source=('...' "$pkgname.tmpfiles")
sha256sums=('...')

package() {
    # Install tmpfiles.d configuration
    install -Dm644 "$srcdir/$pkgname.tmpfiles" \
        "$pkgdir/usr/lib/tmpfiles.d/$pkgname.conf"
    
    # DO NOT create directories with install -dm755
    # Directories are created at runtime by systemd-tmpfiles
    
    # Create symlinks to /var directories (optional, if app needs them)
    ln -s /var/lib/$pkgname/uploads "$pkgdir/usr/share/webapps/$pkgname/uploads"
}
```

**.install file:**
```bash
post_install() {
    cat <<EOF
==> Configuration and setup instructions...
==> 
==> Writable directories are automatically created in /var/lib/$pkgname/
==> by systemd-tmpfiles.d during package installation.
EOF
}

post_upgrade() {
    # Only show upgrade-relevant information
    # Database setup is one-time, not needed on upgrades
    cat <<EOF

==> Web Application Upgrade Notes
==> 

1. Configuration Review:
   - Check for .pacnew files: /etc/webapps/$pkgname/
   - Review new configuration options

2. Database Migrations (if applicable):
   - Check for migration scripts in /usr/share/webapps/$pkgname/updates/
   - Run database updates through application update interface

3. Service Restart:
   - Restart web server: systemctl restart httpd (or nginx + php-fpm)

4. Directories:
   - Writable directories maintained automatically at /var/lib/$pkgname/

Changelog: https://upstream.project/releases
EOF
}
```

**IMPORTANT:** 
- **DO NOT call `systemd-tmpfiles` manually** in .install files
- Pacman automatically invokes systemd-tmpfiles during installation/upgrade via hooks
- .install files should **ONLY print information**, not perform actions
- Directories are created automatically when the package is installed

**Common tmpfiles.d patterns:**

| Directory Type | Mode | User:Group | Age | Rationale |
|----------------|------|------------|-----|-----------|
| Uploads/Data | 0755 | http:http | `-` | Persistent data, never auto-clean |
| Cache | 0755 | http:http | `30d` | Auto-clean old cache files |
| Sessions | 0700 | http:http | `7d` | Clean old sessions weekly |
| Logs | 0755 | http:http | `-` | Keep logs (use logrotate instead) |
| Temp files | 1777 | root:root | `1d` | Shared temp, clean daily |

### Web Application Security

**Configuration separation:**
- Code: `/usr/share/webapps/` (root:root, 644/755)
- Config: `/etc/webapps/` (root:root, 640/750 or http:http for writable)
- Data: `/var/lib/appname/` (http:http, 755/700) - **Created by tmpfiles.d**

**PHP open_basedir:**
```apache
php_admin_value open_basedir "/tmp:/usr/share/webapps/example:/etc/webapps/example:/var/lib/example"
```

### Web Application Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Writable /usr/share | Wrong permissions | Move writable dirs to /var/lib/, use tmpfiles.d |
| Config in /usr/share | Not separated | Move to /etc/webapps/, symlink back |
| Web server required | In depends | Move to optdepends |
| No example configs | Not provided | Create apache/nginx example configs |
| Wrong ownership | Using install -o/-g in package() | Use tmpfiles.d with User/Group fields |
| Directories not created | Missing tmpfiles.d | Create .tmpfiles file (pacman hooks call systemd-tmpfiles automatically) |
| Symlinks to non-existing /var | Dirs created at runtime | Normal - tmpfiles.d creates them on install |
| open_basedir errors | Paths not whitelisted | Add all needed paths to open_basedir |

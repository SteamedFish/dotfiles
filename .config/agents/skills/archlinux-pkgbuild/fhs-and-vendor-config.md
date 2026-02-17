# FHS Compliance and System Package Locations

**This is detailed reference for correct installation paths in Arch Linux packages.**

## Standard FHS Paths

**Correct installation paths:**

| Content | Correct Path | WRONG Path |
|---------|-------------|------------|
| Binaries | /usr/bin | /usr/local/bin, /bin |
| Libraries | /usr/lib | /usr/local/lib, /lib |
| Headers | /usr/include | /usr/local/include |
| App modules | /usr/lib/$pkgname | /usr/libexec |
| Documentation | /usr/share/doc/$pkgname | /usr/doc |
| Licenses | /usr/share/licenses/$pkgname | /usr/share/doc |
| Man pages | /usr/share/man | /usr/man |
| App data | /usr/share/$pkgname | /usr/local/share |
| Desktop entries | /usr/share/applications | /usr/local/share/applications |
| Icons | /usr/share/icons or /usr/share/pixmaps | /usr/local/share/icons |
| Config | /etc | /usr/etc |
| State data | /var/lib/$pkgname | /var/$pkgname |

## System Package Locations (Vendor Config)

**CRITICAL RULE: Vendor-provided configuration ALWAYS goes to `/usr/lib/`, NOT `/etc/`.**

| Config Type | System Package Location (CORRECT) | User Override Location | WRONG Path |
|-------------|-----------------------------------|------------------------|------------|
| systemd user definitions | `/usr/lib/sysusers.d/$pkgname.conf` | `/etc/sysusers.d/` | `/etc/sysusers.d/$pkgname.conf` |
| systemd tmpfiles config | `/usr/lib/tmpfiles.d/$pkgname.conf` | `/etc/tmpfiles.d/` | `/etc/tmpfiles.d/$pkgname.conf` |
| udev rules | `/usr/lib/udev/rules.d/$pkgname.rules` | `/etc/udev/rules.d/` | `/etc/udev/rules.d/$pkgname.rules` |
| systemd services | `/usr/lib/systemd/system/$pkgname.service` | `/etc/systemd/system/` | `/etc/systemd/system/$pkgname.service` |
| systemd network | `/usr/lib/systemd/network/$pkgname.network` | `/etc/systemd/network/` | `/etc/systemd/network/$pkgname.network` |
| modprobe.d | `/usr/lib/modprobe.d/$pkgname.conf` | `/etc/modprobe.d/` | `/etc/modprobe.d/$pkgname.conf` |
| modules-load.d | `/usr/lib/modules-load.d/$pkgname.conf` | `/etc/modules-load.d/` | `/etc/modules-load.d/$pkgname.conf` |
| PAM config | `/usr/lib/pam.d/$pkgname` | `/etc/pam.d/` | `/etc/pam.d/$pkgname` (sometimes valid) |
| environment.d | `/usr/lib/environment.d/$pkgname.conf` | `/etc/environment.d/` | `/etc/environment.d/$pkgname.conf` |

### Why This Matters

1. **Separation of concerns**: `/etc/` is **EXCLUSIVELY for user modifications**, `/usr/lib/` is for **package-provided defaults**
2. **Update semantics**: System updates NEVER touch `/etc/` (protects user config), but can overwrite `/usr/lib/`
3. **Conflict prevention**: Two packages installing to same path in `/usr/lib/` is VALID (systemd merges), but in `/etc/` causes pacman conflicts
4. **Search order**: systemd/udev search `/etc/` first (user overrides), then `/usr/lib/` (package defaults)

### File Naming Convention for System Locations

**ALWAYS use package name as basename** to prevent conflicts:

```bash
# ✅ CORRECT: Package name as filename
install -Dm644 "$srcdir/myapp.sysusers" "$pkgdir/usr/lib/sysusers.d/myapp.conf"
install -Dm644 "$srcdir/myapp.tmpfiles" "$pkgdir/usr/lib/tmpfiles.d/myapp.conf"
install -Dm644 "$srcdir/myapp.rules" "$pkgdir/usr/lib/udev/rules.d/99-myapp.rules"

# ❌ WRONG: Generic or inconsistent names
install -Dm644 "$srcdir/myapp.sysusers" "$pkgdir/usr/lib/sysusers.d/users.conf"  # Conflicts!
install -Dm644 "$srcdir/myapp.tmpfiles" "$pkgdir/usr/lib/tmpfiles.d/cleanup.conf"  # Unclear ownership
install -Dm644 "$srcdir/myapp.rules" "$pkgdir/usr/lib/udev/rules.d/device.rules"  # Which package?
```

**If multiple related files needed, include package prefix:**

```bash
# ✅ CORRECT: Multiple configs with package prefix
/usr/lib/tmpfiles.d/myapp-runtime.conf
/usr/lib/tmpfiles.d/myapp-cache.conf
/usr/lib/sysusers.d/myapp-primary.conf
/usr/lib/sysusers.d/myapp-secondary.conf

# ❌ WRONG: No package identification
/usr/lib/tmpfiles.d/runtime.conf  # What package?
/usr/lib/tmpfiles.d/cache.conf    # Conflict risk
```

## Fixing Paths in prepare()

Many upstream projects install to wrong paths. Fix in prepare():

```bash
prepare() {
    cd "$srcdir/$pkgname-$pkgver"
    
    # Fix systemd unit install path
    sed -i 's|/etc/systemd/system|/usr/lib/systemd/system|g' Makefile
    
    # Fix udev rules path
    sed -i 's|/etc/udev/rules.d|/usr/lib/udev/rules.d|g' setup.py
    
    # Fix binary path
    sed -i 's|/usr/local/bin|/usr/bin|g' CMakeLists.txt
    
    # Fix library path
    sed -i 's|/usr/local/lib|/usr/lib|g' configure
}
```

## Common Path Fixes by Build System

### Makefile Projects

```bash
# Override paths during install
make install DESTDIR="$pkgdir" \
  PREFIX=/usr \
  BINDIR=/usr/bin \
  LIBDIR=/usr/lib \
  MANDIR=/usr/share/man

# Fix paths in prepare()
sed -i 's|/usr/local|/usr|g' Makefile
sed -i 's|/etc/systemd|/usr/lib/systemd|g' Makefile
```

### CMake Projects

```bash
cmake -B build -S . \
  -DCMAKE_INSTALL_PREFIX=/usr \
  -DCMAKE_INSTALL_BINDIR=/usr/bin \
  -DCMAKE_INSTALL_LIBDIR=/usr/lib \
  -DCMAKE_INSTALL_SYSCONFDIR=/etc \
  -DCMAKE_INSTALL_DATADIR=/usr/share
```

### Python Projects

```bash
# setuptools usually installs correctly to /usr
python -m build --wheel --no-isolation
python -m installer --destdir="$pkgdir" dist/*.whl

# If needed, override paths
python setup.py install \
  --root="$pkgdir" \
  --prefix=/usr \
  --optimize=1
```

### Meson Projects

```bash
arch-meson . build \
  --prefix=/usr \
  --bindir=/usr/bin \
  --libdir=/usr/lib \
  --sysconfdir=/etc
```

## Verifying Correct Paths

After building, check installed paths:

```bash
# List all installed files
tar -tzf package.pkg.tar.zst

# Check for wrong paths
tar -tzf package.pkg.tar.zst | grep -E '(^usr/local|^bin/|^lib/)'

# Check systemd/udev paths
tar -tzf package.pkg.tar.zst | grep -E 'etc/(systemd|udev|tmpfiles|sysusers|modprobe)'
```

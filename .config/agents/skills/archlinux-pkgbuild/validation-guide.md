# PKGBUILD Validation Guide

Comprehensive guide for validating PKGBUILD files using namcap and fixing common issues.

## Quick Reference: namcap Validation Commands

```bash
# Validate PKGBUILD file
namcap PKGBUILD

# Validate built package
namcap package-name-version.pkg.tar.zst

# Validate .SRCINFO
namcap .SRCINFO
```

## Common namcap Warnings and Fixes

| Warning | Meaning | Fix |
|---------|---------|-----|
| `missing-maintainer` | No maintainer info | Add `# Maintainer: Your Name <email>` |
| `missing-url` | No URL field | Add `url="https://..."` |
| `missing-license` | No license field | Add `license=('GPL')` or appropriate |
| `missing-description` | No description | Add `pkgdesc="..."` |
| `file-not-in-package` | Listed file missing | Check `backup=()`, `source=()` arrays |
| `file-in-non-standard-dir` | Wrong directory | Follow FHS (see fhs-and-vendor-config.md) |
| `insecure-rpath` | Security issue | Remove RPATH or use `$ORIGIN` |
| `missing-dependency` | Unlisted dependency | Add to `depends=()` array |
| `dependency-not-needed` | Unnecessary dep | Remove from `depends=()` if not used at runtime |
| `variable-not-array` | Should be array | Use `license=('GPL')` not `license='GPL'` |
| `script-link-detected` | Improper script handling | Don't copy to /usr/bin, use proper installation |
| `elffile-not-in-allowed-dirs` | Binary in wrong location | Move to /usr/bin, /usr/lib, etc. |
| `permissions-too-permissive` | Security issue | Use `chmod` in `package()` to fix |
| `file-referred-in-startdir` | Wrong path reference | Use `$pkgdir`, `$srcdir` not `$startdir` |
| `directory-name-in-backup` | Backup array has dir | Only list files, not directories |

## Validation Workflow

### 1. Pre-Build Validation

Before building the package:

```bash
# Check PKGBUILD syntax and structure
namcap PKGBUILD

# Generate .SRCINFO and validate it
makepkg --printsrcinfo > .SRCINFO
namcap .SRCINFO
```

**Fix all errors and warnings** before proceeding.

### 2. Post-Build Validation

After building with `makepkg` or clean chroot:

```bash
# Validate the built package
namcap *.pkg.tar.zst

# Check what files are included
tar -tvf *.pkg.tar.zst | less
```

### 3. Common Issues and Solutions

**Issue: `file-in-non-standard-dir`**
- **Cause**: Files installed in wrong FHS locations
- **Fix**: See fhs-and-vendor-config.md for correct paths
- **Example**: Move from `/usr/share/bin/` to `/usr/bin/`

**Issue: `missing-dependency`**
- **Cause**: Runtime dependencies not listed
- **Fix**: Add to `depends=()` array
- **How to find**: Use `ldd` on binaries, check imports in scripts

**Issue: `dependency-not-needed`**
- **Cause**: Listed dependency not actually used at runtime
- **Fix**: Move to `makedepends=()` if only needed for building, or remove entirely

**Issue: `elffile-not-in-allowed-dirs`**
- **Cause**: Binaries in non-standard locations
- **Fix**: Install to `/usr/bin/` (executables) or `/usr/lib/` (libraries)

**Issue: `insecure-rpath`**
- **Cause**: Hardcoded library paths in ELF binaries
- **Fix**: Either remove RPATH entirely or use `$ORIGIN` for relative paths
- **Tools**: `patchelf --remove-rpath` or `patchelf --set-rpath '$ORIGIN/../lib'`

**Issue: `permissions-too-permissive`**
- **Cause**: Files have overly broad permissions (e.g., 777)
- **Fix**: Add `chmod` commands in `package()` function

```bash
package() {
  install -Dm755 myapp "$pkgdir/usr/bin/myapp"  # 755 for executables
  install -Dm644 config "$pkgdir/etc/myapp/config"  # 644 for config files
}
```

**Issue: `file-referred-in-startdir`**
- **Cause**: Using `$startdir` instead of proper variables
- **Fix**: Use `$srcdir` for source files, `$pkgdir` for installation targets

```bash
# ❌ Wrong
cp "$startdir/file" "$pkgdir/usr/bin/"

# ✅ Correct
cp "$srcdir/file" "$pkgdir/usr/bin/"
```

## Dependency Analysis Tools

Use these tools to find missing dependencies:

```bash
# For compiled binaries - show shared library dependencies
ldd /path/to/binary

# Find which package provides a library
pacman -F libname.so

# For Python packages
grep -r "^import " . | cut -d: -f2 | sort -u

# For Node.js packages
cat package.json | jq -r '.dependencies | keys[]'
```

## Testing Checklist

After fixing all namcap warnings:

- [ ] `namcap PKGBUILD` shows no errors
- [ ] `namcap .SRCINFO` shows no errors
- [ ] `namcap *.pkg.tar.zst` shows no critical errors
- [ ] All files in correct FHS locations
- [ ] All runtime dependencies listed in `depends=()`
- [ ] All build-time dependencies listed in `makedepends=()`
- [ ] Package installs cleanly: `sudo pacman -U *.pkg.tar.zst`
- [ ] Application runs correctly after installation
- [ ] Package removes cleanly: `sudo pacman -R pkgname`

## See Also

- **fhs-and-vendor-config.md** - Correct installation paths for different file types
- **config-file-handling.md** - Handling configuration files properly
- **clean-chroot-reference.md** - Advanced validation in clean chroot environment

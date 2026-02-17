# Clean Chroot Building - Advanced Reference

This document provides advanced clean chroot building techniques beyond the basic workflow in the main PKGBUILD skill.

## Build in tmpfs (for speed)

If you have enough RAM (20GB+ recommended):

```bash
# Create tmpfs mount
sudo mount --mkdir -t tmpfs -o defaults,size=20G tmpfs /mnt/chroots/arch

# Build using tmpfs
extra-x86_64-build -c -r /mnt/chroots/arch

# After building, unmount
sudo umount /mnt/chroots/arch
```

## Resetting a Broken Chroot

If chroot becomes corrupted or contaminated:

```bash
# Using convenience scripts (resets automatically)
extra-x86_64-build -c

# Or manually delete chroot
sudo rm -rf /var/lib/archbuild/extra-x86_64
# Next build will recreate it
```

## Building for Different Repositories

| Repository | Build Script | Use Case |
|------------|-------------|----------|
| extra (stable) | `extra-x86_64-build` | Most packages, AUR submissions |
| extra-testing | `extra-testing-x86_64-build` | Pre-release testing |
| extra-staging | `extra-staging-x86_64-build` | Major rebuilds, dependency chains |
| multilib | `multilib-build` | 32-bit libraries |
| multilib-testing | `multilib-testing-build` | Testing 32-bit packages |
| multilib-staging | `multilib-staging-build` | Major 32-bit rebuilds |

## Custom Dependencies in Clean Chroot

**For packages depending on other custom packages:**

```bash
# Method 1: Using convenience scripts (recommended)
extra-x86_64-build -- -I ~/pkgs/custom-dep-1.0-1-x86_64.pkg.tar.zst

# Method 2: Using manual chroot
makechrootpkg -c -r $CHROOT -I custom-dep-1.0-1-x86_64.pkg.tar.zst

# Multiple dependencies
extra-x86_64-build -- \
  -I ~/pkgs/dep1-1.0-1-x86_64.pkg.tar.zst \
  -I ~/pkgs/dep2-2.0-1-x86_64.pkg.tar.zst
```

## Major Rebuilds (Multiple Interdependent Packages)

**Preferred: Use staging repository:**

```bash
# 1. Build first package against extra, push to staging
extra-x86_64-build
# ... push to extra-staging ...

# 2. Build remaining packages against staging
extra-staging-x86_64-build
```

**Alternative: Install built packages in chroot (not recommended for distribution):**

```bash
# Build first package
extra-x86_64-build

# Build subsequent packages with -n (install in chroot without cleaning)
makechrootpkg -n -r /var/lib/archbuild/extra-x86_64

# This method:
# - Keeps chroot dirty between builds
# - Faster for local development
# - NOT suitable for distribution (dependencies may be wrong)
```

## Passing makepkg Arguments Through Clean Chroot

```bash
# Force check() to run
extra-x86_64-build -- --check

# Skip integrity checks (development only)
extra-x86_64-build -- --skipchecksums

# Multiple arguments
extra-x86_64-build -- --check --noconfirm

# With custom dependencies
extra-x86_64-build -- -I custom.pkg.tar.zst --check
```

## Troubleshooting Clean Chroot Builds

| Problem | Solution |
|---------|----------|
| "chroot is locked" | Remove `/var/lib/archbuild/extra-x86_64.lock` |
| Chroot update fails | Reset with `-c` flag: `extra-x86_64-build -c` |
| Out of space errors | Clean old package caches in chroot |
| Permission denied | Run with `sudo` or ensure user in `wheel` group |
| Custom repo not found | Edit `$CHROOT/root/etc/pacman.conf` |
### Manual Clean Chroot Setup (Advanced)

**Only needed if you want custom chroot location or configuration:**

```bash
# 1. Install devtools
sudo pacman -S devtools

# 2. Create chroot directory
mkdir ~/chroot
CHROOT=$HOME/chroot

# 3. Initialize chroot (root subdirectory is mandatory)
mkarchroot $CHROOT/root base-devel

# 4. (Optional) Edit chroot config
# - Mirrorlist: $CHROOT/root/etc/pacman.d/mirrorlist
# - Pacman config: $CHROOT/root/etc/pacman.conf
# - Makepkg config: ~/.makepkg.conf (used by makechrootpkg)

# 5. Update chroot before building
arch-nspawn $CHROOT/root pacman -Syu

# 6. Build package (run in PKGBUILD directory)
makechrootpkg -c -r $CHROOT

# The -c flag ensures working chroot is cleaned before build
```

**Custom pacman.conf/makepkg.conf (use with caution):**
```bash
mkarchroot -C custom-pacman.conf -M custom-makepkg.conf $CHROOT/root base-devel
```

**Building with custom dependencies:**
```bash
makechrootpkg -c -r $CHROOT -I custom-dep-1.0-1-x86_64.pkg.tar.zst
```

**Passing arguments to makepkg:**
```bash
# Force check() to run
makechrootpkg -c -r $CHROOT -- --check

# Skip integrity checks (for development)
makechrootpkg -c -r $CHROOT -- --skipchecksums
```


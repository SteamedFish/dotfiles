# Arch Linux PKGBUILD Policy, Quality, and Install Script Reference

Detailed reference for quality gates, red flags, config-file handling, and .install script patterns.

## Common Mistakes and Red Flags

### Critical Errors (MUST FIX)

| Mistake | Why It's Wrong | Correct Approach |
|---------|---------------|------------------|
| Building with direct makepkg for distribution | Missing/incorrect dependencies, system pollution | Build in clean chroot using `extra-x86_64-build` |
| Skipping namcap | Violates Arch packaging standards | Always run namcap on PKGBUILD and .pkg.tar.zst (required), plus namcap -i (recommended) |
| Using /usr/local/ | Breaks FHS compliance | Use /usr/ paths only |
| Vendor config in /etc/ | Wrong separation of concerns, conflict risk | Use /usr/lib/sysusers.d/, /usr/lib/tmpfiles.d/, /usr/lib/udev/rules.d/ |
| Generic filenames in system dirs | Package conflicts, unclear ownership | Always use package name: /usr/lib/sysusers.d/$pkgname.conf |
| Missing direct dependencies | Runtime failures | Build in clean chroot OR use find-libdeps, ldd to find all direct deps |
| Including transitive deps | Violates packaging policy | Only list direct dependencies |
| Using 'SKIP' for non-VCS | Security risk | Generate real checksums with updpkgsums |
| Unquoted $pkgdir/$srcdir | Shell expansion errors | Always quote: "$pkgdir" "$srcdir" |
| Self-referencing pkgdesc | Redundant | "Tool for X" not "pkgname is a tool for X" |
| Missing .desktop file for GUI apps | App won't appear in menus | Install to /usr/share/applications/, validate with desktop-file-validate (see archlinux-pkgbuild/cross-platform sub-skill for examples) |
| Hardcoded paths in source | Version bump requires edit | Use variables: $pkgname-$pkgver |
|| Claiming multi-arch support without testing | Package fails on untested architecture | Build and test on ALL arches, or ask user to test before AUR submission |
|| Binary package with wrong arch list | Package install fails or downloads wrong binary | Match `arch=()` to ALL upstream binary availability |
|| Using 'any' for compiled packages | Package isn't architecture-independent | Use specific arches for compiled binaries |
| Using prebuilt binary blobs in source-available packages | Violates source-build-first policy; introduces unauditable binaries | Identify each blob, check for existing Arch packages, or write a separate source-built PKGBUILD |

### Warning Signs (CHECK CAREFULLY)

| Pattern | Potential Issue | Investigation |
|---------|----------------|---------------|
| Custom configure flags | May override Arch defaults | Check Arch guidelines for standard flags |
| Stripping disabled | Debug symbols bloat | Only if upstream requires |
| Empty prepare() | May need path fixes | Check for /usr/local in build output |
| No check() function | Untested package | Run upstream tests if available |
| Many optdepends | Are they all optional? | Some may be required for core functionality |
| Version pinned deps | May break on updates | Use >= only when truly required |

## PKGBUILD Functions Reference

| Function | Required? | Purpose | Common Commands |
|----------|-----------|---------|-----------------|
| **prepare()** | Optional | Patch sources, fix paths | sed, patch, find |
| **pkgver()** | VCS only | Auto-update version | git rev-list, svnversion |
| **build()** | Usually | Compile sources | ./configure, make, cmake |
| **check()** | Optional | Run test suite | make test, make check |
| **package()** | **MANDATORY** | Install to $pkgdir | make install, install |

## Installation Commands Reference

```bash
# Install files with correct permissions
install -Dm644 file.txt "$pkgdir/usr/share/doc/$pkgname/file.txt"  # Regular file
install -Dm755 binary "$pkgdir/usr/bin/binary"                      # Executable
install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"  # License

# Create directories
install -dm755 "$pkgdir/usr/share/$pkgname"

# Copy entire directories
cp -r dir "$pkgdir/usr/share/$pkgname/"

# Remove unwanted files
rm -rf "$pkgdir/usr/share/doc"  # If upstream installs docs incorrectly
```

## Configuration File Handling

**User-modifiable config files MUST be listed in backup=() to prevent pacman from overwriting user changes.**

### Quick Rules

```bash
backup=(
    'etc/myapp/main.conf'     # Relative to root, NO leading slash
)
```

**Include in backup=():** All /etc files users might customize, especially credential files  
**Exclude:** Templates in /usr/share, generated files, service files

**Pacman behavior:** Modified files → Creates .pacnew (user merges manually)

### Security: Sensitive Configuration Files

**If config files contain credentials/secrets (database passwords, API keys):**
- Use `0660` permissions (not world-readable)
- Set ownership via tmpfiles.d type `z`
- Example: `z /etc/webapps/app/database.php 0660 root http - -`

**If application writes to config (web installers):**
- Files owned by root, group set to app user (e.g., root:http)
- Use `0660` for group-writable, not world-readable (security)
- Directory can remain root:root (only files need group access)

**Detailed reference:** See config-file-handling.md for:
- backup=() rules and .pacnew workflow
- Sensitive config file permissions (0660 vs 0640 vs 0644)
- Using tmpfiles.d type `z` for ownership/permission changes
- Security patterns for web applications

## Post-Install Scripts (.install files)

**Use .install files to provide post-installation instructions, perform setup tasks, or notify users of manual configuration requirements.**
**Core principle: Arch Linux users are experienced. Keep output minimal. Never state the obvious.**

### When to Use .install Files

| Use Case | Example |
|----------|---------|
| Complex setup with no upstream docs | Database creation, schema import |
| Manual steps that cannot be automated | Generating secrets, editing credentials |
| Critical warnings | Security notices, breaking changes |
| Upstream has setup docs | Link to them instead of reproducing |

**Do NOT use .install for:**
- Telling users to `systemctl start/enable` when a service file is included — they know
- Steps pacman already performs automatically (tmpfiles, sysusers, udev reload)
- Repeating information from the man page or upstream docs

### .install File Structure

```bash
# /path/to/pkgname.install
post_install() {
    cat <<EOF

==> Package Name
==> ============

1. First setup step:
   - Detailed instructions
   - Commands to run

2. Second setup step:
   - More instructions

For more information: https://upstream.docs

EOF
}

post_upgrade() {
    # IMPORTANT: Only include upgrade-relevant information
    # DO NOT blindly call post_install() unless ALL instructions apply to upgrades
    
    # If upgrade needs specific instructions (migrations, breaking changes):
    cat <<EOF

==> Upgrade Notes
==> =============

- Review configuration files (check .pacnew files)
- Database migration steps (if applicable)
- Breaking changes in this version
- Service restart commands

EOF
    
    # ANTI-PATTERN: Don't do this unless initial setup truly applies to upgrades
    # post_install  # ❌ Wrong if it contains one-time setup instructions
}

pre_remove() {
    # Optional: cleanup before removal
    # Use sparingly - most cleanup should be manual
}

post_remove() {
    # Optional: notify about leftover data
    cat <<EOF

==> Removal Complete
==> =================

User data remains in /var/lib/pkgname/
Remove manually if needed: sudo rm -rf /var/lib/pkgname

EOF
}
```

### PKGBUILD Integration

**Reference the .install file in PKGBUILD:**

```bash
# In PKGBUILD
pkgname=myapp
install="$pkgname.install"  # Add after optdepends=(), before source=()
```

**The .install file must:**
- Be named `$pkgname.install`
- Be in the same directory as PKGBUILD
- NOT be listed in source=() (it's metadata, not a source file)
- Use proper shell syntax (functions, no syntax errors)

### Best Practices

| Do | Don't |
|----|-------|
| Keep total output under 10 lines | Write essay-length instructions |
| Link to upstream docs for complex setup | Duplicate upstream documentation inline |
| Use heredoc (cat <<EOF) for formatting | Use multiple echo statements |
| Separate one-time vs upgrade instructions | Call post_install from post_upgrade blindly |
| Test .install script for syntax errors | Assume it will work without testing |
| Only output what user MUST act on | State obvious things Arch users already know |

### Minimalism Rules (CRITICAL)

**1. No redundant pacman-triggered commands**
Pacman automatically runs `systemd-tmpfiles`, `sysusers`, and `udev` triggers. Never call these in .install:
```bash
# ❌ WRONG — pacman already does this
post_install() {
    systemd-tmpfiles --create
    udevadm trigger
}
```

**2. No obvious systemd instructions**
When a package ships a `.service` file, Arch users know how to start it. Never print:
```bash
# ❌ WRONG — obvious, unwanted noise
echo "Start the service: systemctl start myapp"
echo "Enable on boot:   systemctl enable myapp"
```

**3. Link upstream docs, don't duplicate them**
If upstream has a detailed setup guide, one line is enough:
```bash
# ✅ CORRECT
cat <<EOF
==> myapp: initial setup required — see https://upstream.example/docs/setup
EOF
```

**4. Minimal output — only actionable items**
Every printed line is noise unless the user must act on it. Target < 10 lines total.

### CRITICAL: post_install vs post_upgrade Distinction

**post_install()**: One-time initial setup instructions
- Database creation and schema import
- Initial configuration file setup
- Web server configuration from scratch
- User/group creation
- First-time setup wizards
- Installation verification steps

**post_upgrade()**: Upgrade-relevant information ONLY
- Configuration file changes (.pacnew review)
- Database migrations/schema updates
- Breaking changes between versions
- Service restart requirements
- Deprecated feature warnings
- Changelog references

**Decision Rule:**
```
If instruction is ONLY needed on first install → post_install() only
If instruction is ONLY needed on upgrade → post_upgrade() only  
If instruction applies to BOTH → Consider calling post_install() from post_upgrade()

⚠️  ANTI-PATTERN: Always calling post_install() from post_upgrade()
Only do this if ALL post_install instructions genuinely apply to upgrades.
```

**Examples:**

❌ **WRONG** - Upgrade shows unnecessary one-time setup:
```bash
post_install() {
    cat <<EOF
1. Create database and import schema  # ❌ Only needed once
2. Configure web server               # ❌ Only needed once
3. Run installation wizard            # ❌ Only needed once
EOF
}
post_upgrade() {
    post_install  # ❌ Confuses users with irrelevant steps
}
```

✅ **CORRECT** - Minimal, link-first:
```bash
post_install() {
    cat <<EOF
==> myapp: initial setup required
1. Create database:   sudo -u postgres createdb myapp
2. Import schema:     psql myapp < /usr/share/myapp/schema.sql
3. Edit config:       /etc/webapps/myapp/config.php
4. Complete setup:    http://localhost/myapp/install
Docs: https://github.com/project/wiki/Setup
EOF
}

post_upgrade() {
    # Omit entirely if nothing requires user action
    cat <<EOF
==> myapp upgrade notes
- Review .pacnew files if any
- Changelog: https://github.com/project/releases
EOF
}

✅ **ACCEPTABLE** - When instructions truly apply to both:
```bash
post_install() {
    cat <<EOF
==> myapp: review config at /etc/myapp/config.yaml
EOF
}

post_upgrade() {
    post_install  # ✅ OK — these steps apply to upgrades too
}

### Example: Web Application (Minimal)

```bash
# webapp.install
post_install() {
    cat <<EOF
==> webapp: initial setup required
1. Create DB:   sudo -u postgres createdb webappdb
2. Edit config: /usr/share/webapps/webapp/config.php
3. Link vhost:  sudo ln -s /etc/httpd/conf/extra/webapp.conf /etc/httpd/conf/extra/
4. Finish:      http://localhost/webapp/install
Docs: https://webapp.example/docs/setup
EOF
}

post_upgrade() {
    # Only add if there are breaking changes or required migrations
    cat <<EOF
==> webapp: check for .pacnew files
Changelog: https://webapp.example/releases
EOF
}

### Validation

```bash
# Check .install syntax
bash -n pkgname.install

# Verify it's referenced in PKGBUILD
grep "^install=" PKGBUILD

# Verify it's included in package
namcap *.pkg.tar.zst | grep -i install
```

**Note:** The .install file is automatically included in the package when `install=` is set. Do NOT manually install it in package().

## Specialized Package Types

**For specialized package types, load the appropriate sub-skill:**

- **VCS packages** (-git, -svn, -cvs): Use **archlinux-pkgbuild/vcs-packages**
- **Systemd services** (DynamicUser, tmpfiles.d, sandboxing, init conversion): Use **archlinux-pkgbuild/systemd-services**
- **Compiled languages** (Go, Rust, Haskell, OCaml, Free Pascal, Java): Use **archlinux-pkgbuild/compiled-languages**
- **Interpreted languages** (Node.js, Python, Ruby, PHP, Perl, R, Shell, Lisp): Use **archlinux-pkgbuild/interpreted-languages**
- **Build systems** (CMake, Meson): Use **archlinux-pkgbuild/build-systems**
- **Cross-platform** (Wine, MinGW, Electron, CLR): Use **archlinux-pkgbuild/cross-platform**
- **Desktop integration** (GNOME, KDE, Eclipse, Fonts): Use **archlinux-pkgbuild/desktop-integration**
- **System packages** (DKMS, kernel modules, lib32, nonfree, web apps, split): Use **archlinux-pkgbuild/system-packages**

## Advanced Clean Chroot Techniques

**For advanced clean chroot usage** (tmpfs builds, custom dependencies, major rebuilds, different repositories), see **clean-chroot-reference.md** in this directory.

**Quick troubleshooting:**

| Problem | Solution |
|---------|----------|
| "chroot is locked" | Remove `/var/lib/archbuild/extra-x86_64.lock` |
| Chroot update fails | Reset with `-c` flag: `extra-x86_64-build -c` |
| Permission denied | Run with `sudo` or ensure user in `wheel` group |

## Resources

- Arch Package Guidelines: https://wiki.archlinux.org/title/Arch_package_guidelines
- PKGBUILD(5) man page: https://man.archlinux.org/man/PKGBUILD.5
- FHS specification: https://man.archlinux.org/man/file-hierarchy.7
- AUR submission: https://wiki.archlinux.org/title/AUR_submission_guidelines
- namcap: https://wiki.archlinux.org/title/Namcap
- Clean chroot building: https://wiki.archlinux.org/title/DeveloperWiki:Building_in_a_clean_chroot
- devtools: https://man.archlinux.org/man/extra/devtools/pkgctl-build.1
- nvchecker/pkgctl-version: https://man.archlinux.org/man/extra/devtools/pkgctl-version.1

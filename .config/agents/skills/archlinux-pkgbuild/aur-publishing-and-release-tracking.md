# Arch Linux PKGBUILD AUR Publishing and Release Tracking

Detailed reference for AUR submission workflow, repository hygiene, and nvchecker/pkgctl version tracking.

### Step 8: AUR Submission (if applicable)

**Generate .SRCINFO:**
```bash
makepkg --printsrcinfo > .SRCINFO
```

**Set up AUR SSH:**
```bash
# Add to ~/.ssh/config
Host aur.archlinux.org
    IdentityFile ~/.ssh/aur
    User aur
```

**Set up .gitignore:**
```bash
# Create .gitignore for PKGBUILD repository
cat > .gitignore << 'EOF'
# Build artifacts
*.pkg.tar.zst
*.pkg.tar.xz
*.pkg.tar.gz

# Source tarballs
*.tar.gz
*.tar.bz2
*.tar.xz
*.zip

# Build directories
pkg/
src/

# makepkg metadata (build-time only)
.BUILDINFO
.PKGINFO
.MTREE
.INSTALL

# Temporary files
*.log
*~
*.swp
EOF
```

**CRITICAL .gitignore rules:**

| File | Track? | Why |
|------|--------|-----|
| **PKGBUILD** | ✅ YES | Mandatory source file |
| **.SRCINFO** | ✅ YES | Required for AUR (machine-readable metadata) |
| **\*.install** | ✅ YES | Post-install script source |
| **\*.patch** | ✅ YES | Source patches |
| **Supplementary configs** | ✅ YES | .tmpfiles, .service, example configs |
| **.INSTALL** | ❌ NO | Build artifact (generated from .install) |
| **pkg/, src/** | ❌ NO | Build directories |
| **\*.pkg.tar.*** | ❌ NO | Built packages |
| **.BUILDINFO, .PKGINFO, .MTREE** | ❌ NO | makepkg metadata |

**Two scenarios for AUR submission:**

**Scenario A: Existing local git repository with PKGBUILD files already tracked**

If you already have a git repository with PKGBUILD, .SRCINFO, and related files:

```bash
# Add AUR as a remote to existing repository
cd /path/to/existing/pkgname
git remote add aur ssh://aur@aur.archlinux.org/pkgname.git

# Push to AUR
git push aur master
```

**Benefits:**
- Keeps existing git history
- Single repository for both local development and AUR
- Directory name matches package name
- No file copying needed

**Scenario B: Starting from scratch (new package)**

If you don't have a git repository yet:

```bash
# Clone (empty for new packages)
git clone ssh://aur@aur.archlinux.org/pkgname.git
cd pkgname

# Add files
cp /path/to/PKGBUILD .
makepkg --printsrcinfo > .SRCINFO

# Create .gitignore (see above)
cat > .gitignore << 'EOF'
# Build artifacts
*.pkg.tar.zst
*.pkg.tar.xz
*.pkg.tar.gz

# Source tarballs
*.tar.gz
*.tar.bz2
*.tar.xz
*.zip

# Build directories
pkg/
src/

# makepkg metadata (build-time only)
.BUILDINFO
.PKGINFO
.MTREE
.INSTALL

# Temporary files
*.log
*~
*.swp
EOF

# Commit
git add PKGBUILD .SRCINFO .gitignore
git commit -m "Initial commit: pkgname $pkgver-$pkgrel"

# Push
git push origin master
```

**Decision rule:** If you have an existing git repository with the required files (PKGBUILD, .SRCINFO, .gitignore, patches, configs), use **Scenario A**. Only use **Scenario B** when starting completely fresh.

## pkgrel Update Policy

`pkgrel` counts how many times the **same upstream version** has been re-packaged. It signals to users that a rebuild is needed without an upstream version bump.

### Rule 1: Never bump pkgrel for unpushed changes

If you made mistakes in a local PKGBUILD that was **never pushed to AUR**, no user has ever installed that version. Bumping `pkgrel` would create a phantom rebuild for a version nobody received.

```
AUR has 1.7-1. You're working on 1.8 locally.
You find a mistake in your 1.8 PKGBUILD and fix it.
→ DO NOT bump pkgrel. Set pkgrel=1 and keep iterating.
   No one installed your broken local 1.8; there is nothing to increment from.
```

**Rule:** `pkgrel` only increments when you need to distinguish from a version **already in AUR that users may have installed**.

### Rule 2: Only bump pkgrel when installed users need a rebuild

After pushing a version to AUR (`pkgver-1`), bump `pkgrel` to `pkgver-2` only when users who installed `pkgver-1` have a **broken installation that requires a rebuild**.

| Change | Bump pkgrel? | Reason |
|--------|-------------|--------|
| Upstream Python major version upgrade breaks site-packages path | ✅ YES | Users' installed files are in wrong path; rebuild required |
| Patch a library linking issue that causes runtime crashes | ✅ YES | Installed binaries are broken |
| Fix `install=` hook that corrupted system state (wrote bad files, ran bad commands) | ✅ YES | Hook actively damaged installed state; cleanup + rebuild needed |
| Add a forgotten `depends=()` entry | ❌ NO | Users who successfully installed already had that dep (their build would have failed otherwise) |
| Fix a `makedepends=()` entry | ❌ NO | Build-time only; does not affect installed package |
| Update pkgdesc, url, license metadata | ❌ NO | Cosmetic; no functional change to installed files |
| Style/formatting cleanup in PKGBUILD | ❌ NO | No change to what gets installed |
| Add missing `optdepends=()` entry | ❌ NO | Optional; does not break existing installs |
| Fix `install=` hook that only printed wrong info | ❌ NO | Hook is informational only; installed files are unaffected |

**Decision test:** Ask — "Do users who successfully installed `pkgver-1` have a broken or suboptimal installation **right now**, without reinstalling?" If yes → bump. If no → don't bump.

## Tracking Upstream Releases with nvchecker

**nvchecker** automates checking for new upstream releases. Use **pkgctl version** commands for integration.

### Quick Start

```bash
# Auto-generate .nvchecker.toml from PKGBUILD source array
pkgctl version setup

# Check for new upstream releases
pkgctl version check

# Update PKGBUILD to new version
pkgctl version upgrade
```

### .nvchecker.toml Configuration

Place `.nvchecker.toml` in the same directory as PKGBUILD. The `pkgctl version setup` command auto-creates this by analyzing your `source=()` array.

**Example for GitHub releases:**
```toml
[pkgname]
source = "github"
github = "owner/repo"
```

**Example for PyPI:**
```toml
[python-package]
source = "pypi"
pypi = "package-name"
```

**Example for GitLab:**
```toml
[pkgname]
source = "gitlab"
gitlab = "group/project"
```

**See also:**
- pkgctl-version(1) man page for full documentation
- Example configs in official packages (e.g., https://gitlab.archlinux.org/archlinux/packaging/packages/pacman/-/blob/main/.nvchecker.toml)
- nvchecker documentation: https://nvchecker.readthedocs.io/

### Workflow

1. Initial setup: `pkgctl version setup` (creates .nvchecker.toml)
2. Periodically check: `pkgctl version check`
3. When new version found: `pkgctl version upgrade` (updates pkgver)
4. Review changes, update pkgrel=1, rebuild, test
5. Commit to AUR

**Note:** nvchecker is particularly useful for maintaining multiple packages or tracking fast-moving projects.


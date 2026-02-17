---
name: archlinux-pkgbuild/vcs-packages
description: Use when creating VCS PKGBUILD files (Git, SVN, CVS, Mercurial, Bazaar, Darcs) that fetch development sources - triggers on -git, -svn, -cvs, -hg, -bzr, -darcs suffixes or pkgver() function requirements
---

# VCS Package Creation for Arch Linux

## Overview

**VCS (Version Control System) packages fetch development versions directly from source control repositories.** These packages automatically track upstream development and use pkgver() functions to generate version strings.

**Key principle:** VCS packages should NEVER be updated just for version bumps - only when build logic changes. The pkgver() function handles versioning automatically.

## When to Use

**Use this sub-skill when:**
- Creating packages that track development versions
- Package name ends with: -git, -svn, -cvs, -hg, -bzr, -darcs
- Source comes from version control systems
- Need to implement pkgver() function

**Prerequisites:** Load the main **archlinux-pkgbuild** skill first for core workflow and validation.

## VCS Package General Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Append VCS suffix: -git, -svn, -hg, -bzr, -cvs, -darcs |
| **pkgver initial** | Use placeholder (e.g., r0.0.0) - pkgver() updates it |
| **pkgver() function** | MANDATORY - auto-generates version from VCS state |
| **Checksums** | Always `sha256sums=('SKIP')` for VCS sources |
| **makedepends** | Include VCS tool (git, subversion, cvs, mercurial, bzr) |
| **provides/conflicts** | Usually provides=('pkgname') conflicts=('pkgname') |
| **Update frequency** | NEVER commit for version bumps - only build logic changes |

## Git Packages

### Git Key Rules

| Rule | Description |
|------|-------------|
| **Suffix** | `-git` |
| **Source format** | `git+https://github.com/user/repo.git` or `git+https://...#branch=name` |
| **pkgver format** | `r123.abc1234` (commits + short hash) or version from tags |
| **makedepends** | `git` |
| **Fragments** | `#branch=name`, `#tag=tagname`, `#commit=hash` |

### Git Template (Basic)

```bash
pkgname=example-git
pkgver=r123.abc1234
pkgrel=1
pkgdesc="Example development version"
arch=('x86_64')
url="https://github.com/user/example"
license=('MIT')
depends=('glibc')
makedepends=('git')
provides=("example=${pkgver}")
conflicts=('example')
source=("git+https://github.com/user/example.git")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    # Method 1: Commit count + short hash (most common)
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

build() {
    cd "$srcdir/example"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Git Template (With Tags)

```bash
pkgname=example-git
pkgver=2.1.0.r5.gabc1234
pkgrel=1
pkgdesc="Example from tagged Git repository"
arch=('x86_64')
url="https://github.com/user/example"
license=('GPL')
depends=('lib1')
makedepends=('git')
provides=("example=${pkgver}")
conflicts=('example')
source=("example::git+https://github.com/user/example.git#branch=develop")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    # Method 2: git describe (for tagged repos)
    git describe --long --tags | sed 's/^v//;s/\([^-]*-g\)/r\1/;s/-/./g'
    # Output format: 2.1.0.r5.gabc1234 (tag.r<commits-since-tag>.g<hash>)
}

prepare() {
    cd "$srcdir/example"
    # Apply patches if needed
    # patch -p1 < "$srcdir/fix.patch"
}

build() {
    cd "$srcdir/example"
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
}
```

### Git pkgver() Patterns

**Pattern 1: Commit count + short hash (simple, no tags):**
```bash
pkgver() {
    cd "$srcdir/reponame"
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}
# Output: r456.abc1234
```

**Pattern 2: git describe (for repos with version tags):**
```bash
pkgver() {
    cd "$srcdir/reponame"
    git describe --long --tags | sed 's/^v//;s/\([^-]*-g\)/r\1/;s/-/./g'
}
# Output: 2.1.0.r5.gabc1234
# Format: <tag>.r<commits-since-tag>.g<hash>
```

**Pattern 3: git describe with fallback (handles untagged repos):**
```bash
pkgver() {
    cd "$srcdir/reponame"
    git describe --long --tags 2>/dev/null | sed 's/^v//;s/\([^-]*-g\)/r\1/;s/-/./g' ||
        printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}
```

**Pattern 4: Explicit version from git describe:**
```bash
pkgver() {
    cd "$srcdir/reponame"
    git describe --tags --abbrev=0 | sed 's/^v//'
}
# Output: 1.2.3 (latest tag only, no commit info)
```

### Git Source URL Fragments

**Supported fragments:**

| Fragment | Purpose | Example |
|----------|---------|---------|
| `#branch=name` | Check out specific branch | `git+https://...#branch=develop` |
| `#tag=tagname` | Check out specific tag | `git+https://...#tag=v1.0.0` |
| `#commit=hash` | Check out specific commit | `git+https://...#commit=abc123` |

**Examples:**
```bash
# Track develop branch
source=("git+https://github.com/user/repo.git#branch=develop")

# Specific tag (for stable dev snapshot)
source=("git+https://github.com/user/repo.git#tag=v2.0.0-beta")

# Specific commit (rare, usually for broken HEAD)
source=("git+https://github.com/user/repo.git#commit=abc1234567")

# Multiple Git sources with different branches
source=("example::git+https://github.com/user/example.git#branch=main"
        "plugin::git+https://github.com/user/plugin.git#branch=dev")
```

### Git Submodules

**For repositories with submodules:**

```bash
pkgname=example-git
source=("git+https://github.com/user/example.git")
sha256sums=('SKIP')

prepare() {
    cd "$srcdir/example"
    # Initialize and update submodules
    git submodule update --init --recursive
}
```

**Or use special Git syntax:**
```bash
source=("git+https://github.com/user/example.git#submodules=1")
```

## CVS Packages

**CVS (Concurrent Versions System) is a legacy VCS still used by some projects.**

### CVS Key Rules

| Rule | Description |
|------|-------------|
| **Suffix** | `-cvs` |
| **Source format** | `cvs+:pserver:anonymous:@host:/path#module=name` |
| **pkgver format** | `r123.20260214` (entries count + date) or date only |
| **makedepends** | `cvs` |
| **Authentication** | Use `anonymous:@` not `anonymous@` to avoid password prompts |

### CVS Template (Anonymous Access)

```bash
pkgname=example-cvs
pkgver=r123.20260214
pkgrel=1
pkgdesc="Example CVS package"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('glibc')
makedepends=('cvs')
provides=("example=${pkgver}")
conflicts=('example')
source=("example::cvs+:pserver:anonymous:@cvs.example.com:/cvsroot#module=example")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    # Entries count + date (most precise)
    local entries=$(find . -name Entries -exec cat {} + | grep -c '^/')
    local date=$(date +%Y%m%d)
    printf "r%s.%s" "$entries" "$date"
}

build() {
    cd "$srcdir/example"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### CVS Template (With Specific Tag)

```bash
pkgname=example-cvs
pkgver=r456.20260214
pkgrel=1
pkgdesc="Example CVS package from specific tag"
arch=('x86_64')
url="https://example.com"
license=('MIT')
makedepends=('cvs')
source=("example::cvs+:pserver:anonymous:@cvs.example.com:/cvsroot#module=example&tag=RELEASE_1_0")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    # Use tag + date for tagged checkouts
    printf "r%s.%s" "$(find . -name Entries | wc -l)" "$(date +%Y%m%d)"
}

build() {
    cd "$srcdir/example"
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
}
```

### CVS CVSROOT Formats

**Common CVSROOT formats:**

| Access Method | CVSROOT Format | Example |
|---------------|----------------|---------|
| **pserver (anonymous)** | `:pserver:anonymous:@host:/path` | `:pserver:anonymous:@cvs.gnu.org:/cvsroot` |
| **pserver (password)** | `:pserver:user:pass@host:/path` | `:pserver:user:password@cvs.example.com:/repo` |
| **SSH** | `:ext:user@host:/path` | `:ext:user@cvs.example.com:/cvsroot` |
| **Local** | `/local/path` | `/var/cvs/repository` |

**In PKGBUILD source array:**
```bash
# Anonymous pserver (recommended)
source=("cvs+:pserver:anonymous:@cvs.gnu.org:/cvsroot#module=project")

# With password (avoid if possible, use SSH instead)
source=("cvs+:pserver:user:password@host:/repo#module=project")

# SSH (requires authentication setup)
source=("cvs+:ext:user@cvs.example.com:/cvsroot#module=project")
```

### CVS pkgver() Patterns

**Pattern 1: Date-based (simplest):**
```bash
pkgver() {
    cd "$srcdir/modulename"
    date +%Y%m%d
}
# Output: 20260214
```

**Pattern 2: Entries count + date (recommended):**
```bash
pkgver() {
    cd "$srcdir/modulename"
    local entries=$(find . -name Entries -exec cat {} + | grep -c '^/')
    local date=$(date +%Y%m%d)
    printf "r%s.%s" "$entries" "$date"
}
# Output: r456.20260214
```

**Pattern 3: Last modification time:**
```bash
pkgver() {
    cd "$srcdir/modulename"
    local mtime=$(find . -name Entries -printf '%T@\n' | sort -n | tail -1 | cut -d. -f1)
    printf "r%s" "$mtime"
}
# Output: r1739577600
```

**Pattern 4: Using CVS log (slower but accurate):**
```bash
pkgver() {
    cd "$srcdir/modulename"
    local commits=$(cvs log 2>/dev/null | grep -c '^date:')
    local date=$(date +%Y%m%d)
    printf "r%s.%s" "$commits" "$date"
}
# Output: r234.20260214
```

### CVS Source Fragments

**Supported CVS fragments:**

| Fragment | Purpose | Example |
|----------|---------|---------|
| `#module=name` | Specify CVS module (required) | `#module=project` |
| `#tag=tagname` | Check out specific tag | `#tag=RELEASE_1_0` |
| `#branch=name` | Check out specific branch | `#branch=experimental` |
| `#revision=rev` | Check out specific revision | `#revision=1.45` |

**Examples:**
```bash
# Main trunk
source=("cvs+:pserver:anonymous:@cvs.example.com:/repo#module=project")

# Specific tag
source=("cvs+:pserver:anonymous:@cvs.example.com:/repo#module=project&tag=v1_0")

# Specific branch
source=("cvs+:pserver:anonymous:@cvs.example.com:/repo#module=project&branch=dev")
```

### CVS Authentication

**For anonymous access (recommended):**
```bash
# Use colon before @ to avoid password prompt
source=("cvs+:pserver:anonymous:@cvs.example.com:/cvsroot#module=project")
```

**For SSH access:**
```bash
makedepends=('cvs' 'openssh')
source=("cvs+:ext:username@cvs.example.com:/cvsroot#module=project")
```

**Set up CVS_RSH for SSH:**
```bash
build() {
    export CVS_RSH=ssh
    cd "$srcdir/project"
    ./configure --prefix=/usr
    make
}
```

### CVS Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Password prompt during build | Using `anonymous@` instead of `anonymous:@` | Use `anonymous:@` in CVSROOT |
| Module not found | Wrong module name in fragment | Check `cvs ls` or upstream docs |
| Connection timeout | Server unreachable or wrong CVSROOT | Verify CVSROOT with `cvs -d ... login` |
| Stale sources | makepkg not updating CVS checkout | Remove source in `$SRCDEST` and rebuild |
| Missing CVS command | `cvs` not in makedepends | Add `cvs` to makedepends |
| SSH authentication fails | No SSH keys or wrong username | Set up SSH keys, verify username |
| Empty pkgver | pkgver() accessing wrong directory | Ensure `cd` to correct source directory |

### CVS Testing Connection

```bash
# Test anonymous pserver connection
cvs -d :pserver:anonymous:@cvs.example.com:/cvsroot login
cvs -d :pserver:anonymous:@cvs.example.com:/cvsroot co module

# List available modules
cvs -d :pserver:anonymous:@cvs.example.com:/cvsroot co -c
```

## SVN (Subversion) Packages

### SVN Key Rules

| Rule | Description |
|------|-------------|
| **Suffix** | `-svn` |
| **Source format** | `svn+https://svn.example.com/repo/trunk` |
| **pkgver format** | `r12345` (revision number) |
| **makedepends** | `subversion` |
| **Fragments** | `#revision=1234` for specific revision |

### SVN Template

```bash
pkgname=example-svn
pkgver=r12345
pkgrel=1
pkgdesc="Example Subversion package"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('glibc')
makedepends=('subversion')
provides=("example=${pkgver}")
conflicts=('example')
source=("example::svn+https://svn.example.com/example/trunk")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    local ver="$(svnversion)"
    printf "r%s" "${ver//[[:alpha:]]}"
}

build() {
    cd "$srcdir/example"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### SVN pkgver() Function

```bash
pkgver() {
    cd "$srcdir/reponame"
    local ver="$(svnversion)"
    printf "r%s" "${ver//[[:alpha:]]}"
}
# Output: r12345
```

### SVN Source URL Fragments

```bash
# Trunk (development)
source=("svn+https://svn.example.com/repo/trunk")

# Specific branch
source=("svn+https://svn.example.com/repo/branches/stable")

# Specific tag
source=("svn+https://svn.example.com/repo/tags/v1.0")

# Specific revision
source=("svn+https://svn.example.com/repo/trunk#revision=1234")
```

## Mercurial (Hg) Packages

### Mercurial Key Rules

| Rule | Description |
|------|-------------|
| **Suffix** | `-hg` |
| **Source format** | `hg+https://hg.example.com/repo` |
| **pkgver format** | `r123.abc1234` (revision + hash) |
| **makedepends** | `mercurial` |

### Mercurial Template

```bash
pkgname=example-hg
pkgver=r123.abc1234
pkgrel=1
pkgdesc="Example Mercurial package"
arch=('x86_64')
url="https://example.com"
license=('MIT')
depends=('glibc')
makedepends=('mercurial')
provides=("example=${pkgver}")
conflicts=('example')
source=("hg+https://hg.example.com/example")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    printf "r%s.%s" "$(hg identify -n)" "$(hg identify -i)"
}

build() {
    cd "$srcdir/example"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
}
```

### Mercurial pkgver() Function

```bash
pkgver() {
    cd "$srcdir/reponame"
    printf "r%s.%s" "$(hg identify -n)" "$(hg identify -i)"
}
# Output: r123.abc1234
```

## Bazaar (Bzr) Packages

### Bazaar Key Rules

| Rule | Description |
|------|-------------|
| **Suffix** | `-bzr` |
| **Source format** | `bzr+lp:project` or `bzr+https://...` |
| **pkgver format** | `r123` (revision number) |
| **makedepends** | `bzr` |

### Bazaar Template

```bash
pkgname=example-bzr
pkgver=r123
pkgrel=1
pkgdesc="Example Bazaar package"
arch=('x86_64')
url="https://launchpad.net/example"
license=('GPL')
depends=('glibc')
makedepends=('bzr')
provides=("example=${pkgver}")
conflicts=('example')
source=("bzr+lp:example")
sha256sums=('SKIP')

pkgver() {
    cd "$srcdir/example"
    printf "r%s" "$(bzr revno)"
}

build() {
    cd "$srcdir/example"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$srcdir/example"
    make DESTDIR="$pkgdir" install
}
```

### Bazaar pkgver() Function

```bash
pkgver() {
    cd "$srcdir/reponame"
    printf "r%s" "$(bzr revno)"
}
# Output: r123
```

## VCS Package Testing

**Before creating VCS PKGBUILD:**

```bash
# Test if repository is accessible
git clone https://github.com/user/repo.git
svn checkout https://svn.example.com/repo/trunk
cvs -d :pserver:anonymous:@cvs.example.com:/cvsroot co module
hg clone https://hg.example.com/repo
bzr branch lp:project

# Check for build dependencies
cd repo
# Read BUILD.md, INSTALL.md, README.md
```

**Test pkgver() function before building:**

```bash
# In PKGBUILD directory
source PKGBUILD
cd src/reponame
pkgver
# Should output valid version string
```

## VCS Package Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| pkgver() fails | Wrong directory | Ensure `cd` to correct source directory |
| Stale sources | makepkg caching old checkout | Remove from `$SRCDEST` or use `makepkg -C` |
| Wrong VCS tool | Missing makedepends | Add VCS tool to makedepends |
| Network errors | Unreachable repository | Verify URL, check network/firewall |
| Version format invalid | Bad pkgver() output | Test pkgver() function manually |
| Submodules not fetched | Git submodules not initialized | Add `git submodule update --init` to prepare() |

## VCS Package Validation

**Same validation as regular packages:**

```bash
# 1. Check PKGBUILD
namcap PKGBUILD

# 2. Build package
makepkg -f

# 3. Check package
namcap *.pkg.tar.zst

# 4. Test installation
sudo pacman -U *.pkg.tar.zst
```

## VCS Update Policy

**CRITICAL: VCS packages should NOT be updated just for version bumps.**

| Update Reason | Action |
|---------------|--------|
| Version change only | DO NOTHING - pkgver() handles it |
| Build script changes | Update PKGBUILD, increment pkgrel |
| Dependency changes | Update depends, increment pkgrel |
| New patches needed | Add patches, increment pkgrel |
| New build flags | Update build(), increment pkgrel |
| Fix packaging errors | Fix and increment pkgrel |

**When to commit VCS PKGBUILD updates:**
- Build process changes
- Dependency changes
- New configure flags
- Installation path fixes
- License changes
- Packaging errors

**When NOT to commit:**
- Just because upstream made new commits
- Version number changed (pkgver() handles it)
- "Update to latest commit" (meaningless for VCS packages)

## VCS Package Checklist

Before submitting VCS package:

- [ ] Package name has correct VCS suffix (-git, -svn, etc.)
- [ ] pkgver() function implemented and tested
- [ ] pkgver() output format is valid (no slashes, colons, hyphens)
- [ ] VCS tool in makedepends
- [ ] sha256sums=('SKIP') for VCS sources
- [ ] provides=('pkgname') if conflicts with stable package
- [ ] conflicts=('pkgname') if provides stable package name
- [ ] Source URL is correct and accessible
- [ ] Source URL uses HTTPS (not HTTP)
- [ ] namcap validation passes
- [ ] Package builds and installs correctly
- [ ] .SRCINFO generated with `makepkg --printsrcinfo`

## Resources

- VCS guidelines: https://wiki.archlinux.org/title/VCS_package_guidelines
- Git sources: https://wiki.archlinux.org/title/VCS_package_guidelines#Git
- SVN sources: https://wiki.archlinux.org/title/VCS_package_guidelines#Subversion
- CVS sources: https://wiki.archlinux.org/title/VCS_package_guidelines#CVS
- Mercurial sources: https://wiki.archlinux.org/title/VCS_package_guidelines#Mercurial
- Bazaar sources: https://wiki.archlinux.org/title/VCS_package_guidelines#Bazaar

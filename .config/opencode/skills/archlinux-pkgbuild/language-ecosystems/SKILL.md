---
name: archlinux-pkgbuild/language-ecosystems
description: Use when packaging language-specific libraries, modules, gems, or interpreters (Node.js, Python, Perl, PHP, Ruby, Go, Rust, R, Haskell, OCaml, Java, Lisp, Free Pascal, Shell)
---

# Language Ecosystem Packaging

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers language-specific packaging patterns for programming language ecosystems. Each language has unique conventions for modules, dependencies, and installation paths.

---

## Node.js Packages

**Node.js packages distribute JavaScript applications and libraries that run on Node.js runtime.** Prefer system Node.js over bundled versions.

### Node.js Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `nodejs-packagename` or just `packagename` for applications |
| **Architecture** | Usually `any` (pure JS), `x86_64` if native modules |
| **Dependencies** | Must depend on `nodejs` (system Node.js runtime) |
| **NO bundled node** | NEVER bundle Node.js runtime |
| **npm integration** | Use `npm install` with proper flags |
| **Module location** | `/usr/lib/node_modules/packagename/` |
| **Binaries** | Symlink from `/usr/bin/` to node_modules/.bin/ |
| **Prefer source** | Build from source when possible (not prebuilt tarballs) |

### Node.js Directory Structure

| Content Type | Install Location |
|--------------|------------------|
| Node modules | `/usr/lib/node_modules/packagename/` |
| Executable scripts | `/usr/bin/` (symlinks to node_modules) |
| Documentation | `/usr/share/doc/packagename/` |
| Configuration | `/etc/packagename/` |

### Node.js Template (npm package)

```bash
pkgname=nodejs-example
_npmname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Node.js application"
arch=('any')
url="https://example.com"
license=('MIT')
depends=('nodejs')
makedepends=('npm')
source=("https://registry.npmjs.org/$_npmname/-/$_npmname-$pkgver.tgz")
sha256sums=('...')
noextract=("$_npmname-$pkgver.tgz")

package() {
    npm install -g \
        --prefix="$pkgdir/usr" \
        --cache="$srcdir/npm-cache" \
        --no-audit \
        --no-fund \
        "$srcdir/$_npmname-$pkgver.tgz"
    
    # Fix permissions
    find "$pkgdir/usr" -type d -exec chmod 755 {} +
    
    # Remove references to $pkgdir
    find "$pkgdir" -name package.json -print0 | xargs -0 sed -i "/_where/d"
    
    # Remove cache and unnecessary files
    rm -rf "$pkgdir/usr/etc"
    
    # Install license
    install -Dm644 "$pkgdir/usr/lib/node_modules/$_npmname/LICENSE" \
        "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Node.js Template (from source)

```bash
pkgname=nodejs-example
_gitname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Node.js library"
arch=('any')
url="https://github.com/user/example"
license=('MIT')
depends=('nodejs')
makedepends=('npm')
source=("https://github.com/user/$_gitname/archive/v$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_gitname-$pkgver"
    npm install --production
}

package() {
    cd "$_gitname-$pkgver"
    
    # Install module
    install -dm755 "$pkgdir/usr/lib/node_modules/$_gitname"
    cp -r * "$pkgdir/usr/lib/node_modules/$_gitname/"
    
    # Remove development files
    find "$pkgdir" -name "test" -type d -exec rm -rf {} +
    find "$pkgdir" -name "*.test.js" -delete
    
    # Create bin symlinks
    install -dm755 "$pkgdir/usr/bin"
    ln -s "/usr/lib/node_modules/$_gitname/bin/example" \
        "$pkgdir/usr/bin/example"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Node.js Native Modules

**For packages with C/C++ addons:**

```bash
pkgname=nodejs-example
pkgver=1.0.0
pkgrel=1
arch=('x86_64')  # NOT 'any' - native code
depends=('nodejs')
makedepends=('npm' 'python')  # node-gyp requires python

build() {
    cd "$srcdir/example-$pkgver"
    npm install --production
    
    # Rebuild native modules
    npm rebuild
}
```

### Node.js npm Install Flags

**Important npm flags for packaging:**

| Flag | Purpose |
|------|---------|
| `--prefix=$pkgdir/usr` | Install to package directory |
| `--cache=$srcdir/npm-cache` | Use local cache (not $HOME) |
| `--no-audit` | Skip security audit (build time) |
| `--no-fund` | Disable funding messages |
| `--production` | Install only production dependencies |
| `-g` (global) | Install globally to prefix |

### Node.js Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Wrong install location | Not using --prefix | Use --prefix=$pkgdir/usr |
| Permission errors | npm cache in $HOME | Use --cache=$srcdir/npm-cache |
| $pkgdir in files | npm stores absolute paths | Sed-remove _where from package.json |
| Missing binaries | Executables not symlinked | Create symlinks in /usr/bin/ |
| Wrong architecture | Native modules with arch='any' | Set arch=('x86_64') for native code |
| Build failures | Missing node-gyp deps | Add python to makedepends |

---

## OCaml Packages

**OCaml packages contain libraries and bindings for the OCaml language.**

### OCaml Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `ocaml-*modulename*` (lowercase, hyphens) |
| **Library directory** | `/usr/lib/ocaml/` |
| **META files** | Use ocaml-findlib for installation (`ocamlfind install`) |
| **OCAMLFIND_DESTDIR** | Set to `$pkgdir/usr/lib/ocaml` with `-mkdir` option |
| **Dune build system** | Preferred modern build system for OCaml |
| **Strip option** | Add `options=('!strip')` for bytecode-only packages |
| **Architecture** | Use `x86_64` only if native code; `any` if bytecode only |
| **Dependencies** | Always include `ocaml` in depends, ocaml-findlib if using META |

### OCaml Template (Dune)

```bash
pkgname=ocaml-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example OCaml library"
arch=('x86_64')
url="https://github.com/example/ocaml-example"
license=('MIT')
depends=('ocaml')
makedepends=('dune' 'ocaml-findlib')
source=("https://github.com/example/ocaml-example/archive/$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$srcdir/ocaml-example-$pkgver"
    dune build --release --verbose
}

package() {
    cd "$srcdir/ocaml-example-$pkgver"
    DESTDIR="$pkgdir" dune install --prefix=/usr --libdir=/usr/lib/ocaml
    
    # Remove .install file
    rm -f "$pkgdir/usr/lib/ocaml/example"/*.install
}
```

### OCaml Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Files installed to `/usr/lib/ocaml/site-lib` | Old directory convention | Set `OCAMLFIND_DESTDIR=$pkgdir/usr/lib/ocaml` |
| Strip errors on bytecode | Binary stripping breaks bytecode | Add `options=('!strip')` |
| Wrong architecture | Package has both native/bytecode | Use `x86_64` if native code exists |
| Missing META file | ocaml-findlib not used | Add `ocaml-findlib` to makedepends |

---

## Perl Packages

**Perl packages contain CPAN modules and distributions.**

### Perl Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `perl-*modulename*` (lowercase, hyphens for `::`) |
| **Module directory** | `/usr/lib/perl5/$version/vendor_perl/` |
| **Architecture** | `any` unless XS (compiled C) modules present |
| **PERL_MM_USE_DEFAULT** | Set to `1` to avoid interactive prompts |
| **perllocal.pod removal** | Always remove from package |
| **.packlist removal** | Always remove `.packlist` files |
| **Empty directories** | Remove empty directories with `find ... -delete` |
| **CPAN source URLs** | Use `https://search.cpan.org/CPAN/authors/id/` |

### Perl Template (Makefile.PL)

```bash
pkgname=perl-module-name
_dist=Module-Name
pkgver=1.0.0
pkgrel=1
pkgdesc="Module description"
arch=('any')
url="https://metacpan.org/release/$_dist"
license=('GPL' 'PerlArtistic')
depends=('perl')
makedepends=('perl-extutils-makemaker')
checkdepends=('perl-test-simple')
options=('!emptydirs')
source=("https://search.cpan.org/CPAN/authors/id/A/AU/AUTHOR/$_dist-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$srcdir/$_dist-$pkgver"
    export PERL_MM_USE_DEFAULT=1 PERL_AUTOINSTALL=--skipdeps \
        PERL_MM_OPT="INSTALLDIRS=vendor DESTDIR='$pkgdir'" \
        PERL_MB_OPT="--installdirs vendor --destdir '$pkgdir'" \
        MODULEBUILDRC=/dev/null
    
    perl Makefile.PL
    make
}

check() {
    cd "$srcdir/$_dist-$pkgver"
    export PERL_MM_USE_DEFAULT=1
    make test
}

package() {
    cd "$srcdir/$_dist-$pkgver"
    make install
    
    # Remove perllocal.pod and .packlist
    find "$pkgdir" -name perllocal.pod -delete
    find "$pkgdir" -name .packlist -delete
}
```

### Perl Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Interactive prompts during build | `PERL_MM_USE_DEFAULT` not set | Export `PERL_MM_USE_DEFAULT=1` |
| Files in wrong directory | System perl directory used | Set `PERL_MM_OPT` and `PERL_MB_OPT` |
| `perllocal.pod` in package | Not removed during packaging | Add `find ... -name perllocal.pod -delete` |
| `.packlist` files present | Not removed during packaging | Add `find ... -name .packlist -delete` |
| Wrong architecture | XS module marked as `any` | Change to `x86_64` for compiled modules |

---

## PHP Packages

**PHP packages contain libraries, frameworks, and extensions for PHP.**

### PHP Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `php-*packagename*` (lowercase, hyphens) |
| **Library directory** | `/usr/share/php/` for pure PHP libraries |
| **Extensions directory** | `/usr/lib/php/modules/` for compiled extensions |
| **Architecture** | `any` for pure PHP; `x86_64` for compiled extensions |
| **Autoloading** | Include Composer autoloader or provide autoload script |
| **php.ini integration** | Extensions need config in `/etc/php/conf.d/` |
| **Dependencies** | Include `php` in depends array |

### PHP Template (Library)

```bash
pkgname=php-example
_pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example PHP library"
arch=('any')
url="https://github.com/vendor/example"
license=('MIT')
depends=('php')
source=("https://github.com/vendor/example/archive/$pkgver.tar.gz")
sha256sums=('...')

package() {
    cd "$srcdir/example-$pkgver"
    
    # Install library files
    install -d "$pkgdir/usr/share/php/$_pkgname"
    cp -r src/* "$pkgdir/usr/share/php/$_pkgname/"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### PHP Template (Extension)

```bash
pkgname=php-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example PHP extension"
arch=('x86_64')
url="https://pecl.php.net/package/example"
license=('PHP')
depends=('php')
makedepends=('php-dev')
source=("https://pecl.php.net/get/example-$pkgver.tgz")
sha256sums=('...')

build() {
    cd "$srcdir/example-$pkgver"
    phpize
    ./configure --prefix=/usr
    make
}

check() {
    cd "$srcdir/example-$pkgver"
    make test NO_INTERACTION=1
}

package() {
    cd "$srcdir/example-$pkgver"
    make INSTALL_ROOT="$pkgdir" install
    
    # Install config
    echo "extension=example.so" > example.ini
    install -Dm644 example.ini "$pkgdir/etc/php/conf.d/example.ini"
}
```

### PHP Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Files in `/usr/lib/php` | Wrong installation path | Use `/usr/share/php/` for libraries |
| Extension not loaded | Missing php.ini entry | Create `/etc/php/conf.d/extension.ini` |
| Autoload not working | Composer autoloader missing | Include `vendor/autoload.php` or create custom |
| Wrong architecture | Extension marked as `any` | Use `x86_64` for compiled extensions |
| phpize not found | php-dev not in makedepends | Add `php-dev` to makedepends |

---

## Python Packages

**Python packages contain libraries, applications, and tools for Python.**

### Python Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `python-*modulename*` (lowercase, hyphens) |
| **PEP 517 build** | Use `python-build` + `python-installer` (modern method) |
| **Legacy setuptools** | Only if package doesn't support PEP 517 |
| **Architecture** | `any` for pure Python; `x86_64` if C extensions present |
| **PyPI source URLs** | Use `https://files.pythonhosted.org/packages/source/` |
| **_name variable** | Use `_name=${pkgname#python-}` for PyPI package name |
| **Check function** | Use `pytest` or `python -m unittest` |
| **Site-packages path** | Don't hardcode; use `python -m site --user-site` pattern |
| **Test directories** | Remove `tests/` from package if not needed at runtime |

### Python Template (PEP 517 Modern)

```bash
pkgname=python-example
_name=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Python package"
arch=('any')
url="https://github.com/example/example"
license=('MIT')
depends=('python' 'python-requests')
makedepends=('python-build' 'python-installer' 'python-wheel' 'python-setuptools')
checkdepends=('python-pytest')
source=("https://files.pythonhosted.org/packages/source/${_name::1}/$_name/$_name-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$srcdir/$_name-$pkgver"
    python -m build --wheel --no-isolation
}

check() {
    cd "$srcdir/$_name-$pkgver"
    pytest -v
}

package() {
    cd "$srcdir/$_name-$pkgver"
    python -m installer --destdir="$pkgdir" dist/*.whl
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Python Architecture Detection

| Indicator | Architecture |
|-----------|--------------|
| Pure Python only (`.py` files) | `arch=('any')` |
| C extensions (`.c`, `.so` files) | `arch=('x86_64')` |
| Cython code (`.pyx` files) | `arch=('x86_64')` |
| Rust extensions (`Cargo.toml`) | `arch=('x86_64')` |

### Python Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| `python setup.py install` deprecated | PEP 517 standards | Use `python -m build` + `python -m installer` |
| Wrong site-packages path | Hardcoded Python version | Use dynamic path or let installer handle it |
| Test imports fail | Package not in PYTHONPATH | Set `PYTHONPATH` or install to temp location |
| C extension wrong arch | Marked as `any` | Change to `arch=('x86_64')` |
| Missing `python-build` | Using old build method | Add to makedepends |
| Entry points not working | Setuptools not in makedepends | Add `python-setuptools` even for PEP 517 |

---

## R Packages

**R packages contain libraries and applications for the R statistical computing environment.**

### R Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `r-packagename` (lowercase, must match CRAN/Bioconductor name) |
| **Architecture** | `any` for pure R code, `x86_64` if C/C++/Fortran code present |
| **R dependency** | Always depend on `r` |
| **Installation path** | Use `/usr/lib/R/library/` |
| **makedepends** | Add `gcc-fortran` if Fortran code present |
| **NO manual install** | Use `R CMD INSTALL` (not `install.packages()`) |
| **Check tests** | Run `R CMD check` on built package |

### R Template (CRAN)

```bash
_cranname=example
pkgname=r-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example R package"
arch=('any')
url="https://cran.r-project.org/package=$_cranname"
license=('GPL')
depends=('r')
optdepends=('r-knitr: for vignettes')
source=("https://cran.r-project.org/src/contrib/${_cranname}_${pkgver}.tar.gz")
sha256sums=('...')

build() {
    R CMD INSTALL $_cranname -l "$srcdir"
}

check() {
    cd "$_cranname"
    R CMD check "$srcdir/$_cranname"
}

package() {
    install -dm755 "$pkgdir/usr/lib/R/library"
    cp -r "$srcdir/$_cranname" "$pkgdir/usr/lib/R/library"
}
```

### R Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Missing compiled code | C/Fortran not compiled | Check arch=() is x86_64, add gcc-fortran |
| Wrong install location | Manual installation path | Use R CMD INSTALL to /usr/lib/R/library/ |
| Missing dependencies | R package deps not listed | Check DESCRIPTION file, add to depends |
| Check directory in package | Not removed after build | Remove check/ directory in package() |
| Tests fail | Missing suggested packages | Add to checkdepends or skip tests |

---

## Ruby Packages

**Ruby Gem packages contain libraries and applications for Ruby programming language.**

### Ruby Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `ruby-gemname` (lowercase, match gem name exactly) |
| **Architecture** | `any` for pure Ruby, `x86_64` if native extensions |
| **Ruby dependency** | Always depend on `ruby` |
| **Installation method** | Use `gem install` with `--ignore-dependencies` |
| **Installation path** | `/usr/lib/ruby/gems/$(ruby_ver)/` |
| **NO rdoc/ri** | Use `--no-document` flag |
| **gem2arch tool** | Use to generate initial PKGBUILD |

### Ruby Template

```bash
_gemname=example
pkgname=ruby-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Ruby gem"
arch=('any')
url="https://rubygems.org/gems/$_gemname"
license=('MIT')
depends=('ruby' 'ruby-dependency')
makedepends=('rubygems')
options=('!emptydirs')
source=("https://rubygems.org/downloads/$_gemname-$pkgver.gem")
noextract=("$_gemname-$pkgver.gem")
sha256sums=('...')

package() {
    local _gemdir="$(ruby -e 'puts Gem.default_dir')"
    
    gem install \
        --ignore-dependencies \
        --no-document \
        --install-dir "$pkgdir/$_gemdir" \
        --bindir "$pkgdir/usr/bin" \
        "$_gemname-$pkgver.gem"
    
    # Remove cache
    rm -rf "$pkgdir/$_gemdir/cache"
    
    # Install license
    install -Dm644 "$pkgdir/$_gemdir/gems/$_gemname-$pkgver/LICENSE" \
        "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
    
    # Remove installed license (keep in licenses dir only)
    rm -f "$pkgdir/$_gemdir/gems/$_gemname-$pkgver/LICENSE"
}
```

### Ruby Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| rdoc/ri in package | --no-document not used | Add --no-document to gem install |
| Cache files present | Not removed | rm -rf cache directory |
| Dependencies pulled in | Not using --ignore-dependencies | Add --ignore-dependencies flag |
| Wrong architecture | Native extensions with arch='any' | Change to arch=('x86_64') |
| Build files in package | ext/ directory not removed | Remove ext/ after install |

---

## Rust Packages

**Rust packages are built with Cargo and follow Rust's packaging conventions.**

### Rust Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Use program name (not `rust-*` prefix) |
| **Cargo.lock** | Must be present (use `cargo vendor` or `prepare()` if missing) |
| **Architecture** | `x86_64` (Rust doesn't officially support building for 'any') |
| **makedepends** | Always include `rust` and `cargo` |
| **Vendored dependencies** | Use `cargo vendor` for offline builds |
| **Build flags** | Use `--release --locked` |
| **Install method** | Use `cargo install` with `--root` |

### Rust Template

```bash
pkgname=example-rust
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Rust application"
arch=('x86_64')
url="https://github.com/user/example"
license=('MIT')
depends=('gcc-libs')
makedepends=('rust' 'cargo')
source=("$pkgname-$pkgver.tar.gz::https://static.crates.io/crates/$pkgname/$pkgname-$pkgver.crate")
sha256sums=('...')

prepare() {
    cd "$pkgname-$pkgver"
    
    # Update lockfile if needed
    cargo fetch --locked --target "$CARCH-unknown-linux-gnu"
}

build() {
    cd "$pkgname-$pkgver"
    
    export RUSTUP_TOOLCHAIN=stable
    export CARGO_TARGET_DIR=target
    cargo build --release --locked
}

check() {
    cd "$pkgname-$pkgver"
    
    export RUSTUP_TOOLCHAIN=stable
    cargo test --release --locked
}

package() {
    cd "$pkgname-$pkgver"
    
    cargo install \
        --offline \
        --no-track \
        --path . \
        --root "$pkgdir/usr"
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Rust Build Flags Explanation

| Flag | Purpose |
|------|---------|
| `--release` | Optimized build (not debug) |
| `--locked` | Use Cargo.lock exactly as-is |
| `--frozen` | Like --locked but also fails if lockfile missing |
| `--offline` | No network access during build |
| `--no-track` | Don't record installation in cargo registry |
| `--target` | Specify target triple |

### Rust Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Network access during build | Not using --locked/--frozen | Use --locked flag, run cargo fetch in prepare() |
| Missing Cargo.lock | Upstream doesn't commit it | Run `cargo generate-lockfile` in prepare() |
| Wrong optimization | Debug build installed | Use --release flag |
| Outdated dependencies | Lockfile not updated | Run `cargo update` in prepare() if intentional |
| Build artifacts in /root | CARGO_TARGET_DIR not set | Export CARGO_TARGET_DIR=target |

---

## Shell Packages

**Shell packages provide shell scripts, functions, and extensions.**

### Shell Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Use descriptive name related to the script's purpose |
| **Architecture** | Always `any` |
| **Shebang** | Scripts must have proper shebang (`#!/bin/sh` or `#!/bin/bash`) |
| **Dependencies** | List the shell interpreter (`bash`, `zsh`, `fish`, etc.) |
| **shellcheck** | Validate scripts with `shellcheck` before packaging |
| **Permissions** | Scripts must be executable (`install -Dm755`) |
| **Location** | `/usr/bin/` for executables, `/usr/share/pkgname/` for libraries |

### Shell Template

```bash
pkgname=example-script
pkgver=1.0.0
pkgrel=1
pkgdesc="Example shell script utility"
arch=('any')
url="https://github.com/user/example-script"
license=('MIT')
depends=('bash')
makedepends=('shellcheck')
source=("$pkgname-$pkgver.tar.gz::https://github.com/user/$pkgname/archive/v$pkgver.tar.gz")
sha256sums=('...')

check() {
    cd "$pkgname-$pkgver"
    
    # Validate with shellcheck
    shellcheck -x *.sh
}

package() {
    cd "$pkgname-$pkgver"
    
    # Install main script
    install -Dm755 example.sh "$pkgdir/usr/bin/example"
    
    # Install library scripts
    install -dm755 "$pkgdir/usr/share/$pkgname"
    install -Dm644 lib/*.sh "$pkgdir/usr/share/$pkgname/"
    
    # Install documentation
    install -Dm644 README.md "$pkgdir/usr/share/doc/$pkgname/README.md"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Shell Script Installation Paths

| Script Type | Install Location |
|-------------|------------------|
| Executable scripts | `/usr/bin/scriptname` |
| Shell libraries | `/usr/share/pkgname/lib.sh` |
| Bash completions | `/usr/share/bash-completion/completions/` |
| Zsh completions | `/usr/share/zsh/site-functions/` |
| Fish completions | `/usr/share/fish/vendor_completions.d/` |
| Zsh plugins | `/usr/share/zsh/plugins/pluginname/` |

### Shell Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Script not executable | Wrong install permissions | Use install -Dm755 for scripts |
| Wrong shebang | Incorrect interpreter path | Use #!/bin/sh or #!/bin/bash |
| shellcheck not run | Validation skipped | Add shellcheck to check() function |
| Missing dependencies | External commands not listed | Check script for command usage, add to depends |
| Wrong install location | Script in wrong directory | Follow installation path guidelines |

---

## Go Packages

**Go packages use Go modules for dependency management and require specific build flags.**

### Go Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `go-modulename` for Go-specific tools, otherwise use program name |
| **go dependency** | Add `go` to makedepends |
| **Go modules** | Use `go mod download -modcacherw` in prepare() |
| **GOPATH** | Set `GOPATH="${srcdir}"` to keep modules in build environment |
| **Build flags** | Always use PIE, trimpath, external linkmode for RELRO |
| **Reproducibility** | Use `-trimpath` and set GOPATH for reproducible builds |

### Go Build Flags (MANDATORY)

**Export these before building:**
```bash
export CGO_CPPFLAGS="${CPPFLAGS}"
export CGO_CFLAGS="${CFLAGS}"
export CGO_CXXFLAGS="${CXXFLAGS}"
export CGO_LDFLAGS="${LDFLAGS}"
export GOFLAGS="-buildmode=pie -trimpath -ldflags=-linkmode=external -mod=readonly -modcacherw"
```

**Flag meanings:**
| Flag | Purpose |
|------|---------|
| `-buildmode=pie` | PIE compilation for binary hardening |
| `-trimpath` | Remove full paths for reproducible builds |
| `-mod=readonly` | Prevent module file updates |
| `-modcacherw` | Make module cache writable (cleanup) |
| `-linkmode=external` | Use external linker for RELRO support |

### Go Template

```bash
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Go application"
arch=('x86_64')
url="https://example.com"
license=('MIT')
makedepends=('go')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

prepare() {
    cd "$pkgname-$pkgver"
    
    # Download Go modules
    export GOPATH="${srcdir}"
    go mod download -modcacherw
    
    # Create output directory
    mkdir -p build
}

build() {
    cd "$pkgname-$pkgver"
    
    export CGO_CPPFLAGS="${CPPFLAGS}"
    export CGO_CFLAGS="${CFLAGS}"
    export CGO_CXXFLAGS="${CXXFLAGS}"
    export CGO_LDFLAGS="${LDFLAGS}"
    export GOPATH="${srcdir}"
    export GOFLAGS="-buildmode=pie -trimpath -ldflags=-linkmode=external -mod=readonly -modcacherw"
    
    go build -o build .
}

check() {
    cd "$pkgname-$pkgver"
    go test ./...
}

package() {
    cd "$pkgname-$pkgver"
    install -Dm755 build/$pkgname "$pkgdir/usr/bin/$pkgname"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Go Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| No RELRO | Missing CGO_LDFLAGS or -linkmode=external | Export CGO_LDFLAGS, use -linkmode=external |
| Not reproducible | Missing -trimpath or GOPATH not set | Add -trimpath, set GOPATH="${srcdir}" |
| Makefile ignores flags | Upstream Makefile overrides GOFLAGS | Patch Makefile or skip it |
| GOPATH grows | Default GOPATH used | Set GOPATH="${srcdir}" in prepare() |

---

## Haskell Packages

**Haskell packages use GHC (Glasgow Haskell Compiler) and require coordinated rebuilds.**

### Haskell Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `haskell-libraryname` (all lowercase, match Hackage name) |
| **Architecture** | Always `x86_64` (all Haskell code is arch-dependent) |
| **Source** | Prefer Hackage (hackage.haskell.org) |
| **Dependencies** | Use ghc-libs (runtime) and ghc (build time) |
| **Registration** | Install register.sh/unregister.sh scripts |
| **Coordinated updates** | Library updates require rebuilding ALL dependents |

### Haskell Template

```bash
# Maintainer: Your Name <youremail at domain dot com>
_hkgname=example
pkgname=haskell-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Haskell library"
arch=('x86_64')
url="https://hackage.haskell.org/package/$_hkgname"
license=('BSD')
depends=('ghc-libs' 'haskell-base')
makedepends=('ghc')
source=("https://hackage.haskell.org/packages/archive/$_hkgname/$pkgver/$_hkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_hkgname-$pkgver"
    
    runhaskell Setup configure -O --enable-shared --enable-debug-info \
        --enable-executable-dynamic --disable-library-vanilla \
        --prefix=/usr --docdir="/usr/share/doc/$pkgname" --datasubdir=$pkgname \
        --enable-tests \
        --dynlibdir=/usr/lib --libsubdir=\$compiler/site-local/\$pkgid \
        --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
        --ghc-option='-pie'
    
    runhaskell Setup build $MAKEFLAGS
    runhaskell Setup register --gen-script
    runhaskell Setup unregister --gen-script
    sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
    sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
}

check() {
    cd "$_hkgname-$pkgver"
    runhaskell Setup test
}

package() {
    cd "$_hkgname-$pkgver"
    
    install -D -m744 register.sh "$pkgdir/usr/share/haskell/register/$pkgname.sh"
    install -D -m744 unregister.sh "$pkgdir/usr/share/haskell/unregister/$pkgname.sh"
    runhaskell Setup copy --destdir="$pkgdir"
    install -D -m644 "LICENSE" "${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
    rm -f "${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
}
```

### Haskell Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Dependency not found | Dependent not rebuilt | Rebuild all reverse dependencies |
| Build fails after update | Incompatible library versions | Check upstream changelog, may need revert |
| Missing registration scripts | Scripts not installed | Install register.sh/unregister.sh |
| Library not found at runtime | ghc-libs not in depends | Add ghc-libs to depends |
| Wrong architecture | Set to 'any' | Always use x86_64 |

---

## Java Packages

**Java packages require special handling for classpath management and dependency isolation.**

### Java Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `java-libraryname` for generic names, otherwise use program name |
| **JAR location** | All JARs in `/usr/share/java/myprogram/` (including bundled deps) |
| **Shell wrapper** | Required for end-user programs (in `/usr/bin/`) |
| **CLASSPATH** | Set via `-cp` option (not environment variable) |
| **Dependencies** | `java-runtime` (runtime) or `java-environment` (build time) |
| **Refactor dependencies** | Separate common/large libraries into own packages when possible |

### Java Template

```bash
pkgname=example-java
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Java application"
arch=('any')
url="https://example.com"
license=('GPL')
depends=('java-runtime>=8')
makedepends=('java-environment>=8' 'maven')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    mvn clean package
}

package() {
    cd "$pkgname-$pkgver"
    
    # Install main JAR and dependencies
    install -dm755 "$pkgdir/usr/share/java/$pkgname"
    install -Dm644 target/*.jar "$pkgdir/usr/share/java/$pkgname/"
    install -Dm644 lib/*.jar "$pkgdir/usr/share/java/$pkgname/"
    
    # Create wrapper script
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << 'EOF'
#!/bin/sh
# Build classpath
CP=""
for jar in /usr/share/java/example-java/*.jar; do
    CP="$CP:$jar"
done

# Run main class
exec /usr/bin/java -cp "$CP" com.example.MainClass "$@"
EOF
    
    # Install license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Java Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| JAVA_HOME tests in scripts | Upstream script checks JAVA_HOME | Remove checks, use /usr/bin/java directly |
| Bundled dependencies | All deps in single directory | Initially OK, refactor common deps over time |
| Wrong wrapper location | Script in /opt or /usr/share | Must be in /usr/bin |
| CLASSPATH environment | Set via export | Use -cp flag instead |

---

## Lisp Packages

**Common Lisp packages require ASDF for system loading and specific directory conventions.**

### Lisp Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `cl-packagename` in AUR (e.g., `cl-ppcre`) |
| **Source location** | `/usr/share/common-lisp/source/pkgname/` (use upstream name) |
| **ASDF registry** | Symlinks to `*.asd` in `/usr/share/common-lisp/systems/` |
| **Avoid ASDF-Install** | Don't use for system-wide installation |
| **Cross-platform** | Support multiple Lisp implementations when possible |
| **Dependencies** | `common-lisp` for cross-platform, specific impl if needed |

### Lisp Template

```bash
pkgname=cl-example
_lispname=example  # Upstream name
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Common Lisp library"
arch=('any')
url="https://example.com"
license=('MIT')
depends=('common-lisp')
install=${pkgname}.install
source=("https://example.com/${_lispname}-${pkgver}.tar.gz")
sha256sums=('...')

package() {
  cd "${_lispname}-${pkgver}"
  
  # Install sources
  install -dm755 "$pkgdir/usr/share/common-lisp/source/${_lispname}"
  install -Dm644 *.lisp "$pkgdir/usr/share/common-lisp/source/${_lispname}/"
  install -Dm644 *.asd "$pkgdir/usr/share/common-lisp/source/${_lispname}/"
  
  # Create ASDF registry symlinks
  install -dm755 "$pkgdir/usr/share/common-lisp/systems"
  pushd "$pkgdir/usr/share/common-lisp/systems"
  ln -s ../source/${_lispname}/${_lispname}.asd .
  popd
  
  # Install license
  install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Lisp Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Wrong directory name | Using AUR name instead of upstream | Use upstream name in source/ path |
| Broken symlinks | Absolute path in symlink | Use relative path: ../source/pkgname/file.asd |
| ASDF not found | Missing dependency | Add asdf package or use impl with built-in ASDF |
| Compilation fails | Wrong ASDF setup | Verify central-registry path |

---

## Free Pascal Packages

**Free Pascal Compiler (FPC) packages for Pascal/Delphi-compatible projects.**

### Free Pascal Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | Use project name (cross-compile: `fpc32-` or `fpc-cpu-system-`) |
| **fpc dependency** | Add `fpc` to `makedepends` or `depends` |
| **Units location** | `/usr/lib/fpc/$_fpcver/units/$_unitdir/` |
| **Architecture** | `x86_64` (or `any` for cross-compile, `x86_64` for multilib) |
| **Static libraries** | Add `staticlibs` to `options` if installing import libs |
| **Cross-compile strip** | Add `!strip` for non-Unix targets |

### Free Pascal Template

```bash
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Free Pascal application"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('fpc')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    
    # Build with FPC
    fpc -O3 -XX -CX src/example.pas
}

package() {
    cd "$pkgname-$pkgver"
    
    # Determine FPC paths
    local _unitdir=$(fpc -iSP)-$(fpc -iSO)
    local _fpcver=$(fpc -iV)
    
    # Install binary
    install -Dm755 example "$pkgdir/usr/bin/example"
    
    # Install compiled units
    install -dm755 "$pkgdir/usr/lib/fpc/$_fpcver/units/$_unitdir/$pkgname"
    install -Dm644 *.o *.ppu *.a "$pkgdir/usr/lib/fpc/$_fpcver/units/$_unitdir/$pkgname/"
}
```

### Free Pascal Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Units in wrong location | Wrong path | Use /usr/lib/fpc/$_fpcver/units/$_unitdir/ |
| Missing fpc dependency | Not in makedepends/depends | Add to appropriate array |
| Strip breaks cross-compile | Unix strip on non-Unix binary | Add `!strip` to options |
| Missing staticlibs | Import library without flag | Add `staticlibs` to options |

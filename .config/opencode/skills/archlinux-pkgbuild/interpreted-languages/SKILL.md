---
name: archlinux-pkgbuild/interpreted-languages
description: Use when packaging interpreted language ecosystems (Node.js, Python, Perl, PHP, Ruby, R, Shell, Lisp) - package managers, installation paths, environment setup
---

# Interpreted Language Ecosystems

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers interpreted language packaging where source code is executed by runtime interpreters.

## Quick Reference

| Language | Architecture | Package Manager | Install Location |
|----------|--------------|-----------------|------------------|
| Node.js | any/x86_64 | npm | /usr/lib/node_modules/ |
| Python | any/x86_64 | pip/build | /usr/lib/python3.*/site-packages/ |
| Perl | any/x86_64 | cpan | /usr/lib/perl5/vendor_perl/ |
| PHP | any/x86_64 | composer | /usr/share/php/ |
| Ruby | any/x86_64 | gem | /usr/lib/ruby/gems/ |
| R | any/x86_64 | R CMD INSTALL | /usr/lib/R/library/ |
| Shell | any | N/A | /usr/bin/, /usr/share/ |
| Lisp | any | ASDF | /usr/share/common-lisp/ |

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

> **IMPORTANT:** Most PHP applications are web applications.  
> When packaging PHP web applications, you MUST also follow the **Web Application Guidelines** in `archlinux-pkgbuild/system-packages`.  
> See: Web Application Packaging section for directory structure, configuration separation, writable directories, and web server integration.

### PHP Key Rules

| Rule | Description |
|------|-------------|
| **Package naming** | `php-*packagename*` (lowercase, hyphens) |
| **Library directory** | `/usr/share/php/` for pure PHP libraries |
| **Web applications** | `/usr/share/webapps/appname/` - See system-packages skill for full guidelines |
| **Extensions directory** | `/usr/lib/php/modules/` for compiled extensions |
| **Architecture** | `any` for pure PHP; `x86_64` for compiled extensions |
| **Autoloading** | Include Composer autoloader or provide autoload script |
| **php.ini integration** | Extensions need config in `/etc/php/conf.d/` |
| **Dependencies** | Include `php` in depends array; verify extensions exist (no php-intl, php-mbstring) |

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
| php-intl dependency error | Package doesn't exist | Built into `php` - remove from depends=() |
| php-mbstring dependency error | Package doesn't exist | Built into `php` - remove from depends=() |
| Web app writable dirs in /usr | FHS violation | See system-packages skill for proper structure |

### PHP Web Applications

**For PHP web applications, you MUST follow the guidelines in `archlinux-pkgbuild/system-packages`.**

Key requirements:
- Install to `/usr/share/webapps/appname/`
- Config files in `/etc/webapps/appname/` with symlinks
- Writable directories in `/var/lib/appname/` (use systemd-tmpfiles.d)
- Provide Apache/Nginx example configs in `/etc/webapps/appname/`
- Use `backup=()` for configuration files
- **Add PHP module enabling instructions to .install file if extensions need enabling:**
  - Check which extensions require php.ini configuration (use `pacman -Ql php | grep "\.so$"`)
  - Built-in extensions like `mbstring` are always enabled (no action needed)
  - Extensions with `.so` files (e.g., `gd`, `intl`) may need enabling in `/etc/php/php.ini`
  - **Example .install template:**
    ```bash
    post_install() {
        cat <<EOF
    ==> PHP Extensions
        Required PHP extensions: gd, intl
        
        Install packages:
            pacman -S php-gd
        
        Enable extensions in /etc/php/php.ini:
            Uncomment or add these lines:
                extension=gd
                extension=intl
        
        Restart web server:
            systemctl restart httpd    # Apache
            systemctl restart php-fpm  # Nginx
    EOF
    }
    ```

**See the Web Application Packaging section in system-packages skill for complete guidelines, directory structure, and templates.**

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

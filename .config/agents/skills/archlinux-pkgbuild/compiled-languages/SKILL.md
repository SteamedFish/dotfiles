---
name: archlinux-pkgbuild/compiled-languages
description: Use when packaging compiled language ecosystems (Go, Rust, Haskell, OCaml, Free Pascal, Java) - build flags, dependencies, reproducibility
---

# Compiled Language Ecosystems

**For core PKGBUILD workflow, use archlinux-pkgbuild**

This sub-skill covers compiled language packaging patterns where source code is transformed into native binaries or bytecode.

## Quick Reference

| Language | Architecture | Key Tool | Install Location |
|----------|--------------|----------|------------------|
| Go | x86_64 | go build | /usr/bin/ |
| Rust | x86_64 | cargo | /usr/bin/ |
| Haskell | x86_64 | ghc | /usr/lib/\$compiler/site-local/ |
| OCaml | x86_64/any | dune/ocamlfind | /usr/lib/ocaml/ |
| Free Pascal | x86_64 | fpc | /usr/lib/fpc/\$ver/units/ |
| Java | any | javac/maven | /usr/share/java/ |

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

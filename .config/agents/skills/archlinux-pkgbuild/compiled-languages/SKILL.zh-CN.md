---
name: archlinux-pkgbuild/compiled-languages
description: Use when packaging compiled language ecosystems (Go, Rust, Haskell, OCaml, Free Pascal, Java) - build flags, dependencies, reproducibility
---

# 编译型语言生态系统

**有关核心 PKGBUILD 工作流程，请使用 archlinux-pkgbuild**

本子技能涵盖源代码被转换为本机二进制文件或字节码的编译型语言打包模式。

## 快速参考

| 语言 | 架构 | 关键工具 | 安装位置 |
|----------|--------------|----------|------------------|
| Go | x86_64 | go build | /usr/bin/ |
| Rust | x86_64 | cargo | /usr/bin/ |
| Haskell | x86_64 | ghc | /usr/lib/\$compiler/site-local/ |
| OCaml | x86_64/any | dune/ocamlfind | /usr/lib/ocaml/ |
| Free Pascal | x86_64 | fpc | /usr/lib/fpc/\$ver/units/ |
| Java | any | javac/maven | /usr/share/java/ |

---

## Go 软件包

**Go 软件包使用 Go 模块进行依赖管理，需要特定的构建标志。**

### Go 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `go-modulename` 用于 Go 特定工具，否则使用程序名称 |
| **go dependency** | 添加 `go` 到 makedepends |
| **Go 模块** | 在 prepare() 中使用 `go mod download -modcacherw` |
| **GOPATH** | 设置 `GOPATH="${srcdir}"` 以在构建环境中保留模块 |
| **构建标志** | 始终使用 PIE、trimpath、外部 linkmode 以支持 RELRO |
| **可重现性** | 使用 `-trimpath` 并设置 GOPATH 以实现可重现构建 |

### Go 构建标志（强制）

**在构建前导出这些：**
```bash
export CGO_CPPFLAGS="${CPPFLAGS}"
export CGO_CFLAGS="${CFLAGS}"
export CGO_CXXFLAGS="${CXXFLAGS}"
export CGO_LDFLAGS="${LDFLAGS}"
export GOFLAGS="-buildmode=pie -trimpath -ldflags=-linkmode=external -mod=readonly -modcacherw"
```

**标志含义：**
| 标志 | 目的 |
|------|---------|
| `-buildmode=pie` | PIE 编译以实现二进制加固 |
| `-trimpath` | 移除完整路径以实现可重现构建 |
| `-mod=readonly` | 防止模块文件更新 |
| `-modcacherw` | 使模块缓存可写（清理） |
| `-linkmode=external` | 使用外部链接器以支持 RELRO |

### Go 模板

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
    
    # 下载 Go 模块
    export GOPATH="${srcdir}"
    go mod download -modcacherw
    
    # 创建输出目录
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

### Go 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 没有 RELRO | 缺少 CGO_LDFLAGS 或 -linkmode=external | 导出 CGO_LDFLAGS，使用 -linkmode=external |
| 不可重现 | 缺少 -trimpath 或未设置 GOPATH | 添加 -trimpath，设置 GOPATH="${srcdir}" |
| Makefile 忽略标志 | 上游 Makefile 覆盖 GOFLAGS | 修补 Makefile 或跳过它 |
| GOPATH 增长 | 使用默认 GOPATH | 在 prepare() 中设置 GOPATH="${srcdir}" |

---

## Rust 软件包

**Rust 软件包使用 Cargo 构建，并遵循 Rust 的打包约定。**

### Rust 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 使用程序名称（不是 `rust-*` 前缀） |
| **Cargo.lock** | MUST（必须）存在（如果缺失，使用 `cargo vendor` 或 `prepare()`） |
| **Architecture** | `x86_64`（Rust 官方不支持为 'any' 构建） |
| **makedepends** | 始终包含 `rust` 和 `cargo` |
| **Vendored dependencies** | 使用 `cargo vendor` 进行离线构建 |
| **构建标志** | 使用 `--release --locked` |
| **安装方法** | 使用带 `--root` 的 `cargo install` |

### Rust 模板

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
    
    # 如果需要，更新 lockfile
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
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Rust 构建标志说明

| 标志 | 目的 |
|------|---------|
| `--release` | 优化构建（不是 debug） |
| `--locked` | 完全按原样使用 Cargo.lock |
| `--frozen` | 类似 --locked，但如果缺少 lockfile 也会失败 |
| `--offline` | 构建期间无网络访问 |
| `--no-track` | 不在 cargo 注册表中记录安装 |
| `--target` | 指定目标三元组 |

### Rust 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 构建期间网络访问 | 未使用 --locked/--frozen | 使用 --locked 标志，在 prepare() 中运行 cargo fetch |
| 缺少 Cargo.lock | 上游未提交它 | 在 prepare() 中运行 `cargo generate-lockfile` |
| 错误的优化 | 安装了 debug 构建 | 使用 --release 标志 |
| 过时的依赖 | Lockfile 未更新 | 如果是有意的，在 prepare() 中运行 `cargo update` |
| /root 中的构建产物 | 未设置 CARGO_TARGET_DIR | 导出 CARGO_TARGET_DIR=target |

---

## Haskell 软件包

**Haskell 软件包使用 GHC（Glasgow Haskell Compiler），需要协调重建。**

### Haskell 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `haskell-libraryname`（全部小写，匹配 Hackage 名称） |
| **Architecture** | 始终 `x86_64`（所有 Haskell 代码都依赖于架构） |
| **Source** | 首选 Hackage（hackage.haskell.org） |
| **Dependencies** | 对 ghc-libs 使用运行时依赖，对 ghc 使用构建时依赖 |
| **Registration** | 安装 register.sh/unregister.sh 脚本 |
| **Coordinated updates** | 库更新需要重建 ALL（所有）依赖项 |

### Haskell 模板

```bash
# 维护者：Your Name <youremail at domain dot com>
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

### Haskell 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 找不到依赖 | 依赖项未重建 | 重建所有反向依赖 |
| 更新后构建失败 | 不兼容的库版本 | 检查上游变更日志，可能需要回退 |
| 缺少注册脚本 | 脚本未安装 | 安装 register.sh/unregister.sh |
| 运行时找不到库 | ghc-libs 不在 depends 中 | 添加 ghc-libs 到 depends |
| 错误的架构 | 设置为 'any' | 始终使用 x86_64 |

---

## OCaml 软件包

**OCaml 软件包包含用于 OCaml 语言的库和绑定。**

### OCaml 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `ocaml-*modulename*`（小写，连字符） |
| **Library directory** | `/usr/lib/ocaml/` |
| **META files** | 使用 ocaml-findlib 进行安装（`ocamlfind install`） |
| **OCAMLFIND_DESTDIR** | 设置为 `$pkgdir/usr/lib/ocaml` 并使用 `-mkdir` 选项 |
| **Dune build system** | OCaml 首选的现代构建系统 |
| **Strip option** | 对仅字节码的软件包添加 `options=('!strip')` |
| **Architecture** | 如果为本机代码，使用 `x86_64`；如果仅为字节码，使用 `any` |
| **Dependencies** | 始终在 depends 中包含 `ocaml`，如果使用 META，则包含 ocaml-findlib |

### OCaml 模板（Dune）

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
    
    # 移除 .install 文件
    rm -f "$pkgdir/usr/lib/ocaml/example"/*.install
}
```

### OCaml 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 文件安装到 `/usr/lib/ocaml/site-lib` | 旧目录约定 | 设置 `OCAMLFIND_DESTDIR=$pkgdir/usr/lib/ocaml` |
| 字节码上的剥离错误 | 二进制剥离破坏字节码 | 添加 `options=('!strip')` |
| 错误的架构 | 软件包同时具有本机/字节码 | 如果存在本机代码，使用 `x86_64` |
| 缺少 META 文件 | 未使用 ocaml-findlib | 添加 `ocaml-findlib` 到 makedepends |

---

## Free Pascal 软件包

**Free Pascal Compiler（FPC）软件包用于 Pascal/Delphi 兼容项目。**

### Free Pascal 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 使用项目名称（交叉编译：`fpc32-` 或 `fpc-cpu-system-`） |
| **fpc dependency** | 添加 `fpc` 到 `makedepends` 或 `depends` |
| **Units location** | `/usr/lib/fpc/$_fpcver/units/$_unitdir/` |
| **Architecture** | `x86_64`（或交叉编译 `any`，multilib `x86_64`） |
| **Static libraries** | 如果安装导入库，添加 `staticlibs` 到 `options` |
| **Cross-compile strip** | 对非 Unix 目标添加 `!strip` |

### Free Pascal 模板

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
    
    # 使用 FPC 构建
    fpc -O3 -XX -CX src/example.pas
}

package() {
    cd "$pkgname-$pkgver"
    
    # 确定 FPC 路径
    local _unitdir=$(fpc -iSP)-$(fpc -iSO)
    local _fpcver=$(fpc -iV)
    
    # 安装二进制文件
    install -Dm755 example "$pkgdir/usr/bin/example"
    
    # 安装编译的单元
    install -dm755 "$pkgdir/usr/lib/fpc/$_fpcver/units/$_unitdir/$pkgname"
    install -Dm644 *.o *.ppu *.a "$pkgdir/usr/lib/fpc/$_fpcver/units/$_unitdir/$pkgname/"
}
```

### Free Pascal 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 单元在错误位置 | 错误的路径 | 使用 /usr/lib/fpc/$_fpcver/units/$_unitdir/ |
| 缺少 fpc 依赖 | 不在 makedepends/depends 中 | 添加到适当的数组 |
| 剥离破坏交叉编译 | 在非 Unix 二进制文件上使用 Unix strip | 添加 `!strip` 到 options |
| 缺少 staticlibs | 没有标志的导入库 | 添加 `staticlibs` 到 options |

---

## Java 软件包

**Java 软件包需要特殊处理以进行类路径管理和依赖隔离。**

### Java 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `java-libraryname` 用于通用名称，否则使用程序名称 |
| **JAR location** | 所有 JAR 都在 `/usr/share/java/myprogram/` 中（包括捆绑的依赖） |
| **Shell wrapper** | 最终用户程序必需（在 `/usr/bin/` 中） |
| **CLASSPATH** | 通过 `-cp` 选项设置（不是环境变量） |
| **Dependencies** | `java-runtime`（运行时）或 `java-environment`（构建时） |
| **Refactor dependencies** | 尽可能将通用/大型库分离到单独的软件包中 |

### Java 模板

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
    
    # 安装主 JAR 和依赖
    install -dm755 "$pkgdir/usr/share/java/$pkgname"
    install -Dm644 target/*.jar "$pkgdir/usr/share/java/$pkgname/"
    install -Dm644 lib/*.jar "$pkgdir/usr/share/java/$pkgname/"
    
    # 创建包装器脚本
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << 'EOF'
#!/bin/sh
# 构建类路径
CP=""
for jar in /usr/share/java/example-java/*.jar; do
    CP="$CP:$jar"
done

# 运行主类
exec /usr/bin/java -cp "$CP" com.example.MainClass "$@"
EOF
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Java 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 脚本中的 JAVA_HOME 测试 | 上游脚本检查 JAVA_HOME | 移除检查，直接使用 /usr/bin/java |
| 捆绑的依赖 | 所有依赖都在单个目录中 | 最初可以，随着时间的推移重构通用依赖 |
| 错误的包装器位置 | /opt 或 /usr/share 中的脚本 | MUST（必须）在 /usr/bin 中 |
| CLASSPATH 环境 | 通过 export 设置 | 改用 -cp 标志 |

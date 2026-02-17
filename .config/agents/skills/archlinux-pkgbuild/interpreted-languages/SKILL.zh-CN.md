---
name: archlinux-pkgbuild/interpreted-languages
description: Use when packaging interpreted language ecosystems (Node.js, Python, Perl, PHP, Ruby, R, Shell, Lisp) - package managers, installation paths, environment setup
---

# 解释型语言生态系统

**有关核心 PKGBUILD 工作流程，请使用 archlinux-pkgbuild**

本子技能涵盖源代码由运行时解释器执行的解释型语言打包。

## 快速参考

| 语言 | 架构 | 包管理器 | 安装位置 |
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

## Node.js 软件包

**Node.js 软件包分发在 Node.js 运行时上运行的 JavaScript 应用和库。** 尽可能使用系统 Node.js 而不是捆绑版本。

### Node.js 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `nodejs-packagename` 或仅应用程序的 `packagename` |
| **Architecture** | 通常 `any`（纯 JS），如果本机模块，使用 `x86_64` |
| **Dependencies** | 必须依赖于 `nodejs`（系统 Node.js 运行时） |
| **NO bundled node** | NEVER（绝不）捆绑 Node.js 运行时 |
| **npm integration** | 使用带有适当标志的 `npm install` |
| **Module location** | `/usr/lib/node_modules/packagename/` |
| **Binaries** | 从 `/usr/bin/` 到 node_modules/.bin/ 的符号链接 |
| **Prefer source** | 尽可能从源代码构建（不是预构建的 tarballs） |

### Node.js 目录结构

| 内容类型 | 安装位置 |
|--------------|------------------|
| Node 模块 | `/usr/lib/node_modules/packagename/` |
| 可执行脚本 | `/usr/bin/`（到 node_modules 的符号链接） |
| 文档 | `/usr/share/doc/packagename/` |
| 配置 | `/etc/packagename/` |

### Node.js 模板（npm 软件包）

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
    
    # 修复权限
    find "$pkgdir/usr" -type d -exec chmod 755 {} +
    
    # 移除对 $pkgdir 的引用
    find "$pkgdir" -name package.json -print0 | xargs -0 sed -i "/_where/d"
    
    # 移除缓存和不必要的文件
    rm -rf "$pkgdir/usr/etc"
    
    # 安装 license
    install -Dm644 "$pkgdir/usr/lib/node_modules/$_npmname/LICENSE" \
        "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Node.js 模板（从源代码）

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
    
    # 安装模块
    install -dm755 "$pkgdir/usr/lib/node_modules/$_gitname"
    cp -r * "$pkgdir/usr/lib/node_modules/$_gitname/"
    
    # 移除开发文件
    find "$pkgdir" -name "test" -type d -exec rm -rf {} +
    find "$pkgdir" -name "*.test.js" -delete
    
    # 创建 bin 符号链接
    install -dm755 "$pkgdir/usr/bin"
    ln -s "/usr/lib/node_modules/$_gitname/bin/example" \
        "$pkgdir/usr/bin/example"
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Node.js 本机模块

**对于带有 C/C++ 插件的软件包：**

```bash
pkgname=nodejs-example
pkgver=1.0.0
pkgrel=1
arch=('x86_64')  # NOT（不是）'any' - 本机代码
depends=('nodejs')
makedepends=('npm' 'python')  # node-gyp 需要 python

build() {
    cd "$srcdir/example-$pkgver"
    npm install --production
    
    # 重建本机模块
    npm rebuild
}
```

### Node.js npm 安装标志

**打包的重要 npm 标志：**

| 标志 | 目的 |
|------|---------|
| `--prefix=$pkgdir/usr` | 安装到软件包目录 |
| `--cache=$srcdir/npm-cache` | 使用本地缓存（不是 $HOME） |
| `--no-audit` | 跳过安全审计（构建时） |
| `--no-fund` | 禁用资金消息 |
| `--production` | 仅安装生产依赖 |
| `-g` (global) | 全局安装到前缀 |

### Node.js 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 错误的安装位置 | 未使用 --prefix | 使用 --prefix=$pkgdir/usr |
| 权限错误 | $HOME 中的 npm 缓存 | 使用 --cache=$srcdir/npm-cache |
| 文件中的 $pkgdir | npm 存储绝对路径 | 从 package.json 中 Sed 移除 _where |
| 缺少二进制文件 | 可执行文件未符号链接 | 在 /usr/bin/ 中创建符号链接 |
| 错误的架构 | 带有 arch='any' 的本机模块 | 为本机代码设置 arch=('x86_64') |
| 构建失败 | 缺少 node-gyp 依赖 | 添加 python 到 makedepends |

---

## Python 软件包

**Python 软件包包含用于 Python 的库、应用和工具。**

### Python 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `python-*modulename*`（小写，连字符） |
| **PEP 517 构建** | 使用 `python-build` + `python-installer`（现代方法） |
| **Legacy setuptools** | 仅当软件包不支持 PEP 517 时 |
| **Architecture** | 纯 Python 使用 `any`；如果存在 C 扩展，使用 `x86_64` |
| **PyPI source URLs** | 使用 `https://files.pythonhosted.org/packages/source/` |
| **_name variable** | 对 PyPI 软件包名称使用 `_name=${pkgname#python-}` |
| **Check function** | 使用 `pytest` 或 `python -m unittest` |
| **Site-packages path** | 不要硬编码；使用 `python -m site --user-site` 模式 |
| **Test directories** | 如果运行时不需要，从软件包中移除 `tests/` |

### Python 模板（PEP 517 现代）

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
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Python 架构检测

| 指示器 | 架构 |
|-----------|--------------|
| 纯 Python（仅 `.py` 文件） | `arch=('any')` |
| C 扩展（`.c`、`.so` 文件） | `arch=('x86_64')` |
| Cython 代码（`.pyx` 文件） | `arch=('x86_64')` |
| Rust 扩展（`Cargo.toml`） | `arch=('x86_64')` |

### Python 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| `python setup.py install` 已弃用 | PEP 517 标准 | 使用 `python -m build` + `python -m installer` |
| 错误的 site-packages 路径 | 硬编码 Python 版本 | 使用动态路径或让安装程序处理 |
| 测试导入失败 | 软件包不在 PYTHONPATH 中 | 设置 `PYTHONPATH` 或安装到临时位置 |
| C 扩展架构错误 | 标记为 `any` | 更改为 `arch=('x86_64')` |
| 缺少 `python-build` | 使用旧构建方法 | 添加到 makedepends |
| 入口点无法工作 | Setuptools 不在 makedepends 中 | 即使对于 PEP 517 也添加 `python-setuptools` |

---

## Perl 软件包

**Perl 软件包包含 CPAN 模块和发行版。**

### Perl 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `perl-*modulename*`（小写，将 `::` 替换为连字符） |
| **Module directory** | `/usr/lib/perl5/$version/vendor_perl/` |
| **Architecture** | 除非存在 XS（编译的 C）模块，否则使用 `any` |
| **PERL_MM_USE_DEFAULT** | 设置为 `1` 以避免交互式提示 |
| **perllocal.pod removal** | 始终从软件包中移除 |
| **.packlist removal** | 始终移除 `.packlist` 文件 |
| **Empty directories** | 使用 `find ... -delete` 移除空目录 |
| **CPAN source URLs** | 使用 `https://search.cpan.org/CPAN/authors/id/` |

### Perl 模板（Makefile.PL）

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
    
    # 移除 perllocal.pod 和 .packlist
    find "$pkgdir" -name perllocal.pod -delete
    find "$pkgdir" -name .packlist -delete
}
```

### Perl 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 构建期间交互式提示 | 未设置 `PERL_MM_USE_DEFAULT` | 导出 `PERL_MM_USE_DEFAULT=1` |
| 文件在错误的目录 | 使用系统 perl 目录 | 设置 `PERL_MM_OPT` 和 `PERL_MB_OPT` |
| 软件包中的 `perllocal.pod` | 打包期间未移除 | 添加 `find ... -name perllocal.pod -delete` |
| 存在 `.packlist` 文件 | 打包期间未移除 | 添加 `find ... -name .packlist -delete` |
| 错误的架构 | XS 模块标记为 `any` | 更改为 `x86_64` 用于编译模块 |

---

## PHP 软件包

**PHP 软件包包含用于 PHP 的库、框架和扩展。**

> **重要：** 大多数 PHP 应用是 Web 应用。  
> 打包 PHP Web 应用时，您 MUST（必须）同时遵循 `archlinux-pkgbuild/system-packages` 中的 **Web 应用指南**。  
> 请参阅：Web 应用打包部分，了解目录结构、配置分离、可写目录和 Web 服务器集成。

### PHP 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `php-*packagename*`（小写，连字符） |
| **Library directory** | 纯 PHP 库使用 `/usr/share/php/` |
| **Web applications** | `/usr/share/webapps/appname/` - 请参阅 system-packages 技能获取完整指南 |
| **Extensions directory** | 编译的扩展使用 `/usr/lib/php/modules/` |
| **Architecture** | 纯 PHP 使用 `any`；编译的扩展使用 `x86_64` |
| **Autoloading** | 包含 Composer 自动加载器或提供自动加载脚本 |
| **php.ini integration** | 扩展需要在 `/etc/php/conf.d/` 中的配置 |
| **Dependencies** | 在 depends 数组中包含 `php`；验证扩展是否存在（无 php-intl、php-mbstring） |

### PHP 模板（库）

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
    
    # 安装库文件
    install -d "$pkgdir/usr/share/php/$_pkgname"
    cp -r src/* "$pkgdir/usr/share/php/$_pkgname/"
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### PHP 模板（扩展）

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
    
    # 安装配置
    echo "extension=example.so" > example.ini
    install -Dm644 example.ini "$pkgdir/etc/php/conf.d/example.ini"
}
```

### PHP 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 文件在 `/usr/lib/php` | 错误的安装路径 | 库使用 `/usr/share/php/` |
| 扩展未加载 | 缺少 php.ini 条目 | 创建 `/etc/php/conf.d/extension.ini` |
| 自动加载无法工作 | 缺少 Composer 自动加载器 | 包含 `vendor/autoload.php` 或创建自定义 |
| 错误的架构 | 扩展标记为 `any` | 编译的扩展使用 `x86_64` |
| 找不到 phpize | php-dev 不在 makedepends 中 | 添加 `php-dev` 到 makedepends |
| php-intl 依赖错误 | 软件包不存在 | 内置到 `php` 中 - 从 depends=() 移除 |
| php-mbstring 依赖错误 | 软件包不存在 | 内置到 `php` 中 - 从 depends=() 移除 |
| /usr 中的 Web 应用可写目录 | 违反 FHS | 请参阅 system-packages 技能获取正确的结构 |

### PHP Web 应用

**对于 PHP Web 应用，您 MUST（必须）遵循 `archlinux-pkgbuild/system-packages` 中的指南。**

关键要求：
- 安装到 `/usr/share/webapps/appname/`
- `/etc/webapps/appname/` 中的配置文件带有符号链接
- `/var/lib/appname/` 中的可写目录（使用 systemd-tmpfiles.d）
- 在 `/etc/webapps/appname/` 中提供 Apache/Nginx 示例配置
- 对配置文件使用 `backup=()`
- **如果扩展需要启用，将 PHP 模块启用说明添加到 .install 文件：**
  - 检查哪些扩展需要 php.ini 配置（使用 `pacman -Ql php | grep "\.so$"`）
  - 内置扩展如 `mbstring` 始终启用（无需操作）
  - 带有 `.so` 文件的扩展（例如 `gd`、`intl`）可能需要在 `/etc/php/php.ini` 中启用
  - **示例 .install 模板：**
    ```bash
    post_install() {
        cat <<EOF
    ==> PHP 扩展
        必需的 PHP 扩展：gd, intl
        
        安装软件包：
            pacman -S php-gd
        
        在 /etc/php/php.ini 中启用扩展：
            取消注释或添加以下行：
                extension=gd
                extension=intl
        
        重启 Web 服务器：
            systemctl restart httpd    # Apache
            systemctl restart php-fpm  # Nginx
    EOF
    }
    ```

**有关完整指南、目录结构和模板，请参阅 system-packages 技能中的 Web 应用打包部分。**

---

## Ruby 软件包

**Ruby Gem 软件包包含用于 Ruby 编程语言的库和应用。**

### Ruby 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `ruby-gemname`（小写，完全匹配 gem 名称） |
| **Architecture** | 纯 Ruby 使用 `any`，如果本机扩展，使用 `x86_64` |
| **Ruby dependency** | 始终依赖于 `ruby` |
| **Installation method** | 使用带 `--ignore-dependencies` 的 `gem install` |
| **Installation path** | `/usr/lib/ruby/gems/$(ruby_ver)/` |
| **NO rdoc/ri** | 使用 `--no-document` 标志 |
| **gem2arch tool** | 用于生成初始 PKGBUILD |

### Ruby 模板

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
    
    # 移除缓存
    rm -rf "$pkgdir/$_gemdir/cache"
    
    # 安装 license
    install -Dm644 "$pkgdir/$_gemdir/gems/$_gemname-$pkgver/LICENSE" \
        "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
    
    # 移除已安装的 license（仅保留在 licenses 目录中）
    rm -f "$pkgdir/$_gemdir/gems/$_gemname-$pkgver/LICENSE"
}
```

### Ruby 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 软件包中的 rdoc/ri | 未使用 --no-document | 添加 --no-document 到 gem install |
| 存在缓存文件 | 未移除 | rm -rf 缓存目录 |
| 依赖被拉入 | 未使用 --ignore-dependencies | 添加 --ignore-dependencies 标志 |
| 错误的架构 | 带有 arch='any' 的本机扩展 | 更改为 arch=('x86_64') |
| 软件包中的构建文件 | 安装后未移除 ext/ 目录 | 安装后移除 ext/ |

---

## R 软件包

**R 软件包包含用于 R 统计计算环境的库和应用。**

### R 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `r-packagename`（小写，必须匹配 CRAN/Bioconductor 名称） |
| **Architecture** | 纯 R 代码使用 `any`，如果存在 C/C++/Fortran 代码，使用 `x86_64` |
| **R dependency** | 始终依赖于 `r` |
| **Installation path** | 使用 `/usr/lib/R/library/` |
| **makedepends** | 如果存在 Fortran 代码，添加 `gcc-fortran` |
| **NO manual install** | 使用 `R CMD INSTALL`（不是 `install.packages()`） |
| **Check tests** | 在构建的软件包上运行 `R CMD check` |

### R 模板（CRAN）

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

### R 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 缺少编译代码 | C/Fortran 未编译 | 检查 arch=() 是否为 x86_64，添加 gcc-fortran |
| 错误的安装位置 | 手动安装路径 | 使用 R CMD INSTALL 到 /usr/lib/R/library/ |
| 缺少依赖 | R 软件包依赖未列出 | 检查 DESCRIPTION 文件，添加到 depends |
| 软件包中的检查目录 | 构建后未移除 | 在 package() 中移除 check/ 目录 |
| 测试失败 | 缺少建议的软件包 | 添加到 checkdepends 或跳过测试 |

---

## Shell 软件包

**Shell 软件包提供 shell 脚本、函数和扩展。**

### Shell 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 使用与脚本用途相关的描述性名称 |
| **Architecture** | 始终 `any` |
| **Shebang** | 脚本必须有适当的 shebang（`#!/bin/sh` 或 `#!/bin/bash`） |
| **Dependencies** | 列出 shell 解释器（`bash`、`zsh`、`fish` 等） |
| **shellcheck** | 打包前使用 `shellcheck` 验证脚本 |
| **Permissions** | 脚本必须可执行（`install -Dm755`） |
| **Location** | `/usr/bin/` 用于可执行文件，`/usr/share/pkgname/` 用于库 |

### Shell 模板

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
    
    # 使用 shellcheck 验证
    shellcheck -x *.sh
}

package() {
    cd "$pkgname-$pkgver"
    
    # 安装主脚本
    install -Dm755 example.sh "$pkgdir/usr/bin/example"
    
    # 安装库脚本
    install -dm755 "$pkgdir/usr/share/$pkgname"
    install -Dm644 lib/*.sh "$pkgdir/usr/share/$pkgname/"
    
    # 安装文档
    install -Dm644 README.md "$pkgdir/usr/share/doc/$pkgname/README.md"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Shell 脚本安装路径

| 脚本类型 | 安装位置 |
|-------------|------------------|
| 可执行脚本 | `/usr/bin/scriptname` |
| Shell 库 | `/usr/share/pkgname/lib.sh` |
| Bash 补全 | `/usr/share/bash-completion/completions/` |
| Zsh 补全 | `/usr/share/zsh/site-functions/` |
| Fish 补全 | `/usr/share/fish/vendor_completions.d/` |
| Zsh 插件 | `/usr/share/zsh/plugins/pluginname/` |

### Shell 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 脚本不可执行 | 错误的安装权限 | 对脚本使用 install -Dm755 |
| 错误的 shebang | 解释器路径错误 | 使用 #!/bin/sh 或 #!/bin/bash |
| 未运行 shellcheck | 跳过验证 | 添加 shellcheck 到 check() 函数 |
| 缺少依赖 | 外部命令未列出 | 检查脚本中的命令使用，添加到 depends |
| 错误的安装位置 | 脚本在错误的目录 | 遵循安装路径指南 |

---

## Lisp 软件包

**Common Lisp 软件包需要 ASDF 进行系统加载和特定的目录约定。**

### Lisp 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | AUR 中使用 `cl-packagename`（例如 `cl-ppcre`） |
| **Source location** | `/usr/share/common-lisp/source/pkgname/`（使用上游名称） |
| **ASDF registry** | 到 `/usr/share/common-lisp/systems/` 中 `*.asd` 的符号链接 |
| **Avoid ASDF-Install** | 不要用于系统范围安装 |
| **Cross-platform** | 尽可能支持多个 Lisp 实现 |
| **Dependencies** | 跨平台使用 `common-lisp`，如果需要特定实现 |

### Lisp 模板

```bash
pkgname=cl-example
_lispname=example  # 上游名称
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
  
  # 安装源
  install -dm755 "$pkgdir/usr/share/common-lisp/source/${_lispname}"
  install -Dm644 *.lisp "$pkgdir/usr/share/common-lisp/source/${_lispname}/"
  install -Dm644 *.asd "$pkgdir/usr/share/common-lisp/source/${_lispname}/"
  
  # 创建 ASDF 注册表符号链接
  install -dm755 "$pkgdir/usr/share/common-lisp/systems"
  pushd "$pkgdir/usr/share/common-lisp/systems"
  ln -s ../source/${_lispname}/${_lispname}.asd .
  popd
  
  # 安装 license
  install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Lisp 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 错误的目录名称 | 在 source/ 路径中使用 AUR 名称而不是上游名称 | 在 source/ 路径中使用上游名称 |
| 损坏的符号链接 | 符号链接中的绝对路径 | 使用相对路径：../source/pkgname/file.asd |
| 找不到 ASDF | 缺少依赖 | 添加 asdf 软件包或使用内置 ASDF 的实现 |
| 编译失败 | 错误的 ASDF 设置 | 验证 central-registry 路径 |

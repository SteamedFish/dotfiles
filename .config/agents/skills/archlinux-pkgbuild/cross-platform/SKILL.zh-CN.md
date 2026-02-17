---
name: archlinux-pkgbuild/cross-platform
description: Use when packaging Windows applications under Wine, cross-compiling for Windows with MinGW, packaging Electron-based applications, or .NET/Mono applications
---

# 跨平台兼容性软件包

**有关核心 PKGBUILD 工作流程，请使用 archlinux-pkgbuild**

本子技能涵盖跨平台桥接的软件打包：通过 Wine 的 Windows 应用、通过 MinGW 的 Windows 二进制文件、通过 Electron 的 Web 应用，以及通过 Mono/CLR 的 .NET 应用。

## 快速参考

| 平台 | 使用场景 | 关键依赖 | 安装位置 | 架构 |
|----------|----------|----------------|------------------|--------------|
| Wine | Linux 上的 Windows 应用 | wine | /opt/appname/ | x86_64 |
| MinGW | 为 Windows 交叉编译 | mingw-w64-gcc | /usr/{i686,x86_64}-w64-mingw32/ | any |
| Electron | 作为桌面应用的 Web 应用 | electron* | /usr/lib/appname/ | x86_64/any |
| CLR/Mono | Linux 上的 .NET 应用 | mono | /usr/lib/appname/ | any |

---

## Wine 软件包

**Wine 软件包提供通过 Wine 兼容层运行的 Windows 软件。**

### Wine 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 使用应用名称（除非冲突，否则没有 wine- 前缀） |
| **Architecture** | `x86_64`（即使是 32位 Windows 应用 - wine 提供 multilib） |
| **Wine dependency** | 依赖于 `wine` 或 `wine-staging` |
| **Install location** | `/opt/appname/` 或 `/usr/share/appname/` |
| **Wrapper script** | 在 `/usr/bin/` 中需要以通过 wine 启动 |
| **WINEPREFIX** | 在用户主目录中使用每个应用的前缀 |
| **Desktop integration** | 提供带有正确 Exec 行的 .desktop 文件 |

### Wine 模板

```bash
pkgname=example-app
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Windows application (Wine)"
arch=('x86_64')
url="https://example.com"
license=('custom')
depends=('wine')
optdepends=('wine-gecko: HTML rendering'
            'wine-mono: .NET support')
source=("https://example.com/download/Example-$pkgver.exe")
sha256sums=('...')

package() {
    # 安装 Windows 可执行文件和文件
    install -dm755 "$pkgdir/opt/$pkgname"
    install -Dm755 Example-$pkgver.exe "$pkgdir/opt/$pkgname/example.exe"
    
    # 安装包装器脚本
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << 'EOF'
#!/bin/bash
export WINEPREFIX="$HOME/.wine-example"
export WINEARCH=win32  # 或 win64

# 如果需要，初始化前缀
if [ ! -d "$WINEPREFIX" ]; then
    wineboot -u
fi

# 启动应用
exec wine /opt/example-app/example.exe "$@"
EOF
    
    # Desktop 文件
    install -Dm644 /dev/stdin "$pkgdir/usr/share/applications/$pkgname.desktop" << EOF
[Desktop Entry]
Name=Example Application
Comment=Example Windows app via Wine
Exec=/usr/bin/$pkgname
Icon=$pkgname
Type=Application
Categories=Utility;
EOF
    
    # 安装图标
    install -Dm644 icon.png "$pkgdir/usr/share/pixmaps/$pkgname.png"
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Wine 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 共享 WINEPREFIX | 多个应用使用默认前缀 | 为每个应用使用专用 WINEPREFIX |
| 错误的架构 | 带有 win64 前缀的 32位应用 | 设置 WINEARCH=win32 |
| 缺少 DLL | 需要本机 Windows DLL | 捆绑 DLL 或使用 winetricks |
| 安装程序无法提取 | 错误的提取方法 | 使用 cabextract 或 innoextract |
| Desktop 文件无法启动 | 错误的 Exec 路径 | 使用包装器脚本的绝对路径 |
| 图标不显示 | 错误的图标格式 | 使用 wrestool/icotool 提取 .ico |

---

## MinGW 软件包

**MinGW 软件包使用 mingw-w64 工具链交叉编译软件以在 Windows 上运行。**

### MinGW 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `mingw-w64-pkgname`（静态：`mingw-w64-pkgname-static`） |
| **Architecture** | 始终 `any`（除非可执行文件在构建系统上运行） |
| **Prefixes** | `/usr/i686-w64-mingw32/` 和 `/usr/x86_64-w64-mingw32/` |
| **Dependencies** | 始终依赖于 `mingw-w64-crt` |
| **makedepends** | 始终包含 `mingw-w64-gcc` |
| **options** | 始终：`!strip`、`staticlibs`、`!buildflags` |
| **Build both** | 32位（i686）AND（和）64位（x86_64）版本 |

### MinGW 模板

```bash
pkgname=mingw-w64-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example library (mingw-w64)"
arch=('any')
url="https://example.com"
license=('GPL')
makedepends=('mingw-w64-configure')
depends=('mingw-w64-crt')
options=('!strip' '!buildflags' 'staticlibs')
source=("https://example.com/example-$pkgver.tar.gz")
sha256sums=('...')

_architectures="i686-w64-mingw32 x86_64-w64-mingw32"

build() {
  cd example-$pkgver
  for _arch in ${_architectures}; do
    mkdir -p build-${_arch} && pushd build-${_arch}
    ${_arch}-configure ..
    make
    popd
  done
}

package() {
  for _arch in ${_architectures}; do
    cd "example-$pkgver/build-${_arch}"
    make DESTDIR="${pkgdir}" install
    ${_arch}-strip --strip-unneeded "$pkgdir"/usr/${_arch}/bin/*.dll
    ${_arch}-strip -g "$pkgdir"/usr/${_arch}/lib/*.a
    rm -r "$pkgdir"/usr/${_arch}/share/{doc,man,info}
  done
}
```

### MinGW 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 文件在 /usr/local | 错误的前缀 | 设置 --prefix=/usr/${_arch} |
| 缺少 DLL | 不在 bin/ 目录中 | 安装 DLL 到 /usr/${_arch}/bin/ |
| 只有 32位或 64位 | 缺少架构 | 为 i686 和 x86_64 构建 |
| 没有静态库 | 只构建了共享库 | 同时构建共享和静态库 |

---

## Electron 软件包

**Electron 软件包将 Web 应用与 Chromium 和 Node.js 运行时捆绑在一起。** 尽可能使用系统 Electron。

### Electron 关键规则

| 规则 | 描述 |
|------|-------------|
| **使用系统 electron** | 首选 `electron` 软件包而不是捆绑的运行时 |
| **软件包命名** | 无特殊后缀（使用上游名称） |
| **electron dependency** | 添加版本化的 `electron*`（例如 `electron34`）到 `depends` |
| **Architecture** | 如果没有本机扩展，使用 `any`；如果有编译模块，使用 `x86_64` |
| **Wrapper script** | 需要使用系统 electron 启动应用 |
| **Resources location** | `/usr/lib/appname/`（架构依赖）或 `/usr/share/appname/`（any） |

### Electron 模板

```bash
pkgname=example-app
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Electron application"
arch=('x86_64')
url="https://example.com"
license=('MIT')
depends=('electron34')
makedepends=('npm' 'nodejs')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    
    # 更新 electron 版本
    npm pkg set devDependencies.electron=$(cat /usr/lib/electron34/version)
    
    # 安装依赖
    npm install
    
    # 使用 electron-builder 构建
    ./node_modules/.bin/electron-builder --linux --x64 --dir \
        -c.electronDist=/usr/lib/electron34 \
        -c.electronVersion=$(cat /usr/lib/electron34/version)
}

package() {
    cd "$pkgname-$pkgver"
    
    # 安装应用资源（不是 electron 运行时）
    install -dm755 "$pkgdir/usr/lib/$pkgname"
    cp -r dist/linux-unpacked/resources/app "$pkgdir/usr/lib/$pkgname/"
    
    # 创建包装器脚本
    install -Dm755 /dev/stdin "$pkgdir/usr/bin/$pkgname" << EOF
#!/bin/sh
exec /usr/bin/electron34 /usr/lib/$pkgname/app "\$@"
EOF
    
    # Desktop 文件
    install -Dm644 "$pkgname.desktop" "$pkgdir/usr/share/applications/$pkgname.desktop"
    
    # 图标
    install -Dm644 icon.png "$pkgdir/usr/share/pixmaps/$pkgname.png"
    
    # License
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Electron 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 捆绑的 electron | 未使用系统 electron | 使用 electron 依赖 + 包装器 |
| 错误的 electron 版本 | 版本不匹配 | 匹配 package.json 中的 electron 版本 |
| 缺少 app.asar | 错误的构建输出 | 检查 electron-builder 配置 |
| 包装器无法工作 | 应用路径错误 | 验证 /usr/lib/$pkgname/app 是否存在 |

---

## CLR 软件包（.NET/Mono）

**CLR 软件包使用 Mono 运行时作为 .NET 应用。** Mono 在 Linux 上提供 CLR 运行时。

### CLR 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 附加 `-mono` 后缀（例如 `example-mono`） |
| **arch** | 始终 `any`（Mono 尚不支持 64位编译） |
| **options** | 始终包含 `!strip` |
| **mono dependency** | 始终添加 `mono` 到 `depends` |
| **Signed assemblies** | GAC 安装需要签名的 DLL |
| **Debug database** | 将 .pdb 转换为 .mdb：`pdb2mdb Foo.dll` |
| **Executables** | 安装 shell 包装器到 `/usr/bin` |

### CLR 模板

```bash
pkgname=foo-mono
_pkgname=foo
pkgver=1.0.0
pkgrel=1
pkgdesc=".NET library for Foo"
arch=('any')
url="https://example.com"
license=('GPL')
depends=('mono')
makedepends=('mono')
options=('!strip')
source=("https://example.com/$_pkgname-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_pkgname-$pkgver"
    xbuild Foo.sln
}

package() {
    cd "$_pkgname-$pkgver/bin/Release"
    
    # 对于库：安装 DLL
    install -Dm644 Foo.dll "$pkgdir/usr/lib/$_pkgname/Foo.dll"
    
    # 对于可执行文件：安装包装器脚本
    install -dm755 "$pkgdir/usr/bin"
    cat > "$pkgdir/usr/bin/$_pkgname" << 'EOF'
#!/bin/sh
exec mono /usr/lib/$_pkgname/Foo.exe "$@"
EOF
    chmod 755 "$pkgdir/usr/bin/$_pkgname"
}
```

### CLR 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 找不到程序集 | 错误的 DLL 位置 | 安装到 /usr/lib/appname/ |
| 剥离破坏 DLL | makepkg 剥离 .NET 程序集 | 添加 !strip 到 options |
| 缺少 mono 运行时 | 不在 depends 中 | 添加 mono 到 depends |
| GAC 安装失败 | 未签名程序集 | 签名程序集或跳过 GAC |

---
name: archlinux-pkgbuild/desktop-integration
description: Use when packaging GNOME applications, KDE Plasma widgets/themes, Eclipse IDE plugins, or font packages requiring desktop environment integration
---

# 桌面集成软件包

**有关核心 PKGBUILD 工作流程，请使用 archlinux-pkgbuild**

本子技能涵盖 GNOME、KDE、Eclipse 和字体软件包的桌面特定打包要求。

## 快速参考

| 类型 | 关键工具 | 安装位置 | 钩子处理 |
|------|----------|------------------|---------------|
| GNOME | meson/arch-meson | /usr/bin, /usr/share | GSettings/icons 自动 |
| KDE | cmake + extra-cmake-modules | /usr/lib/qt/plugins | 自动发现 |
| Eclipse | N/A (Java) | /usr/lib/eclipse/dropins | 自动发现 |
| Fonts | N/A | /usr/share/fonts/$pkgname | fc-cache 自动 |

---

## 字体软件包

**字体软件包安装 TrueType、OpenType 或 Variable 字体系统范围。**

### 字体关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `ttf-fontname`（TrueType）或 `otf-fontname`（OpenType） |
| **Variable fonts suffix** | 对 variable 字体添加 `-variable` 后缀 |
| **Architecture** | 始终 `any` |
| **Dependencies** | 无（钩子处理 fontconfig） |
| **Installation path** | `/usr/share/fonts/$pkgname/` |
| **License installation** | `/usr/share/licenses/$pkgname/`（特别是 OFL） |
| **Provides** | 如果符合标准，添加 `provides=('ttf-font')` |

### 字体模板

```bash
pkgname=ttf-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example TrueType font family"
arch=('any')
url="https://example.com"
license=('OFL-1.1-RFN')
provides=('ttf-font')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

package() {
    cd "$pkgname-$pkgver"
    
    # 安装字体
    install -Dm644 -t "$pkgdir/usr/share/fonts/$pkgname" fonts/*.ttf
    
    # 安装 license（OFL 必需）
    install -Dm644 OFL.txt "$pkgdir/usr/share/licenses/$pkgname/OFL.txt"
}
```

### 字体常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 缺少 license | OFL 未安装 | 安装到 /usr/share/licenses/$pkgname/ |
| 错误的目录 | 字体在错误的位置 | 使用 /usr/share/fonts/$pkgname/ |
| 名称大写 | 软件包命名违规 | 使用全小写 |
| 字体缓存未更新 | 旧软件包使用 .install | 移除 .install（钩子处理此问题） |

---

## GNOME 软件包

**GNOME 应用遵循 GNOME 的构建约定并使用通用的 GNOME 基础设施。**

### GNOME 关键规则

| 规则 | 描述 |
|------|-------------|
| **Build system** | 大多数使用 Meson，一些旧的使用 GNU Autotools |
| **Source URL** | 发布使用 download.gnome.org，Git 使用 gitlab.gnome.org |
| **GSettings** | 无需手动干预（pacman 钩子处理更新） |
| **Icons/desktop** | 没有 .install 文件（pacman 钩子处理缓存） |
| **Metainfo validation** | 添加 appstream-glib 到 checkdepends |

### GNOME 模板（Meson）

```bash
pkgname=example-gnome-app
pkgver=45.0
pkgrel=1
pkgdesc="Example GNOME application"
arch=('x86_64')
url="https://gitlab.gnome.org/GNOME/$pkgname"
license=('GPL-2.0-or-later')
depends=('gtk4' 'libadwaita' 'glib2')
makedepends=('meson' 'git' 'appstream-glib')
checkdepends=('appstream-glib')
source=("https://download.gnome.org/sources/$pkgname/${pkgver%.*}/$pkgname-$pkgver.tar.xz")
sha256sums=('...')

build() {
    arch-meson "$pkgname-$pkgver" build
    meson compile -C build
}

check() {
    meson test -C build --print-errorlogs
}

package() {
    meson install -C build --destdir "$pkgdir"
}
```

### GNOME 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 手动 schema 编译 | 钩子时代之前的旧习惯 | 从 .install 中移除 |
| .install 中的图标缓存 | 使用钩子时不需要 | 从 .install 中移除 |
| .install 中的桌面数据库 | 使用钩子时不需要 | 从 .install 中移除 |
| 错误的源 URL | 旧的 git.gnome.org | 使用 gitlab.gnome.org |

---

## KDE 软件包

**KDE 软件包使用 CMake 并遵循 Plasma 小部件、主题和应用的特定约定。**

### KDE 关键规则

| 规则 | 描述 |
|------|-------------|
| **Build directory** | 需要源码外构建（`mkdir -p build`） |
| **Install prefix** | 始终 `-DCMAKE_INSTALL_PREFIX=/usr` |
| **Build type** | 通常省略（遵循 CFLAGS/CPPFLAGS） |
| **Plasma widgets** | 名称：`plasma6-applets-widgetname` |
| **Plasma runners** | 名称：`plasma6-runners-runnername` |
| **Service menus** | 名称：`kf6-servicemenus-servicename` |
| **Themes** | 名称：`plasma6-themes-themename` |

### KDE 模板

```bash
pkgname=plasma6-applets-example
pkgver=1.0.0
pkgrel=1
pkgdesc="Example Plasma 6 widget"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('plasma-workspace')
makedepends=('extra-cmake-modules' 'plasma-framework')
source=("https://example.com/$pkgname-$pkgver.tar.gz")
sha256sums=('...')

prepare() {
    mkdir -p build
}

build() {
    cd build
    cmake ../$pkgname-$pkgver \
        -DCMAKE_INSTALL_PREFIX=/usr
    make
}

package() {
    cd build
    make DESTDIR="$pkgdir" install
}
```

### KDE 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 错误的命名约定 | 与 Arch 标准不一致 | 使用 plasma6-*、kf6-* 前缀 |
| 源码内构建 | 源中的构建产物 | 使用源码外构建目录 |
| 错误的 CMake 前缀 | 文件在错误的位置 | 设置 CMAKE_INSTALL_PREFIX=/usr |
| 缺少 extra-cmake-modules | KDE 宏不可用 | 添加到 makedepends |

---

## Eclipse 软件包

**Eclipse 软件包为 Eclipse IDE 提供插件和功能。**

### Eclipse 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `eclipse-pluginname`（使用插件的通用名称，不是内部 ID） |
| **Architecture** | 通常 `any`（纯 Java），如果包含本机库，使用 `x86_64` |
| **Dependencies** | 必须依赖于 `eclipse` 或特定的 Eclipse 基础 |
| **Install location** | `/usr/lib/eclipse/dropins/pluginname/` |
| **No manual registration** | Eclipse 自动发现 dropins/ 中的插件 |
| **Feature-based** | 安装完整功能，不是单个 JAR |

### Eclipse 模板

```bash
pkgname=eclipse-example
_pkgname=com.example.eclipse.plugin
pkgver=1.0.0
pkgrel=1
pkgdesc="Example plugin for Eclipse IDE"
arch=('any')
url="https://example.com"
license=('EPL')
depends=('eclipse')
source=("https://example.com/eclipse-plugin-$pkgver.zip")
sha256sums=('...')

package() {
    cd "$srcdir"
    
    # 安装到 dropins 目录
    install -dm755 "$pkgdir/usr/lib/eclipse/dropins/$pkgname"
    
    # 复制功能和插件
    if [ -d features ]; then
        cp -r features "$pkgdir/usr/lib/eclipse/dropins/$pkgname/"
    fi
    
    if [ -d plugins ]; then
        cp -r plugins "$pkgdir/usr/lib/eclipse/dropins/$pkgname/"
    fi
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Eclipse 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 插件未检测到 | 错误的安装位置 | 使用 /usr/lib/eclipse/dropins/ |
| 尝试手动注册 | 旧 Eclipse 版本 | 移除注册（自动发现有效） |
| 缺少 features/ 目录 | 仅安装了 plugins/ | 安装完整的功能结构 |
| 使用内部 ID | 软件包命名错误 | 使用通用名称，不是 com.example.id |

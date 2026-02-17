---
name: archlinux-pkgbuild/system-packages
description: Use when packaging DKMS kernel modules, out-of-tree kernel modules, 32-bit multilib libraries, proprietary/nonfree software, web applications, or creating split packages
---

# 系统软件包

**系统软件包处理专门的系统级组件：内核模块（DKMS 和传统）、multilib 32位库、专有/非自由软件、Web 应用和分割软件包。**

## 何时使用本技能

在以下情况下使用：
- 打包内核模块（DKMS 或传统）
- 为 multilib 创建 32位库（lib32-*）
- 打包具有许可限制的专有/非自由软件
- 使用适当的配置分离安装 Web 应用
- 创建分割软件包（一个源多个软件包）

触发短语：
- "kernel module", "DKMS", "dkms.conf"
- "lib32-", "32-bit", "multilib"
- "proprietary", "nonfree", "EULA", "/opt"
- "web app", "webapp", "http user"
- "split package", "pkgbase"

## 快速参考

| 软件包类型 | 关键模式 | 关键文件/路径 |
|--------------|-------------|----------------------|
| **DKMS** | `example-dkms`, depends=(dkms) | /usr/src/$_pkgbase-$pkgver/dkms.conf |
| **内核模块** | 每个内核软件包，`-utils` 分离 | /usr/lib/modules/extramodules-*/module.ko |
| **lib32** | `lib32-pkgname`, arch=('x86_64') | --libdir=/usr/lib32, CC='gcc -m32' |
| **非自由软件** | license=('custom'), options=('!strip') | /opt/appname/, /usr/share/licenses/ |
| **Web 应用** | arch=('any'), http:http 所有权 | /usr/share/webapps/, /etc/webapps/, /var/lib/ |
| **分割** | pkgbase + 多个 package_name() | 共享 build()，单独的 package_*() |

---

## 分割软件包

**对于来自一个源的多个软件包：**

```bash
pkgbase=example-base
pkgname=('example-cli' 'example-gui')
pkgver=1.0.0
pkgrel=1

build() {
    cd "$srcdir/$pkgbase-$pkgver"
    make all
}

package_example-cli() {
    pkgdesc="CLI tool"
    depends=('lib1')
    
    cd "$srcdir/$pkgbase-$pkgver"
    make DESTDIR="$pkgdir" install-cli
}

package_example-gui() {
    pkgdesc="GUI application"
    depends=('lib1' 'gtk3')
    
    cd "$srcdir/$pkgbase-$pkgver"
    make DESTDIR="$pkgdir" install-gui
}
```

**关键点：**
- `pkgbase=` 定义基本名称，`pkgname=()` 数组列出所有软件包
- 单个 `build()`，每个软件包单独的 `package_name()`
- 每个软件包有自己的 `pkgdesc`、`depends`、`optdepends` 等
- 常见模式：工具软件包 + 模块/插件软件包

---

## DKMS 软件包（内核模块）

**DKMS 软件包提供在内核更新时自动重建的内核模块。**

### DKMS 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 附加 `-dkms` 后缀（例如 `example-dkms`） |
| **dkms dependency** | 始终添加 `dkms` 到 `depends` |
| **NO linux-headers** | 不要添加到 depends（dkms 处理此问题） |
| **Source location** | 安装到 `/usr/src/PACKAGE_NAME-PACKAGE_VERSION/` |
| **dkms.conf** | 需要配置文件 |
| **NO manual dkms calls** | Pacman 钩子处理 dkms add/build/install/remove |
| **NO autoload in .install** | 让用户手动加载模块 |

### DKMS 模板

```bash
_pkgbase=example
pkgname=example-dkms
pkgver=1.0.0
pkgrel=1
pkgdesc="Example kernel module (DKMS)"
arch=('x86_64')
url="https://example.com"
license=('GPL2')
depends=('dkms')
conflicts=("$_pkgbase")
install=${pkgname}.install
source=("https://example.com/tarball.tar.gz"
        'dkms.conf'
        "${pkgname}.conf")
sha256sums=('...')

package() {
    # 安装 dkms.conf
    install -Dm644 dkms.conf "$pkgdir/usr/src/$_pkgbase-$pkgver/dkms.conf"
    
    # 在 dkms.conf 中设置名称和版本
    sed -e "s/@_PKGBASE@/$_pkgbase/" \
        -e "s/@PKGVER@/$pkgver/" \
        -i "$pkgdir/usr/src/$_pkgbase-$pkgver/dkms.conf"
    
    # 复制源（包括 Makefile）
    cp -r "$_pkgbase"/* "$pkgdir/usr/src/$_pkgbase-$pkgver/"
    
    # 将冲突模块列入黑名单
    install -Dm644 ${pkgname}.conf "$pkgdir/usr/lib/modprobe.d/${pkgname}.conf"
}
```

**dkms.conf 示例：**
```ini
PACKAGE_NAME="@_PKGBASE@"
PACKAGE_VERSION="@PKGVER@"
BUILT_MODULE_NAME[0]="example"
DEST_MODULE_LOCATION[0]="/kernel/drivers/misc"
AUTOINSTALL="yes"
MAKE[0]="make KERNELRELEASE=${kernelver}"
CLEAN="make clean"
```

---

## 内核模块软件包

**内核模块软件包必须支持多个内核安装并将模块与工具分离。**

### 内核模块关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包分离** | `pkgname` 中的模块，`pkgname-utils` 中的工具 |
| **Dependencies** | 模块软件包依赖于特定内核 + utils 软件包 |
| **Provides** | 替代内核模块提供基本软件包名称 |
| **File placement** | 在 `/lib/modules/X.Y.Z-arch1-1/updates/` 中覆盖模块 |
| **Multiple kernels** | 每个内核需要单独的模块软件包 |

### 内核模块软件包分离

**强制分离模式：**

| 软件包 | 包含 | 依赖 |
|---------|----------|--------------|
| `nvidia` | linux 内核的模块 | `linux`, `nvidia-utils` |
| `nvidia-utils` | 支持文件、工具 | （基本依赖） |
| `nvidia-lts` | linux-lts 内核的模块 | `linux-lts`, `nvidia-utils` |

### 内核模块模板

```bash
pkgname=nvidia
pkgver=550.54.14
pkgrel=1
pkgdesc="NVIDIA drivers for linux kernel"
arch=('x86_64')
url="https://www.nvidia.com"
license=('custom')
depends=('linux' 'nvidia-utils')
makedepends=('linux-headers')
source=("https://example.com/nvidia-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$pkgname-$pkgver"
    
    # 获取内核版本
    _kernver=$(cat /usr/lib/modules/extramodules-*/version)
    
    # 构建模块
    make SYSSRC=/usr/lib/modules/$_kernver/build module
}

package() {
    cd "$pkgname-$pkgver"
    
    # 获取内核版本
    _kernver=$(cat /usr/lib/modules/extramodules-*/version)
    
    # 安装内核模块
    install -Dm644 nvidia.ko \
        "$pkgdir/usr/lib/modules/$_kernver/extramodules/nvidia.ko"
}
```

---

## lib32 软件包（32位）

**lib32 软件包为 multilib 支持在 x86_64 系统上提供 32位库。**

### lib32 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | `lib32-pkgname`（匹配 64位软件包名称） |
| **Architecture** | 始终 `x86_64`（multilib 仅 x86_64） |
| **Dependencies** | 通常依赖于 64位版本以获取文档/二进制文件 |
| **Library location** | `/usr/lib32/`（NOT（不是）/usr/lib/） |
| **Compiler flags** | 使用 `-m32` 和 `lib32-gcc-libs` |
| **Provides/conflicts** | 与 64位版本无 provides/conflicts |

### lib32 模板

```bash
_pkgbasename=example
pkgname=lib32-$_pkgbasename
pkgver=1.0.0
pkgrel=1
pkgdesc="Example library (32-bit)"
arch=('x86_64')
url="https://example.com"
license=('GPL')
depends=('lib32-glibc' "$_pkgbasename")
makedepends=('gcc-multilib')
source=("https://example.com/$_pkgbasename-$pkgver.tar.gz")
sha256sums=('...')

build() {
    cd "$_pkgbasename-$pkgver"
    
    export CC='gcc -m32'
    export CXX='g++ -m32'
    export PKG_CONFIG_PATH='/usr/lib32/pkgconfig'
    
    ./configure \
        --prefix=/usr \
        --libdir=/usr/lib32 \
        --disable-static
    make
}

package() {
    cd "$_pkgbasename-$pkgver"
    make DESTDIR="$pkgdir" install
    
    # 移除除库以外的所有内容
    rm -rf "$pkgdir"/usr/{bin,include,share}
}
```

### lib32 常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| /usr/lib 中的库 | 错误的 libdir | 设置 --libdir=/usr/lib32 |
| 缺少 -m32 标志 | 没有 multilib 编译 | 导出 CC='gcc -m32' |
| 安装的头文件 | 未在 package() 中移除 | rm -rf "$pkgdir"/usr/include |
| 错误的架构 | 设置为 'any' 或 'i686' | 始终使用 arch=('x86_64') |

---

## 非自由软件包

**非自由软件包分发具有许可限制的专有或非自由软件。** 需要特殊处理以进行许可、分发和用户协议。

### 非自由关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 仅当与自由版本冲突时附加 `-nonfree` 后缀 |
| **License handling** | 使用 `custom` license + 安装 EULA/license 文件 |
| **Source restrictions** | 可能需要用户单独下载（无法重新分发） |
| **EULA acceptance** | 在 `.install` 文件中包含 post_install 消息 |
| **NO AUR submission** | AUR 中不鼓励专有软件（视情况而定） |
| **Architecture** | 通常仅 `x86_64`（供应商很少提供多架构） |
| **Binary distribution** | 如果不是从源代码构建，使用 `-bin` 后缀 |

### 非自由许可处理

**专有软件的 License 字段：**

```bash
license=('custom')  # 或 'custom:VendorName'
```

**强制：安装 license/EULA：**

```bash
package() {
    # ... 安装文件 ...
    
    # 安装 EULA
    install -Dm644 EULA.txt "$pkgdir/usr/share/licenses/$pkgname/EULA"
    
    # 或专有 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### 非自由模板（可再分发二进制文件）

```bash
pkgname=example-proprietary
pkgver=1.0.0
pkgrel=1
pkgdesc="Example proprietary application"
arch=('x86_64')
url="https://example.com"
license=('custom')
depends=('glibc')
options=('!strip')  # 不要剥离专有二进制文件
source=("https://example.com/downloads/example-$pkgver-linux-x86_64.tar.gz")
sha256sums=('...')

package() {
    cd "$srcdir/example-$pkgver"
    
    # 安装到 /opt 用于专有软件
    install -dm755 "$pkgdir/opt/$pkgname"
    cp -r * "$pkgdir/opt/$pkgname/"
    
    # 在 /usr/bin 中创建包装器
    install -dm755 "$pkgdir/usr/bin"
    ln -s "/opt/$pkgname/example" "$pkgdir/usr/bin/example"
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### 非自由模板（需要手动下载）

```bash
pkgname=example-restricted
pkgver=1.0.0
pkgrel=1
pkgdesc="Example software with download restrictions"
arch=('x86_64')
url="https://example.com"
license=('custom:Example')
depends=('glibc')
# NO source= - 用户必须下载
DLAGENTS=('http::/usr/bin/curl -fLC - --retry 3 --retry-delay 3 -o %o %u')

# PKGBUILD 应该出错并提供下载说明
if [ ! -f "$startdir/example-$pkgver.tar.gz" ]; then
    error "请手动下载 example-$pkgver.tar.gz 从："
    error "  https://example.com/download"
    error "将文件放在与此 PKGBUILD 相同的目录中。"
    exit 1
fi

source=("file://example-$pkgver.tar.gz")
sha256sums=('...')  # 手动下载的校验和

package() {
    cd "$srcdir/example-$pkgver"
    
    # 安装应用
    install -dm755 "$pkgdir/opt/$pkgname"
    cp -r * "$pkgdir/opt/$pkgname/"
    
    # 安装 license
    install -Dm644 EULA.txt "$pkgdir/usr/share/licenses/$pkgname/EULA"
}
```

### 非自由 .install 文件（EULA）

**显示 EULA 并要求接受：**

```bash
post_install() {
    cat << EOF

==> 需要接受 EULA
    
    通过安装 $pkgname，您同意最终用户许可协议。
    License 文本：/usr/share/licenses/$pkgname/EULA
    
    关键限制：
    - 仅非商业用途
    - 禁止重新分发
    - 禁止逆向工程
    
    如果您不同意，请移除此软件包：pacman -R $pkgname

EOF
}

post_upgrade() {
    post_install
}
```

### 非自由软件位置

**安装路径指南：**

| 软件类型 | 安装位置 | 理由 |
|---------------|------------------|-----------|
| 自包含专有应用 | `/opt/appname/` | FHS：/opt 用于"附加软件包" |
| 专有库 | `/usr/lib/`（谨慎） | 仅当设计为系统范围使用时 |
| 专有二进制文件 | `/usr/bin/`（到 /opt 的符号链接） | 保持 PATH 便利 |
| 专有游戏 | `/opt/gamename/` | 保持游戏资源自包含 |

### 非自由常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 剥离破坏二进制文件 | makepkg 剥离专有代码 | 添加 `!strip` 到 options |
| 缺少库 | 专有二进制文件需要特定的 .so 版本 | 在 /opt 中捆绑库或使用 lib32 依赖 |
| 未显示 EULA | 没有 .install 文件 | 使用 post_install 创建 .install |
| 许可违规 | 重新分发不可重新分发的文件 | 使用手动下载方法 |
| 源不可用 | 仅二进制分发 | 在软件包名称中使用 `-bin` 后缀 |
| 错误的位置 | 安装到 /usr | 对自包含专有应用使用 /opt |

---

## Web 应用软件包

**Web 应用软件包使用适当的配置和安全性安装服务器端 Web 应用。**

### Web 关键规则

| 规则 | 描述 |
|------|-------------|
| **软件包命名** | 使用应用名称（无 `webapp-` 前缀） |
| **Architecture** | 通常 `any`（解释型语言） |
| **Install location** | `/usr/share/webapps/appname/` |
| **Configuration** | `/etc/webapps/appname/`（与代码分离） |
| **Web server config** | 为 Apache/Nginx 提供示例配置 |
| **Permissions** | 对可写目录谨慎（使用 http 用户） |
| **Dependencies** | 将 Web 服务器作为 optdepends（不是 depends） |

### Web 应用目录结构

| 内容类型 | 安装位置 |
|--------------|------------------|
| 应用代码 | `/usr/share/webapps/appname/` |
| 配置文件 | `/etc/webapps/appname/` |
| 可写数据 | `/var/lib/appname/`（使用 tmpfiles.d） |
| 日志 | `/var/log/appname/`（使用 tmpfiles.d） |
| 缓存 | `/var/cache/appname/`（使用 tmpfiles.d） |
| Web 服务器配置 | `/etc/webapps/appname/apache.example.conf` |
| tmpfiles.d 配置 | `/usr/lib/tmpfiles.d/appname.conf` |

### Web 应用模板

```bash
pkgname=example-webapp
pkgver=1.0.0
pkgrel=1
pkgdesc="Example web application"
arch=('any')
url="https://example.com"
license=('GPL')
depends=('php' 'php-sqlite')
optdepends=(
    'apache: Web server'
    'nginx: Web server'
    'mysql: Database backend'
)
backup=('etc/webapps/example/config.php')
install=$pkgname.install
source=("https://example.com/releases/$pkgname-$pkgver.tar.gz"
        'apache.example.conf'
        'nginx.example.conf'
        "$pkgname.tmpfiles")
sha256sums=('...')

package() {
    cd "$pkgname-$pkgver"
    
    # 安装应用
    install -dm755 "$pkgdir/usr/share/webapps/$pkgname"
    cp -r * "$pkgdir/usr/share/webapps/$pkgname/"
    
    # 从应用中移除可写目录（将由 tmpfiles.d 创建）
    rm -rf "$pkgdir/usr/share/webapps/$pkgname"/{uploads,cache,logs,data}
    
    # 将配置移动到 /etc
    install -dm755 "$pkgdir/etc/webapps/$pkgname"
    mv "$pkgdir/usr/share/webapps/$pkgname/config.php" \
        "$pkgdir/etc/webapps/$pkgname/"
    ln -s "/etc/webapps/$pkgname/config.php" \
        "$pkgdir/usr/share/webapps/$pkgname/config.php"
    
    # 创建到 /var/lib 的符号链接（实际目录由 tmpfiles.d 在运行时创建）
    ln -s /var/lib/$pkgname/uploads "$pkgdir/usr/share/webapps/$pkgname/uploads"
    ln -s /var/lib/$pkgname/cache "$pkgdir/usr/share/webapps/$pkgname/cache"
    
    # 安装 systemd-tmpfiles.d 配置
    install -Dm644 "$srcdir/$pkgname.tmpfiles" \
        "$pkgdir/usr/lib/tmpfiles.d/$pkgname.conf"
    
    # 安装示例 Web 服务器配置
    install -Dm644 "$srcdir/apache.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/apache.example.conf"
    install -Dm644 "$srcdir/nginx.example.conf" \
        "$pkgdir/etc/webapps/$pkgname/nginx.example.conf"
    
    # 安装 license
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

### Web 应用 systemd-tmpfiles.d

**对于 /var 中的可写目录强制：** 使用 systemd-tmpfiles.d 而不是在 package() 中创建目录。

**为什么使用 tmpfiles.d：**
- 用户可配置的清理策略（Age 字段）
- 正确的所有权处理（User/Group 字段）
- 运行时目录创建（在系统更新后保留）
- 标准 Arch Linux 实践

**tmpfiles.d 模板（appname.tmpfiles）：**
```conf
# appname 的 systemd-tmpfiles 配置
# 详情请参阅 tmpfiles.d(5)

# Type Path                    Mode User  Group Age Argument
d     /var/lib/appname          0755 http  http  -   -
d     /var/lib/appname/uploads  0755 http  http  -   -
d     /var/lib/appname/cache    0755 http  http  30d -
d     /var/log/appname          0755 http  http  -   -
```

**字段说明：**
- `Type`：`d` = 如果不存在则创建目录，`z` = 在现有路径上调整所有权/权限
- `Path`：目录/文件的绝对路径
- `Mode`：权限（0755、0700 等）
- `User/Group`：所有权（Web 应用使用 http）
- `Age`：清理策略（`-` = 从不，`30d` = 30 天，`7d` = 7 天）
- `Argument`：额外参数（通常 `-`）

**对于敏感配置文件（凭据、机密）：**
```conf
# 如果应用写入配置（例如，Web 安装程序）：
# 文件是 root:http 0660（root 拥有，http 组可以写入）
z     /etc/webapps/appname/config.php      0660 root  http  -   -
z     /etc/webapps/appname/database.php    0660 root  http  -   -  # 包含数据库密码！

# 类型 'z' 调整软件包安装的文件的所有权/权限
# root:http 0660 = root 拥有文件（安全），http 组可以写入，NOT（非）全局可读
# 配置目录 /etc/webapps/appname 保持 root:root（安装的默认值）
```

**为什么对敏感配置使用类型 'z' 和 root:http：**
1. 软件包将文件安装为 root:root 0644（正常）
2. tmpfiles.d 将组更改为 http，权限为 0660
3. Root 拥有文件（更安全 - 只有 root 可以删除/重命名）
4. http 组可以读/写（应用可以修改配置）
5. 文件 NOT（非）全局可读（0660）- 保护凭据

**请参阅 @config-file-handling.zh-CN.md 以获取详细的安全模式。**

**PKGBUILD 集成：**
```bash
source=('...' "$pkgname.tmpfiles")
sha256sums=('...')

package() {
    # 安装 tmpfiles.d 配置
    install -Dm644 "$srcdir/$pkgname.tmpfiles" \
        "$pkgdir/usr/lib/tmpfiles.d/$pkgname.conf"
    
    # DO NOT（不要）使用 install -dm755 创建目录
    # 目录由 systemd-tmpfiles 在运行时创建
    
    # 创建到 /var 目录的符号链接（可选，如果应用需要它们）
    ln -s /var/lib/$pkgname/uploads "$pkgdir/usr/share/webapps/$pkgname/uploads"
}
```

**.install 文件：**
```bash
post_install() {
    cat <<EOF
==> 配置和设置说明...
==> 
==> 可写目录在 /var/lib/$pkgname/ 中自动创建
==> 通过 systemd-tmpfiles.d 在软件包安装期间。
EOF
}

post_upgrade() {
    # 仅显示与升级相关的信息
    # 数据库设置是一次性的，升级时不需要
    cat <<EOF

==> Web 应用升级说明
==> 

1. 配置审查：
   - 检查 .pacnew 文件：/etc/webapps/$pkgname/
   - 审查新配置选项

2. 数据库迁移（如适用）：
   - 检查 /usr/share/webapps/$pkgname/updates/ 中的迁移脚本
   - 通过应用更新界面运行数据库更新

3. 服务重启：
   - 重启 Web 服务器：systemctl restart httpd（或 nginx + php-fpm）

4. 目录：
   - 可写目录在 /var/lib/$pkgname/ 自动维护

变更日志：https://upstream.project/releases
EOF
}
```

**重要：** 
- **DO NOT（不要）在 .install 文件中手动调用 `systemd-tmpfiles`**
- Pacman 通过钩子在安装/升级期间自动调用 systemd-tmpfiles
- .install 文件应该 **ONLY（仅）打印信息**，不执行操作
- 软件包安装时自动创建目录

**常见 tmpfiles.d 模式：**

| 目录类型 | 模式 | 用户:组 | Age | 理由 |
|----------------|------|------------|-----|-----------|
| 上传/数据 | 0755 | http:http | `-` | 持久数据，从不自动清理 |
| 缓存 | 0755 | http:http | `30d` | 自动清理旧缓存文件 |
| 会话 | 0700 | http:http | `7d` | 每周清理旧会话 |
| 日志 | 0755 | http:http | `-` | 保留日志（改用 logrotate） |
| 临时文件 | 1777 | root:root | `1d` | 共享临时，每天清理 |

### Web 应用安全

**配置分离：**
- 代码：`/usr/share/webapps/`（root:root，644/755）
- 配置：`/etc/webapps/`（root:root，640/750 或 http:http 用于可写）
- 数据：`/var/lib/appname/`（http:http，755/700）- **由 tmpfiles.d 创建**

**PHP open_basedir：**
```apache
php_admin_value open_basedir "/tmp:/usr/share/webapps/example:/etc/webapps/example:/var/lib/example"
```

### Web 应用常见问题

| 问题 | 原因 | 修复 |
|-------|-------|-----|
| 可写的 /usr/share | 错误的权限 | 将可写目录移动到 /var/lib/，使用 tmpfiles.d |
| /usr/share 中的配置 | 未分离 | 移动到 /etc/webapps/，符号链接回来 |
| 需要 Web 服务器 | 在 depends 中 | 移动到 optdepends |
| 没有示例配置 | 未提供 | 创建 apache/nginx 示例配置 |
| 错误的所有权 | 在 package() 中使用 install -o/-g | 对 User/Group 字段使用 tmpfiles.d |
| 目录未创建 | 缺少 tmpfiles.d | 创建 .tmpfiles 文件（pacman 钩子自动调用 systemd-tmpfiles） |
| 指向不存在的 /var 的符号链接 | 运行时创建的目录 | 正常 - tmpfiles.d 在安装时创建它们 |
| open_basedir 错误 | 路径未列入白名单 | 将所有需要的路径添加到 open_basedir |

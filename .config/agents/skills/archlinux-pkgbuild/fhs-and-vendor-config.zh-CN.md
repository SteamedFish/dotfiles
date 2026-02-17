# FHS 合规性和系统软件包位置

**这是 Arch Linux 软件包中正确安装路径的详细参考。**

## 标准 FHS 路径

**正确的安装路径：**

| 内容 | 正确路径 | 错误路径 |
|---------|-------------|------------|
| 二进制文件 | /usr/bin | /usr/local/bin, /bin |
| 库 | /usr/lib | /usr/local/lib, /lib |
| 头文件 | /usr/include | /usr/local/include |
| 应用模块 | /usr/lib/$pkgname | /usr/libexec |
| 文档 | /usr/share/doc/$pkgname | /usr/doc |
| License | /usr/share/licenses/$pkgname | /usr/share/doc |
| Man pages | /usr/share/man | /usr/man |
| 应用数据 | /usr/share/$pkgname | /usr/local/share |
| Desktop 条目 | /usr/share/applications | /usr/local/share/applications |
| 图标 | /usr/share/icons 或 /usr/share/pixmaps | /usr/local/share/icons |
| 配置 | /etc | /usr/etc |
| 状态数据 | /var/lib/$pkgname | /var/$pkgname |

## 系统软件包位置（供应商配置）

**关键规则：供应商提供的配置 ALWAYS（始终）放到 `/usr/lib/`，NOT（不要放到） `/etc/`。**

| 配置类型 | 系统软件包位置（正确） | 用户覆盖位置 | 错误路径 |
|-------------|-----------------------------------|------------------------|------------|
| systemd 用户定义 | `/usr/lib/sysusers.d/$pkgname.conf` | `/etc/sysusers.d/` | `/etc/sysusers.d/$pkgname.conf` |
| systemd tmpfiles 配置 | `/usr/lib/tmpfiles.d/$pkgname.conf` | `/etc/tmpfiles.d/` | `/etc/tmpfiles.d/$pkgname.conf` |
| udev 规则 | `/usr/lib/udev/rules.d/$pkgname.rules` | `/etc/udev/rules.d/` | `/etc/udev/rules.d/$pkgname.rules` |
| systemd 服务 | `/usr/lib/systemd/system/$pkgname.service` | `/etc/systemd/system/` | `/etc/systemd/system/$pkgname.service` |
| systemd 网络 | `/usr/lib/systemd/network/$pkgname.network` | `/etc/systemd/network/` | `/etc/systemd/network/$pkgname.network` |
| modprobe.d | `/usr/lib/modprobe.d/$pkgname.conf` | `/etc/modprobe.d/` | `/etc/modprobe.d/$pkgname.conf` |
| modules-load.d | `/usr/lib/modules-load.d/$pkgname.conf` | `/etc/modules-load.d/` | `/etc/modules-load.d/$pkgname.conf` |
| PAM 配置 | `/usr/lib/pam.d/$pkgname` | `/etc/pam.d/` | `/etc/pam.d/$pkgname`（有时有效） |
| environment.d | `/usr/lib/environment.d/$pkgname.conf` | `/etc/environment.d/` | `/etc/environment.d/$pkgname.conf` |

### 为什么这很重要

1. **关注点分离**：`/etc/` **专门用于用户修改**，`/usr/lib/` 用于**软件包提供的默认值**
2. **更新语义**：系统更新 NEVER（绝不）触及 `/etc/`（保护用户配置），但可以覆盖 `/usr/lib/`
3. **冲突预防**：两个软件包在 `/usr/lib/` 中的相同路径安装是有效的（systemd 合并），但在 `/etc/` 中会导致 pacman 冲突
4. **搜索顺序**：systemd/udev 首先搜索 `/etc/`（用户覆盖），然后 `/usr/lib/`（软件包默认值）

### 系统位置的文件命名约定

**ALWAYS（始终）使用软件包名称作为基本名称**以防止冲突：

```bash
# ✅ 正确：软件包名称作为文件名
install -Dm644 "$srcdir/myapp.sysusers" "$pkgdir/usr/lib/sysusers.d/myapp.conf"
install -Dm644 "$srcdir/myapp.tmpfiles" "$pkgdir/usr/lib/tmpfiles.d/myapp.conf"
install -Dm644 "$srcdir/myapp.rules" "$pkgdir/usr/lib/udev/rules.d/99-myapp.rules"

# ❌ 错误：通用或不一致的名称
install -Dm644 "$srcdir/myapp.sysusers" "$pkgdir/usr/lib/sysusers.d/users.conf"  # 冲突！
install -Dm644 "$srcdir/myapp.tmpfiles" "$pkgdir/usr/lib/tmpfiles.d/cleanup.conf"  # 所有权不明确
install -Dm644 "$srcdir/myapp.rules" "$pkgdir/usr/lib/udev/rules.d/device.rules"  # 哪个软件包？
```

**如果需要多个相关文件，请包含软件包前缀：**

```bash
# ✅ 正确：带有软件包前缀的多个配置
/usr/lib/tmpfiles.d/myapp-runtime.conf
/usr/lib/tmpfiles.d/myapp-cache.conf
/usr/lib/sysusers.d/myapp-primary.conf
/usr/lib/sysusers.d/myapp-secondary.conf

# ❌ 错误：没有软件包标识
/usr/lib/tmpfiles.d/runtime.conf  # 什么软件包？
/usr/lib/tmpfiles.d/cache.conf    # 冲突风险
```

## 在 prepare() 中修复路径

许多上游项目安装到错误的路径。在 prepare() 中修复：

```bash
prepare() {
    cd "$srcdir/$pkgname-$pkgver"
    
    # 修复 systemd 单元安装路径
    sed -i 's|/etc/systemd/system|/usr/lib/systemd/system|g' Makefile
    
    # 修复 udev 规则路径
    sed -i 's|/etc/udev/rules.d|/usr/lib/udev/rules.d|g' setup.py
    
    # 修复二进制路径
    sed -i 's|/usr/local/bin|/usr/bin|g' CMakeLists.txt
    
    # 修复库路径
    sed -i 's|/usr/local/lib|/usr/lib|g' configure
}
```

## 按构建系统划分的常见路径修复

### Makefile 项目

```bash
# 在安装期间覆盖路径
make install DESTDIR="$pkgdir" \
  PREFIX=/usr \
  BINDIR=/usr/bin \
  LIBDIR=/usr/lib \
  MANDIR=/usr/share/man

# 在 prepare() 中修复路径
sed -i 's|/usr/local|/usr|g' Makefile
sed -i 's|/etc/systemd|/usr/lib/systemd|g' Makefile
```

### CMake 项目

```bash
cmake -B build -S . \
  -DCMAKE_INSTALL_PREFIX=/usr \
  -DCMAKE_INSTALL_BINDIR=/usr/bin \
  -DCMAKE_INSTALL_LIBDIR=/usr/lib \
  -DCMAKE_INSTALL_SYSCONFDIR=/etc \
  -DCMAKE_INSTALL_DATADIR=/usr/share
```

### Python 项目

```bash
# setuptools 通常正确安装到 /usr
python -m build --wheel --no-isolation
python -m installer --destdir="$pkgdir" dist/*.whl

# 如果需要，覆盖路径
python setup.py install \
  --root="$pkgdir" \
  --prefix=/usr \
  --optimize=1
```

### Meson 项目

```bash
arch-meson . build \
  --prefix=/usr \
  --bindir=/usr/bin \
  --libdir=/usr/lib \
  --sysconfdir=/etc
```

## 验证正确路径

构建后，检查已安装的路径：

```bash
# 列出所有已安装的文件
tar -tzf package.pkg.tar.zst

# 检查错误路径
tar -tzf package.pkg.tar.zst | grep -E '(^usr/local|^bin/|^lib/)'

# 检查 systemd/udev 路径
tar -tzf package.pkg.tar.zst | grep -E 'etc/(systemd|udev|tmpfiles|sysusers|modprobe)'
```

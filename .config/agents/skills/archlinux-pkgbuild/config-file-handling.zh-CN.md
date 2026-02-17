# Arch Linux 软件包中的配置文件处理

**用户可修改的配置文件 MUST（必须）在 backup=() 数组中列出，以防止 pacman 覆盖它们。**

## backup=() 数组规则

**列出 /etc 中用户可能修改的 ALL（所有）文件：**

```bash
# 在 PKGBUILD 中
backup=(
    'etc/myapp/main.conf'
    'etc/myapp/database.conf'
    'etc/myapp/logging.conf'
)
```

**重要：backup=() 中的路径相对于根目录，WITHOUT（无）前导斜杠。**

✅ 正确：`'etc/myapp/main.conf'`  
❌ 错误：`'/etc/myapp/main.conf'`

## 使用 backup=() 的 Pacman 行为

| 场景 | Pacman 行为 | 用户影响 |
|----------|----------------|-------------|
| 用户未修改文件 | 静默替换为新版本 | 配置自动更新 |
| 用户修改了文件 | 创建带有新版本的 .pacnew | 用户手动合并更改 |
| 在移除时文件在 backup=() 中 | 创建 .pacsave 备份 | 卸载后保留用户配置 |

**没有 backup=()：** Pacman 在每次升级时覆盖用户修改。数据库凭据、自定义设置，全部丢失。

## 在 backup=() 中包含什么

**包含：**
- 用户自定义的配置文件（数据库设置、应用配置）
- 包含凭据或机密的文件
- /etc 中具有站点特定设置的文件

**排除：**
- 只读模板（/usr/share/...）
- 生成的文件（.cache、.pid）
- 应 ALWAYS（始终）更新的文件（服务文件）

## 敏感配置文件（凭据、机密）

**当配置文件包含密码、API 密钥或其他敏感数据时，它们 MUST（必须）受到保护，防止未经授权的访问。**

### 安全要求

| 场景 | 权限 | 所有者:组 | tmpfiles.d 类型 | 原因 |
|----------|-------------|-------------|-----------------|-----------|
| **应用写入配置** | `0660` | `appuser:appgroup` | `z` | 仅应用可读写，非全局可读 |
| **应用只读，管理员编辑** | `0640` | `root:appgroup` | `z`（或 install -m） | 应用可读，仅 root 可写，非全局可读 |
| **普通配置（无机密）** | `0644` | `root:root` | 默认安装 | 标准可读配置 |

### 示例：带有凭据的 Web 应用

```bash
# 在 PKGBUILD 中
backup=(
    'etc/webapps/myapp/config.php'      # 应用配置
    'etc/webapps/myapp/database.php'    # 数据库凭据 - 需要 0660
)

package() {
    # 正常安装配置（pacman 将以 root:root 0644 安装）
    install -Dm644 config.php "$pkgdir/etc/webapps/myapp/config.php"
    install -Dm644 database.php "$pkgdir/etc/webapps/myapp/database.php"
    
    # tmpfiles.d 将在安装时调整所有权和权限
    install -Dm644 myapp.tmpfiles "$pkgdir/usr/lib/tmpfiles.d/myapp.conf"
}

# 在 myapp.tmpfiles 中：
# 对于写入自己配置的应用（例如，Web 安装程序）：
z /etc/webapps/myapp/config.php      0660 root  http  -   -
z /etc/webapps/myapp/database.php    0660 root  http  -   -  # 包含数据库密码！

# 只读配置的替代方案（管理员手动编辑）：
z /etc/webapps/myapp/database.php    0640 root  http  -   -  # 仅 root 可写
```

### 为什么使用 systemd-tmpfiles 进行权限设置？

**DON'T（不要）在 PKGBUILD 中这样做：**
```bash
# ❌ 错误 - pacman 跟踪所有权/权限，导致冲突
install -Dm660 -o http -g http database.php "$pkgdir/etc/webapps/myapp/database.php"
```

**DO（请）使用 tmpfiles.d：**
```bash
# ✅ 正确 - pacman 以 root:root 安装，tmpfiles.d 在运行时调整
install -Dm644 database.php "$pkgdir/etc/webapps/myapp/database.php"
# 然后 tmpfiles.d 将所有权更改为 root:http 0660
```

**原因：**
1. 软件包文件应由 root 拥有
2. 运行时通过 tmpfiles.d 更改所有权（在安装/启动时运行）
3. 防止升级时的 pacman 冲突
4. 运行时修改文件的标准 Arch 模式

### 当应用写入配置时

**场景：** 带有基于 Web 的安装程序的 Web 应用（WordPress、Nextcloud 等）

**问题：** 安装程序需要写入数据库凭据到 `/etc/webapps/app/config.php`

**解决方案：**
1. 目录必须可由应用用户写入（通过 tmpfiles.d）
2. 配置文件必须可写且非全局可读（0660）
3. 使用 tmpfiles.d 类型 `z` 在现有文件上调整权限

**示例（.install 文件消息）：**
```bash
post_install() {
    echo "==> /etc/webapps/myapp/ 中的配置文件由 root:http 拥有（0660）"
    echo "==> 这允许 Web 安装程序写入数据库凭据"
    echo "==> 权限受限 - 配置文件 NOT（非）全局可读"
}
```

## 示例 PKGBUILD

```bash
pkgname=myapp
backup=(
    'etc/myapp/main.conf'      # 经常自定义
    'etc/myapp/db.conf'        # 包含凭据 - MUST（必须）保留
    'etc/myapp/logging.conf'   # 有时自定义
)
# /usr/share/ 中的 defaults.conf - NOT（不在）backup 中（只读模板）

package() {
    # /etc 中的配置文件
    install -Dm644 main.conf "$pkgdir/etc/myapp/main.conf"
    install -Dm644 db.conf "$pkgdir/etc/myapp/db.conf"
    install -Dm644 logging.conf "$pkgdir/etc/myapp/logging.conf"
    
    # /usr/share 中的模板（不在 backup 中）
    install -Dm644 defaults.conf "$pkgdir/usr/share/myapp/defaults.conf"
}
```

## 使用 .pacnew 文件的用户工作流程

升级后使用修改的配置：

```bash
# 查找 .pacnew 文件
pacdiff

# 或手动
find /etc -name "*.pacnew"

# 查看差异
diff /etc/myapp/main.conf /etc/myapp/main.conf.pacnew

# 手动合并更改
# 然后移除 .pacnew 文件
rm /etc/myapp/main.conf.pacnew
```

**资源：** https://man.archlinux.org/man/pacman.8.en（搜索 "HANDLING CONFIG FILES"）

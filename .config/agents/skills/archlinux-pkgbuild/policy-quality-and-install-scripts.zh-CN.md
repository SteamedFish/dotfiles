# Arch Linux PKGBUILD 规范、质量与安装脚本参考

面向质量门槛、红旗清单、配置文件处理与 .install 脚本模式的详细参考。

## 常见错误和红旗

### 关键错误（MUST（必须）修复）

| 错误 | 错误原因 | 正确方法 |
|---------|---------------|------------------|
| 使用直接 makepkg 进行分发构建 | 缺少/不正确的依赖、系统污染 | 使用 `extra-x86_64-build` 在 clean chroot 中构建 |
| 跳过 namcap | 违反 Arch 打包标准 | 始终在 PKGBUILD 和 .pkg.tar.zst 上运行 namcap（必需），以及 namcap -i（推荐） |
| 使用 /usr/local/ | 违反 FHS 合规性 | 仅使用 /usr/ 路径 |
| 供应商配置在 /etc/ 中 | 错误的关注点分离、冲突风险 | 使用 /usr/lib/sysusers.d/、/usr/lib/tmpfiles.d/、/usr/lib/udev/rules.d/ |
| 系统目录中的通用文件名 | 软件包冲突、所有权不明确 | 始终使用软件包名称：/usr/lib/sysusers.d/$pkgname.conf |
| 缺少直接依赖 | 运行时失败 | 在 clean chroot 中构建或使用 find-libdeps、ldd 查找所有直接依赖 |
| 包含传递性依赖 | 违反打包策略 | 仅列出直接依赖 |
| 对非 VCS 使用 'SKIP' | 安全风险 | 使用 updpkgsums 生成真实校验和 |
| 未引用的 $pkgdir/$srcdir | Shell 扩展错误 | 始终引用："$pkgdir" "$srcdir" |
| 自引用 pkgdesc | 冗余 | "Tool for X" 而不是 "pkgname is a tool for X" |
| GUI 应用缺少 .desktop 文件 | 应用不会出现在菜单中 | 安装到 /usr/share/applications/，使用 desktop-file-validate 验证（示例请参阅 archlinux-pkgbuild/cross-platform 子技能） |
| 源中的硬编码路径 | 版本升级需要编辑 | 使用变量：$pkgname-$pkgver |
|| 声称多架构支持但未测试 | 在未测试的架构上包失败 | 在所有架构上构建和测试，或在提交到 AUR 之前请求用户测试 |
|| 二进制包的架构列表错误 | 包安装失败或下载错误的二进制 | 匹配 `arch=()` 与所有上游二进制的可用性 |
|| 编译包使用 'any' | 包不是架构无关的 | 编译的二进制使用特定架构 |

### 警告信号（仔细检查）

| 模式 | 潜在问题 | 调查 |
|---------|----------------|-------------|
| 自定义 configure 标志 | 可能覆盖 Arch 默认值 | 检查 Arch 指南以获取标准标志 |
| 禁用剥离 | 调试符号膨胀 | 仅当上游需要时 |
| 空的 prepare() | 可能需要路径修复 | 检查构建输出中的 /usr/local |
| 没有 check() 函数 | 未测试的软件包 | 如果可用，运行上游测试 |
| 许多 optdepends | 它们都是可选的吗？ | 某些可能是核心功能必需的 |
| 版本固定的依赖 | 可能在更新时损坏 | 仅在真正需要时使用 >= |

## PKGBUILD 函数参考

| 函数 | 必需？ | 目的 | 常用命令 |
|----------|-----------|---------|-----------------|
| **prepare()** | 可选 | 打补丁源、修复路径 | sed、patch、find |
| **pkgver()** | 仅 VCS | 自动更新版本 | git rev-list、svnversion |
| **build()** | 通常 | 编译源 | ./configure、make、cmake |
| **check()** | 可选 | 运行测试套件 | make test、make check |
| **package()** | **强制** | 安装到 $pkgdir | make install、install |

## 安装命令参考

```bash
# 使用正确的权限安装文件
install -Dm644 file.txt "$pkgdir/usr/share/doc/$pkgname/file.txt"  # 普通文件
install -Dm755 binary "$pkgdir/usr/bin/binary"                      # 可执行文件
install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"  # License

# 创建目录
install -dm755 "$pkgdir/usr/share/$pkgname"

# 复制整个目录
cp -r dir "$pkgdir/usr/share/$pkgname/"

# 删除不需要的文件
rm -rf "$pkgdir/usr/share/doc"  # 如果上游错误地安装了文档
```

## 配置文件处理

**用户可修改的配置文件 MUST（必须）在 backup=() 中列出，以防止 pacman 覆盖用户更改。**

### 快速规则

```bash
backup=(
    'etc/myapp/main.conf'     # 相对于根目录，NO（无）前导斜杠
)
```

**包含在 backup=() 中：** 所有用户可能自定义的 /etc 文件，特别是凭据文件  
**排除：** /usr/share 中的模板、生成的文件、服务文件

**Pacman 行为：** 修改的文件 → 创建 .pacnew（用户手动合并）

### 安全：敏感配置文件

**如果配置文件包含凭据/机密（数据库密码、API 密钥）：**
- 使用 `0660` 权限（不是 world-readable）
- 通过 tmpfiles.d 类型 `z` 设置所有权
- 示例：`z /etc/webapps/app/database.php 0660 root http - -`

**如果应用写入配置（Web 安装程序）：**
- 文件由 root 拥有，组设置为应用用户（例如 root:http）
- 对 group-writable 使用 `0660`，不是 world-readable（安全）
- 目录可以保持 root:root（只有文件需要组访问）

**详细参考：** 有关以下内容的详细信息，请参阅 config-file-handling.zh-CN.md：
- backup=() 规则和 .pacnew 工作流程
- 敏感配置文件权限（0660 vs 0640 vs 0644）
- 使用 tmpfiles.d 类型 `z` 进行所有权/权限更改
- Web 应用的安全模式

## 安装后脚本（.install 文件）

**使用 .install 文件提供安装后说明、执行设置任务或通知用户手动配置要求。**
**核心原则：Arch Linux 用户经验丰富。保持输出最简。永远不要说废话。**

### 何时使用 .install 文件

| 使用场景 | 示例 |
|----------|---------|
| 无上游文档的复杂设置 | 数据库创建、导入架构 |
| 无法自动化的手动步骤 | 生成密钥、编辑凭据 |
| 重要警告 | 安全通知、重大更改 |
| 上游有文档 → 直接链接 | "参见 https://upstream/install" |

**不要在 .install 中：**
- 告诉用户 `systemctl start/enable`（包含了 .service 文件时用户自然知道）
- 重复 pacman 自动执行的步骤（tmpfiles、sysusers、udev 重载）
- 复述 man page 或上游文档中已有的内容

### .install 文件结构

```bash
# /path/to/pkgname.install
post_install() {
    cat <<EOF

==> 软件包名称
==> ============

1. 第一个设置步骤：
   - 详细说明
   - 要运行的命令

2. 第二个设置步骤：
   - 更多说明

更多信息：https://upstream.docs

EOF
}

post_upgrade() {
    # 重要：仅包含与升级相关的信息
    # DO NOT（不要）盲目调用 post_install()，除非 ALL（所有）说明都适用于升级
    
    # 如果升级需要特定说明（迁移、重大更改）：
    cat <<EOF

==> 升级说明
==> =============

- 审查配置文件（检查 .pacnew 文件）
- 数据库迁移步骤（如适用）
- 此版本中的重大更改
- 服务重启命令

EOF
    
    # 反模式：不要这样做，除非初始设置真正适用于升级
    # post_install  # ❌ 错误，如果它包含一次性设置说明
}

pre_remove() {
    # 可选：移除前清理
    # 谨慎使用 - 大多数清理应该是手动的
}

post_remove() {
    # 可选：通知剩余数据
    cat <<EOF

==> 移除完成
==> =================

用户数据保留在 /var/lib/pkgname/
如果需要，手动移除：sudo rm -rf /var/lib/pkgname

EOF
}
```

### PKGBUILD 集成

**在 PKGBUILD 中引用 .install 文件：**

```bash
# 在 PKGBUILD 中
pkgname=myapp
install="$pkgname.install"  # 在 optdepends=() 之后添加，在 source=() 之前
```

**.install 文件必须：**
- 命名为 `$pkgname.install`
- 与 PKGBUILD 在同一目录中
- NOT（不要）在 source=() 中列出（它是元数据，不是源文件）
- 使用正确的 shell 语法（函数，无语法错误）

### 最佳实践

| 应该 | 不应该 |
|----|-------|
| 总输出控制在 10 行以内 | 写入论文长度的说明 |
| 有上游文档时直接链接 | 将上游文档内嵌复制 |
| 使用 heredoc（cat <<EOF）进行格式化 | 使用多个 echo 语句 |
| 区分一次性与升级说明 | 盲目地从 post_upgrade 调用 post_install |
| 测试 .install 脚本语法错误 | 假设它会工作而不进行测试 |
| 只输出用户必须操作的内容 | 说出 Arch 用户早就知道的废话 |

### 最简原则（重要）

**1. 不要运行 pacman 自动触发的命令**
Pacman 会自动运行 `systemd-tmpfiles`、`sysusers` 和 `udev` 触发器，不要在 .install 中调用：
```bash
# ❌ 错误 — pacman 已经自动执行
post_install() {
    systemd-tmpfiles --create
    udevadm trigger
}
```

**2. 不要输出显而易见的 systemd 说明**
当包提供了 `.service` 文件时，Arch 用户自然知道如何启动它。不要打印：
```bash
# ❌ 错误 — 侮辱用户智商
echo "启动服务：systemctl start myapp"
echo "开机启动：systemctl enable myapp"
```

**3. 链接上游文档，不要复制**
如果上游有详细的设置指南，一行即可：
```bash
# ✅ 正确
cat <<EOF
==> myapp：需要初始设置 — 参见 https://upstream.example/docs/setup
EOF
```

**4. 最小输出 — 只输出用户需要操作的内容**
每一行打印都是干扰，除非用户需要执行操作。目标：总计 < 10 行。

### 重要：post_install 与 post_upgrade 的区别

**post_install()**：一次性初始设置说明
- 数据库创建和架构导入
- 初始配置文件设置
- 从头开始的 Web 服务器配置
- 用户/组创建
- 首次设置向导
- 安装验证步骤

**post_upgrade()**：仅与升级相关的信息
- 配置文件更改（.pacnew 审查）
- 数据库迁移/架构更新
- 版本之间的重大更改
- 服务重启要求
- 弃用功能警告
- 变更日志引用

**决策规则：**
```
如果说明仅在首次安装时需要 → 仅 post_install()
如果说明仅在升级时需要 → 仅 post_upgrade()  
如果说明适用于 BOTH（两者）→ 考虑从 post_upgrade() 调用 post_install()

⚠️  反模式：始终从 post_upgrade() 调用 post_install()
仅当 ALL（所有）post_install 说明真正适用于升级时才这样做。
```

**示例：**

❌ **错误** - 升级显示不必要的一次性设置：
```bash
post_install() {
    cat <<EOF
1. 创建数据库并导入架构  # ❌ 只需要一次
2. 配置 Web 服务器               # ❌ 只需要一次
3. 运行安装向导            # ❌ 只需要一次
EOF
}
post_upgrade() {
    post_install  # ❌ 用不相关的步骤混淆用户
}
```

✅ **正确** — 最简、链接优先：
```bash
post_install() {
    cat <<EOF
==> myapp：需要初始设置
1. 创建数据库：  sudo -u postgres createdb myapp
2. 导入架构：    psql myapp < /usr/share/myapp/schema.sql
3. 编辑配置：    /etc/webapps/myapp/config.php
4. 完成设置：    http://localhost/myapp/install
文档：https://github.com/project/wiki/Setup
EOF
}

post_upgrade() {
    # 如无需用户操作可省略
    cat <<EOF
==> myapp 升级说明
- 如有，请检查 .pacnew 文件
- 变更日志：https://github.com/project/releases
EOF
}

✅ **可接受** — 当说明真正适用于两者时：
```bash
post_install() {
    cat <<EOF
==> myapp：请检查配置 /etc/myapp/config.yaml
EOF
}

post_upgrade() {
    post_install  # ✅ 可以 — 这些步骤也适用于升级
}

### 示例：Web 应用（最简版）

```bash
# webapp.install
post_install() {
    cat <<EOF
==> webapp：需要初始设置
1. 创建数据库：sudo -u postgres createdb webappdb
2. 编辑配置：  /usr/share/webapps/webapp/config.php
3. 链接虚拟主机：sudo ln -s /etc/httpd/conf/extra/webapp.conf /etc/httpd/conf/extra/
4. 完成安装：  http://localhost/webapp/install
文档：https://webapp.example/docs/setup
EOF
}

post_upgrade() {
    # 仅在有重大更改或必要迁移时添加
    cat <<EOF
==> webapp：请检查 .pacnew 文件
变更日志：https://webapp.example/releases
EOF
}

### 验证

```bash
# 检查 .install 语法
bash -n pkgname.install

# 验证它在 PKGBUILD 中被引用
grep "^install=" PKGBUILD

# 验证它包含在软件包中
namcap *.pkg.tar.zst | grep -i install
```

**注意：** 当设置了 `install=` 时，.install 文件会自动包含在软件包中。Do NOT（不要）在 package() 中手动安装它。

## 专门软件包类型

**对于专门的软件包类型，加载适当的子技能：**

- **VCS 软件包**（-git、-svn、-cvs）：使用 **archlinux-pkgbuild/vcs-packages**
- **Systemd 服务**（DynamicUser、tmpfiles.d、沙盒化、init 转换）：使用 **archlinux-pkgbuild/systemd-services**
- **编译型语言**（Go、Rust、Haskell、OCaml、Free Pascal、Java）：使用 **archlinux-pkgbuild/compiled-languages**
- **解释型语言**（Node.js、Python、Ruby、PHP、Perl、R、Shell、Lisp）：使用 **archlinux-pkgbuild/interpreted-languages**
- **构建系统**（CMake、Meson）：使用 **archlinux-pkgbuild/build-systems**
- **跨平台**（Wine、MinGW、Electron、CLR）：使用 **archlinux-pkgbuild/cross-platform**
- **桌面集成**（GNOME、KDE、Eclipse、Fonts）：使用 **archlinux-pkgbuild/desktop-integration**
- **系统软件包**（DKMS、内核模块、lib32、非自由软件、Web 应用、分割）：使用 **archlinux-pkgbuild/system-packages**

## 高级 Clean Chroot 技术

**有关高级 clean chroot 用法**（tmpfs 构建、自定义依赖、主要重建、不同仓库），请参阅本目录中的 **clean-chroot-reference.zh-CN.md**。

**快速故障排除：**

| 问题 | 解决方案 |
|---------|----------|
| "chroot is locked" | 移除 `/var/lib/archbuild/extra-x86_64.lock` |
| Chroot 更新失败 | 使用 `-c` 标志重置：`extra-x86_64-build -c` |
| 权限被拒绝 | 使用 `sudo` 运行或确保用户在 `wheel` 组中 |

## 资源

- Arch Package Guidelines：https://wiki.archlinux.org/title/Arch_package_guidelines
- PKGBUILD(5) 手册页：https://man.archlinux.org/man/PKGBUILD.5
- FHS 规范：https://man.archlinux.org/man/file-hierarchy.7
- AUR 提交：https://wiki.archlinux.org/title/AUR_submission_guidelines
- namcap：https://wiki.archlinux.org/title/Namcap
- Clean chroot 构建：https://wiki.archlinux.org/title/DeveloperWiki:Building_in_a_clean_chroot
- devtools：https://man.archlinux.org/man/extra/devtools/pkgctl-build.1
- nvchecker/pkgctl-version：https://man.archlinux.org/man/extra/devtools/pkgctl-version.1

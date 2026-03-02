# Arch Linux PKGBUILD AUR 发布与版本跟踪

面向 AUR 提交流程、仓库清单规则、nvchecker/pkgctl 版本跟踪的详细参考。

### 步骤 8：AUR 提交（如适用）

**生成 .SRCINFO：**
```bash
makepkg --printsrcinfo > .SRCINFO
```

**设置 AUR SSH：**
```bash
# 添加到 ~/.ssh/config
Host aur.archlinux.org
    IdentityFile ~/.ssh/aur
    User aur
```

**设置 .gitignore：**
```bash
# 为 PKGBUILD 仓库创建 .gitignore
cat > .gitignore << 'EOF'
# 构建产物
*.pkg.tar.zst
*.pkg.tar.xz
*.pkg.tar.gz

# 源码压缩包
*.tar.gz
*.tar.bz2
*.tar.xz
*.zip

# 构建目录
pkg/
src/

# makepkg 元数据（仅构建时）
.BUILDINFO
.PKGINFO
.MTREE
.INSTALL

# 临时文件
*.log
*~
*.swp
EOF
```

**重要的 .gitignore 规则：**

| 文件 | 跟踪？ | 原因 |
|------|--------|-----|
| **PKGBUILD** | ✅ 是 | 强制源文件 |
| **.SRCINFO** | ✅ 是 | AUR 所需（机器可读元数据） |
| **\*.install** | ✅ 是 | 安装后脚本源 |
| **\*.patch** | ✅ 是 | 源码补丁 |
| **补充配置** | ✅ 是 | .tmpfiles、.service、示例配置 |
| **.INSTALL** | ❌ 否 | 构建产物（从 .install 生成） |
| **pkg/, src/** | ❌ 否 | 构建目录 |
| **\*.pkg.tar.*** | ❌ 否 | 已构建的软件包 |
| **.BUILDINFO, .PKGINFO, .MTREE** | ❌ 否 | makepkg 元数据 |

**AUR 提交的两种场景：**

**场景 A：已跟踪 PKGBUILD 文件的现有本地 git 仓库**

如果您已经有包含 PKGBUILD、.SRCINFO 和相关文件的 git 仓库：

```bash
# 将 AUR 添加为现有仓库的远程
cd /path/to/existing/pkgname
git remote add aur ssh://aur@aur.archlinux.org/pkgname.git

# 推送到 AUR
git push aur master
```

**优点：**
- 保留现有的 git 历史
- 单个仓库用于本地开发和 AUR
- 目录名称与软件包名称匹配
- 无需文件复制

**场景 B：从头开始（新软件包）**

如果您还没有 git 仓库：

```bash
# 克隆（新软件包为空）
git clone ssh://aur@aur.archlinux.org/pkgname.git
cd pkgname

# 添加文件
cp /path/to/PKGBUILD .
makepkg --printsrcinfo > .SRCINFO

# 创建 .gitignore（见上文）
cat > .gitignore << 'EOF'
# 构建产物
*.pkg.tar.zst
*.pkg.tar.xz
*.pkg.tar.gz

# 源码压缩包
*.tar.gz
*.tar.bz2
*.tar.xz
*.zip

# 构建目录
pkg/
src/

# makepkg 元数据（仅构建时）
.BUILDINFO
.PKGINFO
.MTREE
.INSTALL

# 临时文件
*.log
*~
*.swp
EOF

# 提交
git add PKGBUILD .SRCINFO .gitignore
git commit -m "Initial commit: pkgname $pkgver-$pkgrel"

# 推送
git push origin master
```

**决策规则：** 如果您有包含所需文件（PKGBUILD、.SRCINFO、.gitignore、补丁、配置）的现有 git 仓库，请使用 **场景 A**。仅在完全从头开始时才使用 **场景 B**。

## 使用 nvchecker 跟踪上游版本

**nvchecker** 自动检查新的上游版本。使用 **pkgctl version** 命令进行集成。

### 快速入门

```bash
# 从 PKGBUILD source 数组自动生成 .nvchecker.toml
pkgctl version setup

# 检查新的上游版本
pkgctl version check

# 更新 PKGBUILD 到新版本
pkgctl version upgrade
```

### .nvchecker.toml 配置

将 `.nvchecker.toml` 放在与 PKGBUILD 相同的目录中。`pkgctl version setup` 命令通过分析您的 `source=()` 数组自动创建此文件。

**GitHub releases 示例：**
```toml
[pkgname]
source = "github"
github = "owner/repo"
```

**PyPI 示例：**
```toml
[python-package]
source = "pypi"
pypi = "package-name"
```

**GitLab 示例：**
```toml
[pkgname]
source = "gitlab"
gitlab = "group/project"
```

**另请参阅：**
- pkgctl-version(1) 手册页获取完整文档
- 官方软件包中的示例配置（例如 https://gitlab.archlinux.org/archlinux/packaging/packages/pacman/-/blob/main/.nvchecker.toml）
- nvchecker 文档：https://nvchecker.readthedocs.io/

### 工作流程

1. 初始设置：`pkgctl version setup`（创建 .nvchecker.toml）
2. 定期检查：`pkgctl version check`
3. 发现新版本时：`pkgctl version upgrade`（更新 pkgver）
4. 审查更改，更新 pkgrel=1，重新构建，测试
5. 提交到 AUR

**注意：** nvchecker 对于维护多个软件包或跟踪快速变化的项目特别有用。

